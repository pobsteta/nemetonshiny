# Changelog

## nemetonshiny 2026.10.1 (2026-10-09)

Première release au **versionnage calendaire `AAAA.M.N`**, comme le cœur
`nemeton` : la version dit l’année et le mois de la release, `N` son
rang dans le mois. Le cycle de dev reste en `.900x`, et
`version-consistency` vérifie le format et sa concordance avec la date
de cette entrée. Elle succède à la 2.0.1.

Changements cassants : aucun.

### Nuage de points drone

- **Nouveau sous-onglet Terrain › Import › « Nuage de points drone »**
  (spec 059 du cœur, `nemeton (>= 2.1.0)`).
  - On dépose le nuage d’un vol (`.las`, `.laz`, `.copc.laz`) : il est
    rangé dans `cache/layers/drone_nuage/` du projet.
  - On choisit LiDAR drone ou photogrammétrie, puis
    [`nemeton::traiter_nuage_points()`](https://pobsteta.github.io/nemeton/reference/traiter_nuage_points.html)
    produit le MNT, le MNS et le MNH, en tâche de fond (`ExtendedTask`).
  - En photogrammétrie, le MNT vient du LiDAR HD du projet (ou de la BD
    ALTI), et le MNH LiDAR HD sert à mesurer le décalage vertical sur
    sol nu. Les mosaïques LiDAR vides sont écartées.
  - Le MNH, le MNS et le MNT s’affichent sur une carte, avec les
    contrôles de qualité : points, densité, parts de sol et de bruit,
    décalage vertical retiré et son écart interquartile, et les
    avertissements du cœur. Le bilan est relu à la réouverture du
    projet.
  - Aucun code de plus pour la suite : `resolve_project_dem()` et
    `resolve_project_chm()` prennent les produits drone en premier, et
    le projet passe en NDP 2.
- Limite d’envoi de fichiers portée à 20 Go (`shiny.maxRequestSize`),
  pour les nuages de points.
- Plancher cœur `nemeton (>= 2.1.0)`.

### Marculus

- **Marculus : CSV au format 4 et lots d’affouage** (brief Marculus du
  2026-10-07, Marculus v0.52.0 à v0.55.1).
  - Chaque tige garde le `lot` attribué au martelage (colonne `Lot` du
    CSV, clé `lot` du `.marsync`). Il n’est jamais recalculé.
  - Les contextes portent `affouage`, `volumeMaxLotM3`, `Journal`, et,
    dans le CSV, `Tarif`, `TarifNumero` et `CoefficientForme`. Ces
    réglages sont gardés sur l’action (`reglages_terrain`) pour
    l’affichage, mais **jamais réémis** vers le téléphone : une clé
    absente laisse le terrain décider.
  - La synthèse du martelage montre, pour un contexte d’affouage, le
    bilan par lot : tiges, volume net, et état complet ou incomplet. Il
    suit la règle d’annulation des volumes : le lot de la tige retirée
    perd son volume. Un lot dont le volume atteint exactement le maximum
    est clos.
  - Un CSV `Journal;NET` (tiges à comptabiliser) est signalé comme un
    état de comptage. Il ne remplace jamais une tige lue dans un journal
    complet ou un `.marsync`.
  - Le CSV du bilan par lot (`Lot;Tiges;Volume_m3;Etat`) est reconnu et
    refusé, avec un message qui dit quoi importer.
  - Les textes d’aide parlent du « format 2 ou supérieur ».

### Caches et LiDAR

- **Caches contrôlés sur l’emprise** (brief LiDAR HD du 2026-10-07, §
  5). Ces caches étaient réutilisés sur la seule existence du fichier.
  Désormais :
  - `irc.tif` est contrôlé sur l’emprise, comme `ndvi.tif` ;
  - `ndvi_s2_v2.tif` et `spectral/<scène>/` portent une clé de géométrie
    (`<fichier>.cle`) et sont recalculés si les unités changent ;
  - un CHM Open-Canopy qui ne couvre plus les parcelles est mis de côté
    (`.perime`), puis la prédiction est relancée.
- **Repli lasR plafonné en mémoire.** Il dépassait le plafond de 12 Go
  avec 4 dalles. Il prend désormais au plus 4 workers, et un par 3 Go de
  la moitié de la RAM disponible. Réglable par
  `options(nemetonshiny.lasr_ncores =)` ou `NEMETON_LASR_NCORES`.
- Le reste du brief (la couche `IGNF_LIDAR-HD_METADONNEE:metadata`)
  était déjà livré par `0dbfaaeb`, gardé tel quel.

## nemetonshiny 2.0.1 (2026-10-09)

- **Dalles LiDAR HD vides refusées.** L’IGN publie parfois le nuage de
  points avant les rasters dérivés. Le WMS sert alors des dalles MNH/MNT
  valides au sens GeoTIFF, mais 100 % NoData, au lieu d’un 404. L’app
  les acceptait. Le calcul se terminait sans erreur, mais les familles
  Carbone, Risques, Production et Énergie restaient vides, et le MNT
  vide remplaçait la BD ALTI (Couchey, `20261008_212542_uywc`).
  Désormais :
  - une dalle sans pixel valide (moins de 1 %) est un échec, comme un
    404 : elle est supprimée du cache et le journal dit « vide (produit
    non encore publié par l’IGN) ». Les dalles et mosaïques vides déjà
    en cache sont purgées au recalcul suivant ;
  - une mosaïque qui couvre moins de 90 % de l’emprise n’est pas
    retenue. La chaîne passe alors à la source suivante : CHM dérivé du
    nuage COPC par lasR, puis Theia, puis Open-Canopy. Pour le terrain,
    elle prend le MNT lasR, sinon garde la BD ALTI ;
  - garde-fou final, quelle que soit la source : un CHM ou un MNT sans
    pixel valide sur l’emprise du projet n’est jamais retenu ;
  - l’accessibilité, la desserte et la reGénération (R3) ne prennent
    plus une mosaïque MNT LiDAR vide.
- **Cause des cases vides.** Un indicateur vide faute de CHM ou de MNT
  porte le statut `sans_chm` ou `sans_mnt`, et la vue famille explique
  pourquoi la case est vide.
- Brief cœur 2.0.0 : `create_qfield_project()` est remplacée par
  `create_qgis_project()` (plan d’échantillonnage). Le mock obsolète de
  `theia_configure_s3()` est retiré des tests SUFOSAT.

## nemetonshiny 2.0.0 (2026-10-08)

Jalon de la **spec 058** : la plateforme bâtit désormais les UGF d’une
forêt publique à partir du parcellaire ONF calé sur le cadastre. Elle
n’a plus d’autre façon de croiser l’ONF. Le code est celui de la 1.3.1.
Le numéro majeur marque la rupture livrée en 1.3.0 et 1.3.1 :

- **Ancien croisement ONF retiré, sans retour possible.** Ont disparu le
  calage « parcelle entière au-delà de 90 % », la purge sur la part
  forestière et le réglage `seuil_foret`. Un projet qui porte
  `seuil_foret` dans son `metadata.json` se relit sans erreur, mais ce
  réglage n’a plus d’effet. Les nouveaux réglages sont la couverture
  minimale, la tolérance d’accrochage, les largeurs et surfaces
  minimales, et le seuil de rattachement.
- **Nouveau modèle d’UGF** : chaque UGF peut porter sa parcelle
  forestière ONF dans les colonnes `onf_*`. Elles sont écrites dans
  `ugs.json` et dans l’export GeoPackage. Les fichiers antérieurs se
  lisent sans migration.
- **Deux chemins vers les UGF ONF** : croiser un projet existant
  (1.3.0), ou créer un projet depuis la forêt publique d’une commune
  (1.3.1).
- **API MCP** : nouveaux outils `appliquer_ugf` et `croiser_onf`.
- Plancher cœur `nemeton (>= 1.2.0)`.

## nemetonshiny 1.3.1 (2026-10-08)

- **Nouveau projet depuis la forêt ONF** (spec 058, chemin A) : un
  bouton du bloc ONF de la carte UGF ouvre une modale (département,
  commune). L’app retient les parcelles cadastrales de la commune qui
  touchent le parcellaire ONF, appartiennent à une personne publique
  (DGFiP) et sont couvertes au seuil des paramètres. Elle crée le projet
  avec ses UGF numérotées d’après les parcelles forestières, puis
  l’ouvre. Le projet courant est conservé, à la différence de l’import
  CSV. Le travail tourne en tâche asynchrone. Les parcelles non retenues
  sont listées de la plus couverte à la moins couverte. Mesuré sur
  Sombernon : « Forêt communale de Sombernon », 20 parcelles (204,7 ha),
  58 UGF, écart médian de calage 3,2 m, 48 à 70 s dans l’app.
- **Parcelles écartées** : une parcelle privée affiche aussi sa
  couverture ONF dès 1 %. À Sombernon, ZA 0029 est privée mais couverte
  à 99 %.
- Surfaces et écarts de calage des notifications ONF en notation
  française (virgule décimale).

## nemetonshiny 1.3.0 (2026-10-08)

- **Croisement ONF, chemin unique** (spec 058, briefs `ugf-depuis-onf`,
  `onf-nouveau-chemin-seul`, `onf-chemin-unique-api-coeur`) : le bouton
  « Croiser avec l’ONF » et l’import CSV ne passent plus que par
  [`nemeton::construire_ugf_onf()`](https://pobsteta.github.io/nemeton/reference/construire_ugf_onf.html).
  Le parcellaire ONF est recalé sur le cadastre (calage élastique), qui
  n’est jamais déformé. Un seul appel couvre tout le projet, communes
  voisines comprises. L’ancienne chaîne (calage « parcelle entière
  au-delà de 90 % », purge sur la part forestière, `seuil_foret`) est
  retirée. Le croisement tourne en tâche asynchrone, car le premier
  appel télécharge le fichier DGFiP (376 Mo).
- **Parcelles hors régime forestier** : avec la purge cochée, les
  parcelles privées, trop peu couvertes par l’ONF ou hors parcellaire
  ONF quittent le projet. La notification les liste avec leur raison et
  leur propriétaire DGFiP. L’import CSV garde toujours toutes ses
  parcelles.
- **Paramètres ONF** : couverture minimale, et en réglages avancés
  repliés la tolérance d’accrochage, la largeur et la surface minimales
  d’une UGF hors ONF et le seuil de rattachement, avec un bouton «
  Valeurs par défaut ». Les bornes sont contrôlées. Un ancien
  `metadata.json` qui contient `seuil_foret` se relit sans erreur.
- **N° ONF dans les UGF** : colonnes `onf_foret_id`, `onf_foret_nom`,
  `onf_parcelle`, `onf_domaniale` et `onf_part`. Elles sont conservées
  par l’import, la fusion (si même parcelle forestière) et la division,
  et affichées dans le tableau, la popup et l’export GeoPackage. Un
  `ugs.json` antérieur se lit sans erreur.
- **Outils MCP** `appliquer_ugf` et `croiser_onf` : refus si un calcul
  tourne ou si le projet est verrouillé. `appliquer_ugf` refuse aussi un
  IDU inconnu et un pavage inexact, sans modifier le projet. Le retour
  indique enfin que les indicateurs sont périmés : `save_ug_data()` les
  avait déjà mis de côté, et le second contrôle répondait « non ».
- **reGénération** : l’axe ombre du classement des essences s’active
  avec le LAI réellement utilisé par UGF (spec 039). Sans clé CDS, le
  `lai_max` de BILJOU est tiré du nuage LiDAR et mis en cache (brief
  lai-lidar).
- **R5** : un test verrouille le sens de l’inversion du score.
- Plancher `Imports: nemeton (>= 1.2.0)`.

## nemetonshiny 1.2.2 (2026-10-08)

- **reGénération, onglets en tête** : « Carte + Tableau » et « Contexte
  régional (E-OBS) » remontent en haut du panneau, soulignés et avec
  icône, comme dans le Plan d’actions. Le bandeau d’état (projet requis,
  modèle NDP, avertissements) passe sous les onglets au lieu de les
  repousser.

## nemetonshiny 1.2.1 (2026-10-08)

Reliquat de l’audit des briefs cœur → app du 2026-10-08, et pictogramme
feuillu de RECONFORT.

- **Carte bivariée E-OBS, cache périmé** (brief 034 bivariate-cache,
  bug A) : un `context_bivariate.tif` écrit sous un autre schéma (3 × 3
  hérité, ou `palette$ncol` absent) n’est plus servi ; il est recalculé
  en 5 × 5. La vue T°max et la vue précipitations ne sont pas
  concernées.
- **UGF nommées dans le bandeau de cause** (brief trois-derniers-points,
  point 1.3) : quand un indicateur n’est que partiellement vide et que
  le cœur en donne la cause (A5 `skipped_no_reference`, repli R1…), le
  bandeau de la vue famille cite les UGF concernées (cinq au plus, puis
  « (+n) »).
- **Libellés d’indicateurs lus dans le cœur seulement** (brief
  indicator-families, étape 4) : les 40 clés `indicator_<CODE>` retirées
  ; `clean_indicator_label()` résout aussi les codes courts (`C1`, `R5`)
  par
  [`nemeton::indicator_labels()`](https://pobsteta.github.io/nemeton/reference/indicator_labels.html).
- **R5** (brief 008 R5-brief-shiny-radar) : test « pas de double
  inversion » (R5 brut transmis au cœur, qui l’inverse) ; clés
  `r5_label` / `r5_tooltip` inutilisées et périmées (« FORDEAD seul »)
  retirées.
- **Nettoyage** : 15 clés `foret_ancienne_*` orphelines depuis la source
  nationale automatique (spec 031) ; commentaire périmé sur la colonne
  `P1` (brief 057 §4 bis) ; guide de l’application à jour de l’onglet
  **Atlas** (sous-onglets Sélection / Synthèse) et des sous-onglets du
  Suivi sanitaire.
- **Suivi sanitaire, pictogramme RECONFORT** : l’onglet RECONFORT et son
  sous-onglet de carte portaient un résineux (`tree-fill`), comme
  FORDEAD. RECONFORT suit les feuillus (chêne, châtaignier) : il porte
  désormais un arbre à houppier rond, dessiné en SVG au format des
  icônes Bootstrap (qui n’ont pas de feuillu).

## nemetonshiny 1.2.0 (2026-10-08)

Restes des briefs cœur → app (audit du 2026-10-07).

- **Badge canopée** (brief 033) : la provenance passe par
  [`nemeton::canopy_provenance()`](https://pobsteta.github.io/nemeton/reference/canopy_provenance.html)
  ; troisième état « CHM ML (Open-Canopy) ». Les clés internes
  deviennent celles du cœur (`lidar_hd`, `prosail_s2`, `opencanopy`).
- **Desserte, pistes OSM hors BD TOPO** (brief desserte-visualisation) :
  le calque montre enfin le gisement, `osm_hors_corridor` rendu par
  [`foretaccess::comparer_desserte_osm()`](https://pobsteta.github.io/foretaccess/reference/comparer_desserte_osm.html),
  et non plus l’acquisition brute.
- **Étapes E-OBS** (brief 034 §2.1) : requête CDS, décompression,
  lecture, réduction s’affichent sous le bouton « Auto (E-OBS) ».
- **Forçage BILJOU** (brief 027 biljou) : la notification du moteur
  nomme l’unité SAFRAN ou l’unité × année ERA5 en cours.
- **Licence E-OBS** (brief 027 brancheA §5) : attribution ECA&D /
  Copernicus et licence non commerciale sous la carte de contexte.
- **A5** (brief 032 a5-diagnostic) : cause nommée
  `skipped_no_reference`.
- **Carte reGénération, deux couches** : « ΔT°max × ΔVPD » (brief 027
  onglet §4.3), carte bivariée 3 × 3 en terciles des UGF du projet, et «
  Meilleure essence » (spec 039 §7), l’essence classée première par
  [`nemeton::regen_rank_species()`](https://pobsteta.github.io/nemeton/reference/regen_rank_species.html).
- **Typage de desserte, voie IFN** (brief 040) : choix « taux saisi /
  référentiel IFN (par essence) ». L’essence vient de la BD Forêt du
  projet, la SER de `ensure_ugf_ser()` ; en NDP 0, P1 vide est comblé
  par
  [`nemeton::completer_volume_ifn()`](https://pobsteta.github.io/nemeton/reference/completer_volume_ifn.html).
  Le résultat affiche l’échelon du taux (SER, GRECO ou national), la
  part du volume issue de la référence IFN et le nombre d’essences non
  reconnues. Mesuré sur Couchey : 13 parcelles sur 23 comblées, taux
  régional SER C20, moins d’une seconde.
- **Familles d’indicateurs** (brief indicator-families, points 2, 3
  et 5) : le menu des 12 familles est construit par boucle sur
  [`nemeton::indicator_families()`](https://pobsteta.github.io/nemeton/reference/indicator_families.html)
  ; `FAMILLE_NMT_MAP` (tiré par `getFromNamespace`) disparaît au profit
  de `get_famille_code()` ; l’ordre des familles de l’export PDF et des
  objectifs du plan d’actions vient du cœur ; code mort retiré dans
  `mod_synthesis.R`. Les clés i18n `famille_*` restent un repli, écrasé
  à l’exécution par le cœur et vérifié par un test.

## nemetonshiny 1.1.0 (2026-10-07)

- **Onglet Atlas à sous-onglets** : « Sélection » et « Synthèse »
  deviennent deux sous-onglets de l’onglet principal « Atlas », comme
  les quatre sous-onglets de « Terrain accessible ». La barre de
  navigation perd un onglet. Le bouton « Voir les résultats », les liens
  profonds (`?tab=synthesis`, et maintenant `?tab=atlas`), le tour guidé
  et la carte UGF suivent ; les messages qui renvoient à ces écrans
  disent « Atlas › Sélection » / « Atlas › Synthèse ». Nouveau
  `R/service_navigation.R` (onglet logique ↔︎ onglet parent).

- **Suivi sanitaire à sous-onglets** : les trois modes FAST, FORDEAD et
  RECONFORT deviennent des sous-onglets de « Suivi sanitaire » ; le
  bouton radio « Mode de suivi » de la barre latérale est retiré. Le
  navset reprend l’identifiant `mode` et ses valeurs (`quick` / `health`
  / `reconfort`) : paramètres de la barre latérale et sous-onglets
  internes (alertes, carte, plan de validation) suivent le mode
  exactement comme avant. Clés i18n `monitoring_onglet_*` ajoutées,
  `monitoring_mode_label` / `monitoring_mode_{quick,health,reconfort}`
  retirées.

- **LiDAR HD** : les dalles sont désormais listées par la couche WFS
  `IGNF_LIDAR-HD_METADONNEE:metadata` de la Géoplateforme (colonnes
  `url_mnh`, `url_mnt`, `url_mns`, `url_npl`). Les couches par produit
  (`IGNF_MNH-LIDAR-HD:dalle`, `IGNF_MNT-LIDAR-HD:dalle`,
  `IGNF_NUAGES-DE-POINTS-LIDAR-HD:dalle`) ont été retirées par l’IGN et
  répondent 404 : aucun projet ne trouvait plus de dalle, et le calcul
  basculait sur la reconstruction lasR depuis les nuages de points, qui
  dépasse le plafond mémoire de 12 Go. Les couches par produit restent
  essayées en repli. Les noms de fichiers sont inchangés : les dalles
  déjà en cache sont réutilisées.

## nemetonshiny 1.0.1 (2026-10-07)

- L’onglet « Sélection » s’appelle désormais **« Atlas »** (FR et EN).
  Les messages qui y renvoient (retrait des parcelles hors forêt ONF,
  plan d’échantillonnage, mode local SQLite) suivent. La clé technique
  `tab_selection` est inchangée.

## nemetonshiny 1.0.0 (2026-10-06)

Première version stable. Elle repart de zéro : les projets et les bases
créés avec une version 0.x ne sont pas repris.

Adoption du cœur **nemeton 1.0.0** (brief
`nemeton/specs/057-contrat-api-1.0/brief-nemetonshiny-1.0.0.md`).
Plancher `Imports: nemeton (>= 1.0.0)`.

#### Rupture : la 1.0.0 repart de zéro

- **Aucune migration des projets 0.x** (décision du 2026-10-06). Chaque
  projet porte désormais un marqueur `format_projet = 1`. Un projet sans
  marqueur apparaît « Antérieur à la 1.0 » dans la liste et ne propose
  que sa suppression ; l’API le refuse avec une erreur de classe
  `nemetonshiny_projet_ancien`. Il faut le recréer avec les mêmes
  parcelles.
- **Bases de données neuves** : le cœur 1.0.0 refuse toute base créée
  avant lui (erreur `nemeton_legacy_schema`). L’application l’explique
  maintenant en clair (« base antérieure à la 1.0.0 : la recréer »),
  pour la base de suivi sanitaire comme pour la base PostGIS.
  `inst/sql/schema.sql` décrit à lui seul le schéma complet (colonnes
  `_norm`, archives de reGénération et du plan d’actions) ; les six
  fichiers `migration_00N_*.sql` sont retirés.
- Supprimés :
  - le contrôle de « sens des indicateurs » (`indicator_sense_version`,
    invalidation et avertissement à l’ouverture) ;
  - la fonction exportée `projet_migrer()` et l’erreur
    `nemetonshiny_projet_perime` ;
  - la lecture des anciens formats de fichiers (`atomes.gpkg`,
    `atome_id`, métadonnées d’avant 0.16).

  L’initialisation des UGF à la première ouverture est conservée, sous
  son vrai nom (`ensure_project_ug()`), avec la mise de côté des
  fichiers UGF illisibles.
- [`projet_etat()`](https://pobsteta.github.io/nemetonshiny/reference/projet_etat.md)
  expose `format_projet` et `format_ok` à la place de `sens_vu`,
  `sens_courant` et `migration_necessaire` ;
  [`projets_lister()`](https://pobsteta.github.io/nemetonshiny/reference/projets_lister.md)
  expose `format_ok` à la place de `sens_a_jour`.

#### Valeurs du cœur 1.0.0 (spec 056) expliquées dans l’interface

- **P2 sans âge réel** : la BD Forêt ne fournit plus d’âge inventé, si
  bien que P2 (mode CHM) est vide. La fiche le dit (`p2_sans_age`) et
  propose le mode IFN. C1, retombé sur le NDVI faute d’âge et de modèle
  LiDAR, est signalé comme valeur indicative (`c1_ndvi_sans_age`).
- Nouveaux statuts traduits FR/EN :
  - P3 calculé sur le seul diamètre (ou diamètre et forme, ou diamètre
    et défauts) ;
  - P2 hors des courbes de station ;
  - indicateurs conditionnels sans leur source : microclimat (A3, A4,
    W4, R6), coupes rases SUFOSAT (T3), données spectrales (B4, L3), et
    zone non couverte.
- Couche INPN « zones humides » retirée : son motif ramenait surtout des
  ZNIEFF, et le cœur ne la lisait pas.
- FORDEAD exige **Python ≥ 3.11** (README, guide).

#### À faire par les utilisateurs

Recréer les projets, la base de suivi sanitaire (SQLite ou PostgreSQL)
et la base PostGIS si elle est utilisée. Les valeurs changent nettement
: W3 baisse de 50 à 70 points sur LiDAR, N1 d’environ 20 points, et P2
reste vide sans âge réel.

## nemetonshiny 0.157.1 (2026-10-06)

#### Added

- **Guide de l’application**
  ([`vignette("guide-application_fr")`](https://pobsteta.github.io/nemetonshiny/articles/guide-application_fr.md),
  article du site pkgdown), repris du cœur, qui le retire dans sa 1.0.0.
  Il est réécrit pour l’application actuelle : onglets et parcours
  (Sélection et UGF, Synthèse, Familles, Plan d’actions, Terrain, Suivi
  sanitaire FAST / FORDEAD / RECONFORT, reGénération), réglages, usage à
  plusieurs (rôles, verrou, pas d’isolation), API hors interface, liens
  profonds et serveur MCP. Brief
  `vers-nemetonshiny/2026-10-06-nemeton-guide-app-a-reprendre.md`.

## nemetonshiny 0.157.0 (2026-10-05)

Briefs du cœur `nemeton` 0.213.0 à 0.216.0. Plancher
`Imports: nemeton (>= 0.216.0)`.

#### Changed

- **Scores de famille et score global pondérés par la surface des UGF**
  (n° 66,
  [`nemeton::aggregate_family_scores()`](https://pobsteta.github.io/nemeton/reference/aggregate_family_scores.html)).
  Une UGF de 0,5 ha ne pèse plus autant qu’une UGF de 50 ha. **Les
  scores affichés changent** : onglet Synthèse (score global, radar,
  tableau), rapport PDF, prompt de synthèse IA,
  [`projet_etat()`](https://pobsteta.github.io/nemetonshiny/reference/projet_etat.md)
  et serveur MCP, qui donnent maintenant tous les mêmes chiffres. Le
  prompt IA citait jusque-là une moyenne simple des familles comme «
  score global », il reprend celui de l’onglet. Les valeurs des
  indicateurs ne changent pas : `indicator_sense_version` reste le même.
- **Composite NDVI Sentinel-2 de C2** construit par le cœur (n° 64,
  [`nemeton::build_ndvi_season_composite()`](https://pobsteta.github.io/nemeton/reference/build_ndvi_season_composite.html)),
  qui retire l’offset radiométrique des scènes traitées depuis 2022
  (0.215.0). L’ancien cache `ndvi_s2.tif`, biaisé (NDVI sous-estimé
  d’environ 0,3 en forêt), est supprimé ; le nouveau s’appelle
  `ndvi_s2_v2.tif`.
- **Indices ombrothermiques** (mois secs de Gaussen, De Martonne) du
  contexte reGénération calculés par le cœur (n° 65,
  [`nemeton::climate_ombrothermic_indices()`](https://pobsteta.github.io/nemeton/reference/climate_ombrothermic_indices.html)).

#### À faire par les utilisateurs

- **Recalculer les projets** : avec le cœur 0.212 à 0.216, R1
  (`fire_exp`), R5, S3, les indices de famille et C2 changent.
- **Relancer FORDEAD** sur les zones suivies, et s’attendre à des
  alertes FAST très différentes (mode tendance surtout) et à des courbes
  NDVI post-2022 nettement plus hautes : l’offset Sentinel-2 est
  corrigé. Les cartes FAST en cache sont recalculées seules (leur clé
  porte la version de radiométrie).

## nemetonshiny 0.156.1 (2026-10-05)

#### Added

- **Signal « page prête » pour VICTOR** (brief
  `vers-nemetonshiny/2026-10-05-signal-pret-pour-victor.md`). Ouverte
  par une autre page, l’application envoie une fois
  `postMessage({source: "nemetonshiny", type: "ready" | "invalid", project, tab})`
  à cette page. Le signal part quand le lien profond est appliqué et que
  Shiny est au repos depuis 1 s sans sortie recalculée, ou aussitôt pour
  un lien refusé. Origine `NEMETON_VICTOR_ORIGIN` (défaut
  `http://127.0.0.1:8788`, jamais `"*"`) ; rien n’est envoyé sans
  opener.

#### Fixed

- **Le serveur entier s’arrêtait** quand un onglet était fermé pendant
  la restauration d’un projet. Depuis shiny 1.14, lire un réactif d’une
  session fermée lève une erreur, et dans un rappel `later` elle
  remontait jusqu’à `runApp()`. Les 31 rappels passent par
  `.later_sur()`, qui ignore cette erreur et laisse remonter les autres.
- **Mode dev (`load_all(); run_app()`) cassé** :
  [`pkgload::load_all()`](https://pkgload.r-lib.org/reference/load_all.html)
  source `tests/testthat/helper-*.R`, et un patch de test remplaçait
  [`promises::future_promise`](https://rstudio.github.io/promises/reference/future_promise.html)
  par une fonction rendant `NULL`. Toute tâche asynchrone rendait donc
  `NULL` ; la restauration de projet se relançait en boucle (des
  milliers de fois par minute). Les patches ne s’appliquent plus que
  sous testthat (`TESTTHAT=true`).

## nemetonshiny 0.156.0 (2026-10-05)

Mise en œuvre des constats ouverts de l’audit 1.0, en huit lots.

#### Fixed : sécurité et robustesse

- **Secrets** : fichiers de clés Theia/LLM lisibles par le seul
  propriétaire dès leur création ; `PGSSLMODE` imposé hors localhost
  pour la base de suivi ; identifiants masqués dans les erreurs de base
  et dans les messages ntfy ; avertissement unique pour un topic ntfy
  public sans jeton.
- **Keycloak de développement** : realm renommé
  `keycloak/realm-nemeton-dev.json`, sans aucun secret en clair. Secret
  client et mots de passe viennent de variables **obligatoires**
  (`.env.example`) : `docker compose` refuse de démarrer sans elles.
- **Lecture seule** : commentaires, actions de desserte et plan
  d’actions n’écrivent plus rien en lecture seule. Le statut est
  recalculé à chaque changement d’authentification (connexion,
  déconnexion, autre compte). Audit et validation sanitaire signés par
  l’utilisateur connecté.
- **Écritures atomiques** : rognage OSO, mosaïque et tuiles LiDAR
  (`.part`) ; une mosaïque ratée rend `NULL` et non plus la première
  tuile seule. Un calcul dont aucun indicateur n’aboutit est un échec,
  et non plus un projet « terminé » vide.

#### Fixed : multi-utilisateurs

- **Langue par session** : changer de langue ne touche plus l’option
  globale partagée par toutes les sessions (`?lang=` au rechargement).
- **Hors de la boucle Shiny** : analyse reGénération, appels LLM
  (synthèse, familles, plan d’actions, conseil reGénération) et
  battement du verrou passent en tâches asynchrones. Délais pour
  l’enfant Python (30 min d’inactivité, 12 h au total) et chien de garde
  par étape de la chaîne.

#### Fixed : suivi sanitaire, caches, desserte

- Zones de suivi : vrai échec en cas de problème, garde sur le backend
  réel (SQLite local compris). G3 contrôlé avant FORDEAD. Annulations
  RECONFORT/FORDEAD/FAST fiables. RECONFORT calculé sur la zone `_tot`.
- Caches de couches indexés sur l’emprise. Réseau de desserte invalidé
  quand le tampon ou les parcelles changent, un GeoPackage par moteur.
  Rayon pris en compte dans le contexte reGénération. RVT régénéré si
  périmé.
- Import de ténements : CRS deviné et signalé ; éléments hors des
  parcelles rejetés et comptés (ils étaient rattachés en silence à la
  première parcelle). Une contenance absente ou nulle retombe sur la
  surface géométrique, et la migration UGF n’avorte plus.
- Configuration des sources : enregistrer un bloc ne fait plus perdre
  les saisies en cours des autres blocs.
- Score global indisponible : état « pas de données » au lieu d’un
  plantage.

#### Changed : interface et qualité

- Badges NDP lisibles (texte sombre, contraste AA) et traduits. Un seul
  bouton principal dans la Synthèse (GeoPackage en
  `btn-outline-primary`).
- Avertissements de téléchargement, rapport PDF et derniers textes en
  dur passés par l’i18n ; trois clés en double supprimées ; balisage cli
  retiré d’un message.
- Annonces de sélection au lecteur d’écran rétablies ; écouteurs
  JavaScript qui s’empilaient (piège de focus des modales, validation)
  dédoublonnés ; noms d’indicateurs échappés.
- Code mort supprimé (radar et palettes de `utils_theme.R`,
  `tenement_split_by_line()`, `db_load_parcels()`, `needs_migration()`,
  sept helpers cadastre/communes, `DATA_SOURCES` en double). Blocs
  roxygen remis sur leur fonction. Méthode `print` de l’i18n
  enregistrée.
- Logo allégé (1,1 Mo → 210 Ko). NEWS et CHANGELOG archivés avant
  0.130.0 (`NEWS-archive.md`, `CHANGELOG-archive.md`).
- CI : job `R-CMD-check-oldrel` (version précédente de R), non bloquant.
  Quatre tests vides supprimés, un test du formulaire projet réparé.

#### Documenté, pas corrigé

- **Pas d’isolation entre utilisateurs** dans une même instance (CONTRAT
  §2) : une instance correspond à un collectif de confiance.
- Trois calculs métier à rapatrier dans le cœur (composite NDVI, indices
  Gaussen et De Martonne, moyenne des UGF non pondérée par la surface) :
  brief `vers-nemeton/2026-10-05-logique-metier-a-rapatrier.md`.
- Reporté après la 1.0 : factoriser `mod_monitoring.R` (3 400 lignes,
  trois machineries de tâche presque identiques) et `mod_action_plan.R`.

## nemetonshiny 0.155.0 (2026-10-05)

#### Fixed — Briefs coeur 0.208 a 0.212 (audit 1.0) soldes cote app

- **T2 n’est plus NA sur tous les projets** (brief 0.212.0 §4). Depuis
  `nemeton 0.212.0`, T2 rend NA sans source ; l’app ne lui passait ni N2
  ni T1. N2 est desormais calcule avant T2 et transmis comme source
  prioritaire, T1 en repli (une source toute NA n’est pas transmise). Un
  T2 deja enregistre tout NA est recalcule au prochain lancement.
- **Methode de R1 expliquee** : `r1_status` (repli sans fireexposuR,
  sans BD Foret, echec de fireexposuR ; R1 non calcule faute de MNT ou
  de composante) traduit FR/EN dans la fiche de l’indicateur. Le bandeau
  retient le premier statut traduit : un repli sur une seule unite n’est
  plus masque par la methode nominale des autres.
- **Purge des caches de zones** :
  `prune_orphan_zone_caches(project_uuid =)` (brief 0.209.0) - rien
  n’est purge si la base connectee ne connait aucune zone du projet (app
  pointee sur une autre base).
- **Corpus RAG** : la racine du corpus (`nemeton.corpus_root` /
  `NEMETON_CORPUS_ROOT`) est resolue dans la session et transmise au
  worker d’import, qui ne recevait pas l’option (brief 0.210.0).
- **Sources documentaires et reponses IA rendues sans HTML brut**
  (`markdown_safe()`) :
  [`shiny::markdown()`](https://rdrr.io/pkg/shiny/man/markdown.html)
  laissait passer `<img onerror=...>` ; les citations Markdown du coeur
  ne sont pas echappees (brief 0.210.0). Les liens `<https://...>` des
  citations sont conserves.
- **Motifs du rapport d’import des validations** traduits
  (`unknown_stade`, `missing_stade`, `no_alert_within_snap`).

#### Added — Pilotage par un assistant (brief `specs/BRIEF-pilotage-victor-aigora.md`, lot A)

- **Serveur MCP** (`inst/mcp/server.R`, `mcptools` en `Suggests`) : huit
  outils pour Claude Code / AIGORA / VICTOR - `lister_projets`,
  `resume_projet`, `lancer_calcul`, `etat_calcul`, `annuler_calcul`,
  `generer_rapport`, `exporter_gpkg`, `url_app`. Fines enveloppes de
  l’API hors interface ; un projet se designe par son id ou son nom
  (casse et accents ignores ; plusieurs candidats -\> liste, jamais de
  choix silencieux). Reponses JSON `{"ok": ...}`.
  `session_tools = FALSE` : sans lui, mcptools transferait les appels
  vers la session RStudio ouverte. Verifie en stdio de bout en bout.
- **Calcul detache** : `lancer_calcul` demarre un `Rscript` en session
  propre (`setsid`) qui survit a la tache de l’assistant, par le meme
  chemin que l’application (plafond memoire, journal).
  `data/compute_job.json` suit pid, statut et erreur ; un processus
  disparu sans finir est signale `echec` avec la fin du journal. Refus
  si un calcul tourne (assistant ou application), si le projet est
  verrouille en edition, ou si une migration est necessaire.
- **Liens profonds** `?project=<id>&tab=<onglet>` : le projet s’ouvre
  par le meme chemin que les cartes ” projets recents “, l’onglet est
  selectionne une fois ce projet charge ; valeur inconnue ignoree avec
  un avertissement. `NEMETON_APP_PORT` fixe le port des liens construits
  par `url_app`.

#### A savoir

Les releases coeur 0.208 a 0.212 changent beaucoup de valeurs (volumes
de desserte, indice de regeneration, R3, R4, B1, B3, L1, L2, A1, A2, W1,
W2, R1, R5, R7, S1, S2, T1, T2, essences resineuses) : **recalculer les
projets**. Calibrages coeur a valider : borne B1 = 4 statuts, cout B3,
reference A2 = 100, contrastes OSO de L1, borne E1/E2 = 2,64.

## nemetonshiny 0.154.0 (2026-10-04)

#### Fixed — Une lecture ne detruit plus les indicateurs

- **`invalidate_indicators()` renomme au lieu de supprimer.** Un simple
  chargement d’un projet calcule avant un changement de sens (spec 048)
  supprimait `data/indicators.parquet` sans retour possible (incident du
  2026-10-04 sur Couchey, via `load_project()` -\>
  `ensure_indicator_sense_current()`). Le fichier devient
  `data/indicators.perime-v<sens>-<date>.parquet` ; deux generations
  sont conservees et listees dans `metadata.json` (`indicateurs_perimes`
  : fichier, sens, motif, date). Vaut pour toutes les invalidations
  (sens, decoupage UGF). Si le renommage echoue, les indicateurs restent
  en place.
- **Un projet neuf porte le marqueur de sens courant.**
  `create_project()` ne posait pas `indicator_sense_version` : un projet
  cree puis calcule sans passer par l’ouverture dans l’application etait
  vu en sens v1, et ses indicateurs tout juste calcules etaient
  invalides a la premiere ouverture.

#### Changed — Plancher coeur

- **`Imports: nemeton (>= 0.212.0)`** (brief coeur 0.212.0, seconde
  passe sur les calculs) : la borne de normalisation d’E1/E2 double
  (2,64, densite seche) ; tests alignes. Les projets existants sont a
  recalculer, beaucoup d’indicateurs changent de valeur (cf. NEWS de
  `nemeton` 0.212.0).

#### Added — API hors interface (brief aigora-nemeton du 2026-10-04)

- **Neuf fonctions exportees** pour piloter un diagnostic sans
  l’application
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
  [`projet_gpkg()`](https://pobsteta.github.io/nemetonshiny/reference/projet_gpkg.md).
  Fines enveloppes des services existants.
- **[`projet_lire()`](https://pobsteta.github.io/nemetonshiny/reference/projet_lire.md)
  n’ecrit rien** : meme construction que l’onglet Synthese (R5, R6/R7,
  scores de famille du coeur), sans les migrations d’ouverture. Si une
  migration serait necessaire, erreur classee
  `nemetonshiny_projet_perime` ; `projet_migrer()` l’applique
  explicitement. Test d’empreinte : aucun fichier du projet ne change.
- **Erreurs classees** (`nemetonshiny_erreur` et sous-classes) et
  contrat des commentaires du rapport documentes dans `CONTRAT.md`
  (section 1 bis).
- **`NEMETON_PROJECT_DIR`** fixe le dossier des projets par defaut hors
  interface (`run_app(project_dir =)` l’emporte).

#### Changed — Pilotage VICTOR / AIGORA, etape 1 : service de synthese

- **Scores de famille et score global extraits dans
  `R/service_synthesis.R`** (`project_family_scores()`,
  `project_family_means()`, `project_global_index()`,
  `project_synthesis_summary()`). L’onglet Synthese les consomme ;
  comportement inchange. Prealable au serveur MCP du brief
  `specs/BRIEF-pilotage-victor-aigora.md` : un consommateur sans
  interface obtient exactement les memes chiffres que l’onglet, sans
  dupliquer le code. `project_synthesis_summary()` renvoie une liste
  serialisable (score global, 12 familles, NDP, confiance, nombre d’UGF
  et de parcelles).

## nemetonshiny 0.153.0 (2026-10-03)

#### Changed — Phase 3 de l’audit 1.0 : contrat public et packaging

- **`run_app(options = list(...))` fonctionne.** `options` est un
  argument a part entiere, fusionne avec les valeurs par defaut ; passe
  dans `...`, il entrait en conflit avec l’option fixe de `shinyApp()`,
  si bien qu’il etait impossible de choisir le port ou l’hote. Le
  navigateur ne s’ouvre plus qu’en session interactive, jamais sur un
  serveur.
- **L’image Docker se construit et sert l’application** (verifie : HTTP
  200, tous les paquets se chargent). Elle ne se construisait pas : base
  `rocker/r-ver:4.4.0`, dont le depot CRAN fige a mi-2024 ne contient ni
  `ellmer` ni `shinyOAuth` (passage en 4.6.1, la version de la CI),
  `devtools` absent de l’image (remplace par `remotes`), `git` manquant
  pour les paquets GitHub, `libuv` manquant pour `fs`. L’application
  tourne sous un utilisateur non root, avec les projets dans le volume
  `/data` a chemin fixe, et le CMD passe `options` et `project_dir`.
- **Licence : GPL-3 ou ulterieure**, alignee partout (`DESCRIPTION`
  `GPL (>= 3)`, texte complet dans `LICENSE.md`, README, ADR-006) ;
  `LICENSE-EUPL.md` marque comme historique.
- **`CONTRAT.md`** : le contrat public de la 1.0 (point d’entree,
  variables d’environnement, format des projets et ses garanties, schema
  PostGIS, profils d’experts, politique de compatibilite).
- **README** reecrit : prerequis reels (chaine Rust pour `foretaccess`,
  dependances GitHub), installation par `pak`, tous les parametres de
  [`run_app()`](https://pobsteta.github.io/nemetonshiny/reference/run_app.md),
  image Docker.
- **R CMD check sans WARNING** : plus aucun caractere non ASCII dans
  `R/` (chaines en `\uXXXX`, commentaires translitteres ; table des
  traductions verifiee identique), `future`, `arrow` et `geoarrow`
  passent en `Imports` (l’asynchrone et l’enregistrement des projets en
  dependent), `lidR` et `methods` declares, fichier `.s2.out` retire. Le
  moteur facultatif `opencanopy` (hors CRAN) n’est pas declare : il est
  resolu a l’execution
  ([`getExportedValue()`](https://rdrr.io/r/base/ns-reflect.html)),
  derriere son
  [`requireNamespace()`](https://rdrr.io/r/base/ns-load.html), pour ne
  pas imposer son installation. La CI echoue desormais sur un WARNING.
- `main` est protegee : `version-consistency`, `R-CMD-check` et `tests`
  doivent etre verts avant un merge.
- `migration_001` signalee comme historique et destructive.

## nemetonshiny 0.152.5 (2026-10-03)

#### Fixed — Phase 2 de l’audit 1.0, lots B a D : ne plus perdre de donnees, ni les ecrire dans le mauvais projet

**Ecritures atomiques (lot B).** - `metadata.json`, parcelles
(GeoPackage et Parquet), tenements, `ugs.json`, indicateurs et
commentaires s’ecrivent sur une copie renommee ensuite sur la cible
(`R/utils_io.R` : `.write_json_atomic()`, `.st_write_atomic()`,
`.replace_file()`). Un worker tue en cours d’ecriture ne laisse plus un
JSON tronque (projet « corrompu ») ni un GeoPackage supprime sans
remplacant. - **Le decoupage UGF n’est plus ecrase sur une erreur de
lecture** : des fichiers UGF illisibles sont mis de cote dans
`data/ug_sauvegarde_<date>/` (trace dans `metadata$ug_sauvegarde`) avant
que la migration ne recree un decoupage par defaut. - **Un plan
d’actions illisible est copie** (`action_plan.illisible-<date>.json`)
avant d’etre remplace par un plan vide : la sauvegarde suivante ne
detruit plus les actions ni l’audit. - La synchronisation PostGIS d’un
projet est **transactionnelle** : un echec ne laisse plus le projet sans
parcelles, et deux synchronisations concurrentes ne dupliquent plus les
parcelles.

**Resultats asynchrones lies a leur projet (lot C).** - Fin ou
annulation d’un calcul : le projet n’est recharge que s’il est toujours
celui ouvert (`.est_projet_courant()`) ; l’affichage ne bascule plus sur
l’ancien projet pendant que verrou et sauvegardes suivent le nouveau. -
« Tout calculer » porte le projet qui l’a lance : l’etat du run s’ecrit
dans ce projet, et le run s’arrete si un autre projet est ouvert entre
deux etapes ; une reponse tardive ne rouvre plus un run clos. -
reGeneration (gel R7, contexte regional, moteur) : un resultat arrive
pendant qu’un autre projet est ouvert n’y est plus injecte. - Desserte
(OSM, detection, typage, optimisation), plan d’echantillonnage et
ingestion terrain sont remis a zero au changement de projet ;
l’ingestion attache le fichier **valide**, pour le projet de la
validation, et plus le fichier selectionne depuis. Appel
`import_qgis_gpkg()` au lieu de l’alias deprecie.

**Plan d’actions et UGF (lot D).** - **Le calendrier ne glisse plus au
1er janvier.** Les decalages `annee_cible` sont ancres sur
`plan$annee_base` (annee de creation ; pour un plan existant, annee de
sa premiere relecture, soit ce que l’utilisateur voit aujourd’hui). Une
seule conversion, `action_plan_annee_civile()`, pour la table, le
Kanban, le PDF, le GPKG et Marculus. **Le GPKG ne retranche plus un an**
: une action « 2028 » a l’ecran sort desormais « 2028 » dans le GPKG
terrain. - « Tout calculer » **n’efface plus jamais les actions** :
l’option « ecraser » restee cochee dans la modale IA n’est plus relue
par la chaine ; l’ecrasement manuel passe par la suppression auditee. -
**Editer le decoupage UGF invalide les indicateurs** (creation, fusion,
decoupe, deplacement : l’affectation tenement -\> UGF change) ; renommer
ou changer de groupe ne les touche pas. - **Changer les parcelles d’un
projet** met l’ancien decoupage UGF de cote, invalide les indicateurs et
le signale a l’utilisateur.

#### Fixed — Phase 2 de l’audit 1.0, lot A : plus de donnees inventees, plus de plantages francs

- **Plus de NDVI aleatoire.** Quand le WMS IGN echouait, l’app ecrivait
  dans `cache/layers/ndvi.tif` un raster `runif(0.5, 0.85)`, puis le
  relisait a chaque calcul comme une mesure (C1, masque du CHM).
  Desormais la couche est simplement absente (indicateur NA) et rien
  n’est mis en cache. Un ancien NDVI synthetique deja en cache est
  reconnu a sa signature (0,001 deg en EPSG:4326, valeurs entre 0,5 et
  0,85) et jete au prochain calcul : le projet retente le vrai
  telechargement.
- **Plus de valeurs aleatoires pour un indicateur inconnu.** Un
  indicateur dont la fonction n’existe pas dans le coeur (slug renomme,
  coeur trop ancien) recevait `runif(0, 100)`, sauvegarde comme une
  vraie valeur. C’est maintenant une erreur, que la boucle de calcul
  transforme en NA avec sa cause.
- **Plan de validation : plus de colonnes perdues.** L’accumulation dans
  `samples.gpkg` ne gardait que les colonnes communes avec les plans
  deja enregistres : un plan FAST persiste apres un plan FORDEAD
  effacait definitivement `alert_class` et `visit_order`. Union des
  colonnes (NA du bon type pour les manquantes) et ecriture sur une
  copie qui ne remplace le fichier qu’une fois ecrite.
- **Plantage de session sur les tiges non cubees.** `.format_m3()` sur
  un volume NA (tige Marculus sans hauteur) faisait tomber la session a
  chaque ouverture du projet.
- **Deploiement sans base de donnees** : un utilisateur connecte n’est
  plus mis en lecture seule comme si un autre tenait le verrou
  (`lock_acquire_or_null()` convertit la sentinelle « pas de base »).

## nemetonshiny 0.152.4 (2026-10-02)

#### Security — Phase 1 de l’audit 1.0 : failles fermees

Correctifs issus de la revue complete du depot (page « Audit
nemetonshiny 1.0 »).

- **Identifiant de projet valide cote serveur.** `get_project_path()`
  n’accepte plus qu’un segment de chemin unique (ni separateur, ni
  `.`/`..`, ni caractere de controle) et verifie que le dossier resolu
  reste sous la racine des projets, lien symbolique compris.
  L’identifiant arrive du navigateur et `delete_project()` supprime
  recursivement ce qu’il designe. La suppression d’un projet corrompu
  revérifie qu’il l’est, et refuse une session en lecture seule. La
  liste des projets prend le nom du dossier comme identifiant (un
  dossier copie gardait l’id de l’original).
- **L’authentification echoue ferme.** OAuth configure mais client
  indisponible (Keycloak injoignable, `shinyOAuth` absent) : la session
  reste non authentifiee, en lecture seule, au lieu de retomber en mode
  anonyme editeur.
- **« Sans role = editeur » reserve au mode anonyme.** Avec un
  fournisseur d’identite, un utilisateur sans role Nemeton n’a plus
  aucun droit (c’etait le cas par defaut avec Keycloak, qui ne transmet
  pas les roles dans `userinfo`). Les roles techniques de Keycloak
  (`offline_access`, `uma_authorization`, `default-roles-*`) sont
  ignores ; `gestionnaire`, role d’edition du realm livre, est reconnu.
  Une seule regle : `auth_has_role()` / `can_admin_app()`. **A faire sur
  un deploiement Keycloak existant** : publier les roles du realm dans
  `userinfo` (mapper « realm roles », claim `realm_access.roles`, option
  « Add to userinfo »), comme le fait desormais
  `keycloak/realm-nemeton.json`. Sans cela, tous les utilisateurs
  connectes passent en lecture seule.
- **Cles du serveur reservees a l’administrateur.** Enregistrer ou
  supprimer une cle Theia ou LLM (fichier dans le `~` du serveur,
  variables d’environnement de tout le processus) exige le role
  administrateur ; le mode anonyme (poste mono-utilisateur) le garde.
- **RAG** : la reinitialisation du corpus exige le role administrateur,
  comme les autres ecritures ; la table du rapport echappe tout sauf le
  badge d’action.
- **XSS stocke** : libelles et groupes d’UGF, nom de projet et essences
  Marculus sont echappes avant d’entrer dans les infobulles, popups,
  legendes et modales (editeur d’UGF, vues familles, plan d’actions).
- **PDF** : le nom de projet est echappe dans la page de titre du plan
  d’actions (un `_` ou un `&` faisait echouer le rendu) ; dans le
  rapport, les commentaires et metadonnees neutralisent l’antislash et
  le dollar, si bien qu’aucune commande TeX ne s’execute plus au rendu.
  `latex_escape()` imprime enfin un antislash correctement.
- **Mise a jour d’un projet** refusee a une session en lecture seule.
- **Docker** : `.dockerignore` exclut `.Renviron*`, `.env*`, `*.apikey`
  et `keycloak/` du contexte de construction (`COPY . /app`).
- **Realm Keycloak de developpement** : mapper des roles dans
  `userinfo`, `sslRequired` passe de `none` a `external`.

**Reste a faire par le proprietaire du depot** : `.Renviron.txt` est
suivi par git ; verifier qu’il ne contient que des valeurs factices et
faire tourner le topic ntfy s’il est reel.

## nemetonshiny 0.152.3 (2026-10-02)

#### Fixed — Bandeau d’invalidation : les Risques cites pour un projet venu de la v1

Depuis la v3 du sens des indicateurs (0.143.25), le bandeau affiche a
l’ouverture d’un projet invalide ne citait que Paysage, Dynamique
temporelle et Energie. Un projet calcule avant la v2 subit pourtant
aussi l’inversion des Risques (R1-R4, spec 048) : son utilisateur n’en
etait pas prevenu. `ensure_indicator_sense_current()` porte desormais la
version d’origine (`version_vue`), et un projet venu de la v1 recoit un
message qui cite les Risques (« plus le score est haut, moins l’UGF est
exposee »). Un projet venu de la v2 garde le message actuel.

## nemetonshiny 0.152.2 (2026-10-02)

#### Fixed — Houppiers : un echec n’est plus muet (brief du 2026-08-25, §2)

Le 2026-08-25, un projet recalcule n’avait ecrit aucun houppier, et rien
ne disait pourquoi : `precompute_houppiers()` rendait le meme `0` sur
cinq chemins, une erreur de segmentation finissait en avertissement
console, et l’export Marculus partait sans la couche, sans un mot. C’est
ce silence qui a laisse passer la panne « plus aucun houppier depuis fin
aout » (v0.144.1).

- **Une trace sur disque** : chaque calcul ecrit `metadata$houppiers`
  (`statut`, `nombre`, `date`, et `detail` en cas d’erreur). Les statuts
  distinguent ce qui demande des actions differentes : `ok`,
  `coeur_ancien`, `projet_absent`, `sans_chm`, `vide`, `chm_suspect`,
  `echec_segmentation`, `echec_ecriture`.
- **Une erreur n’est plus un resultat vide** : la segmentation qui leve
  rend un objet `houppiers_erreur` portant son message.
- **L’export le dit** : sans houppiers, le message de fin d’export le
  signale avec la raison consignee au calcul (ou « relancez le calcul »
  pour un projet calcule avant cette version) ; sans desserte, il
  renvoie a l’onglet Desserte. Le message passe alors en avertissement.

#### Changed — Date de martelage : le 1er janvier est annonce comme une annee cible (brief du 2026-08-24, option A)

Une action pas encore martelee part toujours vers Marculus avec le 1er
janvier de son annee cible, pour garder le tri de la liste sur le
telephone (decision du 2026-10-02, option A). Le message de fin d’export
dit desormais combien de contextes le portent : une annee de programme,
pas une date de chantier, a corriger dans Marculus au martelage (le
telephone permet deja de la modifier). Une date revenue du terrain
(import Marculus) continue de primer.

## nemetonshiny 0.152.1 (2026-10-02)

#### Fixed — P2 en mode CHM ne sature plus a 100/100 (ecart n. 17, `nemeton >= 0.207.0`)

En mode CHM, mode par defaut, P2 est un indice de station H0 **en
metres** (9 a 37 m), que la normalisation plafonnait a 15, le maximum en
m3/ha/an du mode historique : la plupart des peuplements sortaient a
100/100. `.add_normalized_indicators()` passe desormais la colonne de
statut de l’indicateur (`.p2_status = "indice_station_m"`, deja
transportee) a `nemeton::normalize_indicator(statut = )`, qui plafonne
alors a 40 m (H0 = 18,5 m donne 46/100). Le mode IFN (m3/ha/an) garde le
plafond de 15.

Le statut d’un calcul precedent est aussi retire avant d’ecrire le
nouveau : un P2 repasse du mode CHM au mode IFN, qui n’a pas de statut,
aurait sinon garde `indice_station_m` et ete normalise avec le plafond
en metres.

**Les scores P2 des projets en mode CHM baissent**, et la famille
Production avec eux : c’est la correction voulue. Un projet calcule avec
un coeur anterieur a 0.207.0 n’a pas de `.p2_status` : il faut
recalculer P2.

#### Added — Production du massif : prevision corrigee par FORMS-T (spec 054 lot 5-bis, `nemeton >= 0.206.0`)

Le panneau « Production du massif (IFN) » passe a
`ifn_production_domaines()` les covariables du massif
(`ifn_covariables_domaines()` : hauteur FORMS-T et altitude du MNT du
projet). Le coeur corrige alors la prevision de la SER de l’ecart entre
le massif et sa SER (`predicteur = "hybride"`, production en volume
seulement), avec une erreur reduite de 19 a 35 %.

- La hauteur est **toujours FORMS-T** (Theia, 10 m, en cm), quel que
  soit le CHM des indicateurs : le modele est cale dessus. L’annee la
  plus recente lisible est prise (de l’annee precedente jusqu’a 2019).
- Sans Theia, sans MNT, ou avec une covariable manquante, rien n’est
  passe et la prevision reste celle de la SER : le coeur annoncerait
  sinon une prevision hybride sans l’avoir corrigee.
- Le panneau nomme la prevision (SER, ou SER corrigee par FORMS-T de
  telle annee) et ajoute deux reserves : surface hors de la plage
  calibree (22 500 a 1 000 000 ha, variance extrapolee) et massif sans
  placette IFN (valeur predite, `nature = "prediction"`).

Plancher `Imports: nemeton (>= 0.207.0)`.

## nemetonshiny 0.152.0 (2026-10-02)

#### Added — Production IFN par sylvoecoregion : P2, E1 flux et production du massif (spec 054)

Cablage des modes **opt-in** du coeur (`nemeton >= 0.205.0`, plancher
`Imports` releve). Un nouveau bloc « Production IFN par sylvoecoregion »
dans Parametres › Sources & parametres choisit, par projet :

- **P2** : indice de station (CHM, defaut inchange) ou **production de
  la sylvoecoregion** (`indicateur_p2_station(source = "ifn_fh")`,
  m3/ha/an, Fay-Herriot) ;
- **E1** (seulement avec P2 IFN) : stock (defaut) ou **flux**
  (`production_field = "P2"`), avec une part recoltee choisie (curseur
  0-1) ou le taux observe par l’IFN dans la SER (`"ifn_ser"`). Aucune
  part par defaut n’est envoyee au coeur.

Au calcul, le code SER de chaque UGF est localise une fois
([`nemeton::localiser_ser()`](https://pobsteta.github.io/nemeton/reference/localiser_ser.html))
puis mis en cache (`data/ugf_ser.rds`, cle UGF + empreinte de geometrie
; un echec n’est pas mis en cache). P2 IFN et E1 flux **ne demandent
plus de CHM**. Les colonnes annexes du coeur (`P2_rse`, `P2_provenance`,
`P2_nature`, `E1_mode`) sont conservees en colonnes prefixees
(`.p2_rse`, …, `.e1_mode`, `.e1_taux`) et E1 recoit P2 et
`P2_provenance` dans ses unites. Un P2/E1 calcule sous un autre mode est
recalcule a la reprise.

Affichage : sous la carte P2, la valeur par SER avec sa RSE, son echelon
(SER / GRECO / national) et sa nature (modelisee ou moyenne brute), et
la mention que ce n’est **pas** la productivite de la station. E1 en «
recolte observee » est libelle « Bois-energie issu de la recolte
actuelle de la SER » avec un avertissement : jamais presente comme un
potentiel. La famille Production gagne un panneau « Production du massif
(IFN) » (`ifn_production_domaines()` sur l’union des UGF : valeur, RSE,
placettes, poids direct, part en bordure, avec reserves sous 0,2 /
au-dela de 50 % / sous 3 000 ha) et les ratios prelevement/production de
chaque SER (definitions IGN et vidange, RSE, mise en garde sur le biais
de +10 %). Calcules dans le worker de calcul (environ 5 s), persistes
dans `data/production_ifn.rds`.

Hors perimetre : covariables de domaine FORMS-T (`nemeton 0.206.0`, pas
encore publie) et `completer_volume_ifn()` (l’app ne comble pas P1).

## nemetonshiny 0.151.4 (2026-09-25)

#### Added — Plan d’actions : fiches des UGF selectionnees sous le graphique

Comme dans reGeneration, selectionner une ou plusieurs lignes du tableau
des actions affiche, sous le graphique du bilan, un bloc « Fiches des
UGF selectionnees » : une fiche par UGF (une par ligne) avec ses actions
triees par annee (annee, type, statut, priorite, volume, bilan), le
resume du martelage Marculus s’il existe, et un **commentaire libre**
par UGF. Les commentaires sont enregistres a part du plan
(`data/action_plan_ug_comments.json`, ecriture atomique, sauvegarde
differee d’une seconde) et ne sont pas modifiables en lecture seule. Le
bouton ambre « Inserer le conseil IA dans les UGF selectionnees »
recopie le dernier message de « Affiner le plan » (bloc JSON retire)
dans le commentaire de chaque UGF selectionnee. Verifie sous Chrome
headless : fiches affichees, tableaux sans debordement, commentaire
persiste puis relu.

## nemetonshiny 0.151.3 (2026-09-25)

#### Changed — Plan d’actions : le graphique du bilan passe sous le tableau

Dans la carte « Tableau des actions », la courbe du bilan cumule et les
totaux (cout, revenu, bilan, surface) etaient au-dessus du tableau. Ils
sont maintenant **sous** le tableau, juste apres la pagination. Le corps
de la carte n’est plus en mode « remplissage » : sinon, le tableau
s’etirait sur toute la hauteur et renvoyait le graphique tout en bas de
la carte. Verifie sous Chrome headless (pagination jusqu’a 521 px,
totaux a partir de 538 px) ; un test verifie l’ordre tableau puis
graphique.

#### Changed — reGeneration : le tableau des UGF s’intitule « Tableau des actions »

La carte du tableau de la colonne droite s’intitulait « UGF ». Elle
s’intitule maintenant « Tableau des actions » (nouvelle cle
`regen_table_title`). Le panneau vert des exports garde son titre, et
les fiches parcelles leur titre « UGF … ».

#### Removed — reGeneration : la case « Bilan hydrique seul (rapide) »

La case disparait de la barre laterale gauche : l’analyse lancee depuis
l’onglet est toujours complete (bilan hydrique et microclimat), comme
elle l’etait deja avec la case decochee, le reglage par defaut. L’option
reste dans le service
(`run_regeneration(cfg = list(hydric_only = TRUE))`) pour un appel
programmatique ; ses tests sont inchanges. La cle i18n
`regen_run_hydric_only` est supprimee.

#### Changed — reGeneration : le tableau des UGF comme celui du Plan d’actions

Dans reGeneration, sous-onglet « Carte + Tableau », le tableau des UGF
adopte la presentation de celui du Plan d’actions :

- **Colonne « UGF »** : la premiere colonne donne le libelle lisible («
  Foret domaniale d’Orleans – parcelle 1115 »), et non plus
  l’identifiant interne `ug_id`, qui sert de repli quand une UGF n’a pas
  de libelle. Les autres en-tetes sont traduits via les cles
  `regen_col_*`, et les valeurs arrondies a 2 decimales.
- **Recherche en regex** en haut du tableau, par exemple
  `parcelle 11|haute`.
- **Sous le tableau** : le nombre de lignes affichees (5, 10, 25, 50,
  toutes) et la pagination « Prec. 1 2 3 … Suiv. », en vert selon le
  theme, avec le compte d’UGF a droite.
- **Hauteur** : le tableau prend 60 % de la colonne et les fiches 40 %
  (contre 50/50 auparavant), pour que les lignes et la pagination
  tiennent.
- **Traduction** : tous les libelles du tableau passent par l’i18n.
- **Case « Masquer les UG mal couvertes » retiree** : elle masquait du
  tableau et des fiches les UG dont moins de 50 % de la surface a ete
  modelisee (`couverture_pct`, part de l’UG couverte par la grille
  microclimatique). Le tableau affiche desormais toutes les UG ; la
  colonne « Couverture (%) » dit sur quelle part de sa surface chacune a
  ete calculee.

L’ordre des lignes est inchange, donc le lien ligne -\> carte -\> fiche
parcelle aussi. Verifie sous Chrome headless sur une copie de «
Reconfort » : la recherche `parcelle 110|parcelle 111` ramene le tableau
de 24 a 13 UGF. Un test verifie le libelle, le repli sur l’identifiant
et l’ordre des lignes.

#### Added — un « i » explicatif sur Annee, Type et Priorite

Dans « Couche affichee », a droite de la carte des actions, chaque choix
porte un « i » (`info_popover_in_label()`) : un clic ouvre l’explication
sans changer de couche. Chacun dit comment la couleur d’une UGF est
choisie quand elle porte plusieurs actions :

- **Annee** : la prochaine echeance, c’est-a-dire l’annee la plus proche
  parmi ses actions (clair = proche, fonce = lointaine) ;
- **Type** : le premier type par ordre alphabetique ;
- **Priorite** : la plus haute (rouge, orange, vert).

Dans les trois cas, le gris signale une UGF sans valeur. Un test verifie
les trois « i ».

## nemetonshiny 0.151.2 (2026-09-25)

#### Changed — Plan d’actions : le choix de coloration passe a droite de la carte

Dans le sous-onglet « Carte + Tableau », le choix de coloration (Annee,
Type, Priorite) quitte l’en-tete de la carte, ou il se serrait contre le
titre. Il passe dans une barre laterale **a droite de la carte**, titree
**« Couche affichee »**, comme la carte de reGeneration :
`layout_sidebar`, barre toujours ouverte, boutons a la verticale. La
barre ne fait que 150 px et la marge autour de la carte est reduite,
pour que la carte garde sa largeur face au tableau. L’identifiant
`map_color_by` et le comportement sont inchanges.

Verifie sous Chrome headless : dans une fenetre de 1900 px, la carte
mesure 534 x 1048 px avec le panneau a sa droite, et le passage a « Type
» recolore la carte et sa legende. Un test verrouille la structure :
barre a droite, toujours ouverte, carte dans la zone principale et choix
dans le panneau.

## nemetonshiny 0.151.1 (2026-09-25)

#### Fixed — reimporter un martelage apporte enfin les volumes des tiges

Sur « Reconfort », les actions avaient bien leur volume total (244,7 m3
et 151,4 m3), mais les 141 tiges stockees n’en portaient aucun. Le
tableau et la carte de la fiche restaient donc sans volume. Les tiges
avaient d’abord ete importees depuis des CSV au format 2, sans volumes,
puis reimportees depuis des CSV au format 3. Les memes tiges revenaient,
avec le meme `uuid` et le meme `modifie`. A egalite, la fusion gardait
la version **deja stockee**, celle sans volume.

La version tout juste importee gagne desormais a egalite de `modifie`.
Il suffit de **reimporter** les fichiers : les tiges recuperent leurs
volumes, leur methode de cubage et leur mode de mesure. Un test couvre
ce cas, et la mutation est detectee.

## nemetonshiny 0.151.0 (2026-09-25)

#### Added — la fiche d’une action martelee : plan, diagramme par classe, tableau

Un double-clic sur une fiche du Kanban ouvre la fenetre d’edition de
l’action. Quand l’action a des tiges importees de Marculus, une section
**Martelage** s’ajoute sous le formulaire, dans une fenetre plus large
et defilante :

- **Plan de situation** : l’UGF de l’action et ses tiges georeferencees,
  colorees par essence avec les couleurs envoyees au telephone, cadre
  sur la parcelle. Fonds OSM ou satellite. L’infobulle d’une tige donne
  l’essence, la classe, la categorie et le volume.
- **Diagramme debout, empile par classe** : une barre par essence,
  empilee par classe de PB (en bas) a TGB, chaque classe dans la couleur
  de sa categorie, eclaircie pour la plus petite classe de la categorie.
  C’est le « detail par classe » de l’ecran Statut de Marculus, avec les
  memes couleurs et la meme nuance, mais redresse. Le total de l’essence
  s’affiche au-dessus de chaque barre, et la legende est groupee par
  categorie.
- **Tableau des tiges designees** : date, essence, classe, categorie,
  quantite, hauteur, qualite, volume, parcelle et fix GNSS.

Les tiges affichees sont celles qui restent **apres les annulations**,
selon la regle de Marculus : une annulation retire la derniere tige de
sa case (`marculus_tiges_designees()`). Les categories suivent les
seuils par defaut de Marculus (`SeuilsCategories.DEFAUT`) : PB \< 27,5
cm \<= BM \< 47,5 \<= GB \< 67,5 \<= TGB, en diametre. Une circonference
est ramenee au diametre, et le mode de mesure du contexte est desormais
recopie sur chaque tige a l’import. Les seuils reglables sur le
telephone ne sont pas exportes : ce sont donc ceux par defaut qui
s’appliquent.

Correctif general au passage : dans une fenetre, les widgets DT et
leaflet restaient vides. Ils reportent leur rendu tant que leur
conteneur n’a pas de taille, ce qui est le cas pendant l’animation
d’ouverture, et htmlwidgets ne les relance pas sur `shown.bs.modal`. Un
relais dans `custom.js` declenche `shown.htmlwidgets` a l’ouverture de
toute fenetre.

Verifie sous Chrome headless (vrai module, vrais scripts du Kanban,
copie de « Reconfort », 40 tiges) : le double-clic ouvre la fiche, la
carte charge ses tuiles et ses 41 formes, le diagramme dessine 42
segments groupes PB, BM, GB et TGB, et le tableau pagine « Tiges 1 a 15
sur 40 ».

## nemetonshiny 0.150.0 (2026-09-25)

#### Added — les volumes du martelage reviennent de Marculus

Marculus exporte desormais les volumes qu’il calcule sur le telephone
(commit `8cf54a9`, apres Marculus v0.50.1 ; brief
`briefs/vers-marculus/2026-09-25-volumes-exportes.md`). Marculus reste
ainsi la **seule source** des volumes de martelage : Nemeton ne
recalcule rien, et le chiffre affiche au bureau est celui que
l’operateur a vu sur le terrain.

- **Lecture** : volumes unitaires par tige (`volumeTigeM3`,
  `volumeHouppierM3`, `volumeTotalM3`, `surfaceTerriereM2`, `cubage`) et
  totaux nets par contexte, depuis le `.marsync` et depuis le CSV
  `FormatCsv;3`. Le format 2, sans volumes, reste accepte : il laisse
  les volumes de l’action inchanges.
- **Action** : les totaux du telephone font foi.
  - `quantite$volume_m3` recoit le bois fort tige et alimente la colonne
    Volume et le bilan.
  - `volume_total_m3`, `surface_terriere_m2` et `nb_tiges_non_cubees`
    s’ajoutent.
  - La fiche lit une copie separee, `volume_martele_m3`, pour ne jamais
    presenter une estimation de l’IA comme un volume martele.
- **Fiche Kanban** : « Martelage du 15/10/2027 · 4 tige(s) designee(s) ·
  dont 1 Biodiversite · 3,31 m3 bois fort ».
- **Synthese** : le volume total de l’action en badge, une colonne
  Volume (m3) par essence avec un total, et un avertissement quand des
  tiges n’ont pas pu etre cubees (EMERGE sans hauteur saisie).
- **Carte** : l’infobulle d’une tige donne son volume et la methode de
  cubage.

Le volume par essence et par classe suit **la regle d’annulation de
Marculus** (`VolumesMartelage.totaux()`) : une annulation retire la
**derniere** tige comptee de sa case, avec son volume, et non le volume
de sa propre saisie. Verifie sous Chrome headless sur une copie de «
Reconfort » : une annulation sur le hetre 40 retire 0,87 m3, et la
synthese totalise 3,31 m3, comme le telephone.

Tests : totaux du `.marsync` appliques a l’action, format 2 sans effet
sur le volume, regle d’annulation (la mutation est detectee), CSV
`FormatCsv;3`, fiche et synthese avec et sans volume.

## nemetonshiny 0.149.0 (2026-09-25)

#### Changed — retour Marculus : fiche Kanban, statut realise, synthese lisible

- **Le martelage fait passer l’action a « Realisee »** dans le Kanban :
  un contexte revenu avec au moins une tige designee (compte net \> 0)
  change de colonne, meme si le telephone l’a laisse en « Planifiee ».
  Un contexte sans tige garde le statut du telephone.
- **La fiche Kanban montre le martelage** sous le commentaire, par
  exemple « Martelage du 15/10/2027 · 10 tige(s) designee(s) · dont 1
  Biodiversite ». Le compte « Biodiversite » est celui des tiges de
  qualite `Biodiversite` (arbres-habitats, a cavites, bois mort sur
  pied), net des annulations. Il est garde dans l’action
  (`quantite$nb_tiges_biodiversite`) et expose par
  `actions_to_dataframe()`, comme `date_martelage`. La ligne n’apparait
  qu’apres un martelage : un nombre de tiges seul peut venir du plan IA.
- **La synthese ne deborde plus de la page** : elle prend la forme de la
  feuille de martelage, avec une ligne par essence, une colonne par
  classe et des totaux, au lieu d’une ligne par couple essence x classe
  (25 lignes pour une eclaircie). Elle defile a l’interieur de la
  fenetre (`modal-dialog-scrollable`), et le bouton « Fermer » reste
  visible. Le bilan de l’import s’affiche dans la fenetre : en toast, il
  recouvrait « Fermer ».

Verifie sous Chrome headless sur une copie de « Reconfort », dans une
fenetre de 900 px : la fenetre s’arrete a 839 px, aucun toast ne la
recouvre, et la fiche importee passe en « Realisee » avec sa ligne de
martelage.

#### Fixed — import Marculus : un fichier vide est dit vide

Deux televersements de CSV, le 2026-09-25, ont affiche « Fichier
illisible : ce n’est pas un export Marculus (.marsync ou sauvegarde
JSON) ». Les fichiers recus par l’app faisaient **0 octet** : le contenu
manquait des l’origine, par exemple a cause d’une copie depuis le
telephone interrompue ou d’une synchronisation pas terminee. Le format
n’etait donc pas en cause. Le message envoyait pourtant chercher de ce
cote, et ne citait meme pas le CSV.

- Un fichier vide recoit maintenant son propre message : « Fichier vide
  (0 octet), rien a importer : verifiez qu’il a bien ete copie depuis le
  telephone ».
- Le message « illisible » cite les trois formats acceptes : `.marsync`,
  sauvegarde JSON et CSV de contexte au format 2.
- Les messages donnent le **nom d’origine** du fichier, et non le nom
  temporaire que Shiny lui attribue (`0.csv`).

Un test couvre le cas du fichier vide.

## nemetonshiny 0.148.3 (2026-09-25)

#### Fixed — import CSV Marculus : la qualite du fix sous une seule forme

Marculus a livre le CSV `FormatCsv;2` dans sa **v0.48.0** (reponse
`briefs/traites/2026-09-25-reponse-marculus-csv-format-2.md`). Sa
colonne `QualiteFix` garde le **libelle** (« RTK fixe », « Estime »…),
alors que le `.marsync` porte le **nom** de l’enum (`RTK_FIXE`,
`ESTIME`…). Une meme tige pouvait donc revenir sous deux formes selon le
fichier importe. Les libelles du CSV sont maintenant ramenes au nom de
l’enum, selon la table de `FixGnss.kt`. Un test le verifie.

Le brief Marculus « l’export ecrit CIRCONFERENCE » (Marculus v0.49.0)
etait deja traite par la v0.148.2 : contextes en `DIAMETRE`, classes de
20 a 90 par pas de 5, verifies sur les 20 contextes de « Reconfort ». La
reponse est deposee dans
`briefs/vers-marculus/2026-09-25-reponse-export-mode-diametre.md`.

## nemetonshiny 0.148.2 (2026-09-25)

#### Changed — les contextes Marculus partent au diametre, 20 a 90 cm

L’export creait ses contextes de martelage en **circonference**, avec
des classes de 20 a 200 cm par pas de 5. Chaque chantier devait donc
etre corrige sur le telephone. Il reprend desormais les valeurs par
defaut d’un contexte cree dans Marculus (`CreationContexteScreen.kt`) :
**mesure au diametre, classes de 20 a 90 cm par pas de 5**. L’operateur
peut toujours les modifier sur le telephone. Un test fixe ces valeurs.

## nemetonshiny 0.148.1 (2026-09-25)

#### Changed — la feuille de martelage arrive en couleurs, celles de Marculus

Jusqu’ici, l’export vers Marculus envoyait toutes les essences en blanc
sur noir : sur le telephone, les colonnes de la feuille de martelage se
ressemblaient toutes. Chaque essence porte desormais sa couleur, tiree
du referentiel chromatique **BD Foret® V2**, celui dont Marculus derive
ses propres couleurs (`Referentiels.COULEURS_ESSENCES_DEFAUT`,
`docs/essences-bdforet-v2.html`).

- **Couleur par famille** : les chenes decidus en bleu, le hetre en
  indigo, le sapin en grenat, le douglas en brique, le pin sylvestre en
  orange, les autres feuillus en bleu-gris, etc. L’epicea prend le
  rouge-orange, comme dans Marculus, pour se distinguer du sapin, que la
  BD Foret regroupe avec lui.
- **Pas deux colonnes identiques** : deux essences d’une meme famille,
  comme le chene sessile et le chene pedoncule, ou le frene, l’erable et
  le charme (tous « autre feuillu »), recoivent des **nuances** de la
  couleur de famille, plus claires puis plus foncees. Elles restent donc
  reconnaissables, sans se confondre.
- **Texte lisible** : blanc ou noir, selon le meilleur contraste WCAG,
  avec au moins 4,5:1 sur les dix essences des profils de groupe.

Les couleurs sont encodees comme Marculus les stocke : entier ARGB
signe, comme le donne `Color.toArgb()`. Tests : couleurs du referentiel,
nuances distinctes, absence de doublon, format ARGB, contraste. Controle
sur l’export reel de « Reconfort ».

## nemetonshiny 0.148.0 (2026-09-25)

#### Added — « Importer de Marculus » accepte les CSV de contexte (FormatCsv;2)

Marculus ecrit desormais, dans son CSV de contexte, les cles qui lui
manquaient (commit `8848a67`, publie apres Marculus v0.47.0 ; brief
`briefs/vers-marculus/2026-09-25-csv-importable-nemeton.md`) :

- en-tete : `FormatCsv;2`, `ContexteId`, `Statut`, `DateMartelage`,
  `Modifie` ;
- journal : colonnes `Uuid`, `Parcelle` et `Modifie` en fin de ligne.

L’import les lit comme un `.marsync` :

- l’action est retrouvee par `ContexteId` ;
- les tiges sont unies par `Uuid`, donc un CSV et un `.marsync` du meme
  contexte ne se doublent pas ;
- le terrain fait foi pour le statut et la date ;
- les tiges nouvelles apparaissent sur la carte et dans la synthese.

La modale d’import accepte maintenant `.marsync`, `.json` et `.csv`. Un
CSV de l’ancien format, sans `FormatCsv`, donc sans identifiants, est
refuse. Un message dedie invite alors a le reexporter ou a partager le
`.marsync`, au lieu d’un « fichier illisible ».

Tests (fixtures conformes a `ExportCsv.kt`) : lecture complete, avec un
nom a `;` echappe, un horodatage avec ou sans fraction de seconde, un
point decimal et la parcelle ; application a l’action ; union CSV +
`.marsync` sans doublon ; format 1 signale a part ; date vide sans
effet.

## nemetonshiny 0.147.1 (2026-09-25)

#### Changed — bouton « Importer de Marculus »

Le bouton d’import du Plan d’actions s’appelle desormais « Importer de
Marculus » (en : « Import from Marculus »), au lieu de « Importer les
donnees Marculus ». Il fait ainsi pendant a « Telecharger vers Marculus
», juste au-dessus.

## nemetonshiny 0.147.0 (2026-09-25)

#### Added — importer le martelage Marculus dans le Plan d’actions

Un bouton « Importer les donnees Marculus » rejoint la barre laterale
droite du Plan d’actions, sous « Telecharger vers Marculus ». Il ferme
la boucle : les chantiers partent vers le telephone, et leur martelage
revient dans le plan.

- **Fichiers acceptes** : les `.marsync` partages depuis le telephone
  (un par contexte) et la sauvegarde complete, au meme format JSON.
  Plusieurs fichiers sont acceptes a la fois. Les tiges sont unies par
  `uuid`, comme sur le telephone : reimporter un partage ne double rien.
- **Appariement** : un contexte exporte par Nemeton porte l’`id` de son
  action. Un contexte cree a la main sur le telephone n’a pas d’action :
  il est compte et signale, pas applique.
- **Actions mises a jour, le terrain fait foi** : statut Kanban, date de
  martelage (`date_martelage`, reutilisee par l’export suivant), annee
  cible quand elle reste dans l’horizon du plan, et nombre de tiges
  martelees (`quantite$nb_tiges`). Ce nombre est net des annulations,
  selon la regle du telephone (PLUS - ANNULATION). Chaque changement
  passe par l’audit, qui garde l’ancienne valeur. Un reimport identique
  ne modifie rien.
- **Carte** : une couche « Tiges martelees » affiche les tiges
  geolocalisees, avec l’essence, la classe, la hauteur, la qualite et la
  date au clic. Les annulations, qui retirent un compte sans designer
  une tige, ne sont pas dessinees. Les tiges sont stockees dans le
  projet (`data/marculus_tiges.json`).
- **Synthese** : apres l’import, une modale donne pour chaque action le
  nombre de tiges par essence et par classe, comme la feuille de
  martelage du telephone.

Nouveau service `R/service_marculus_import.R`, 19 cles i18n. Tests : 23
pour le service (totaux nets, union par `uuid`, terrain qui fait foi,
audit, reimport neutre, date de l’annee en cours, fichier illisible,
date reprise a l’export) et 2 pour le module. Mutations detectees.
Parcours verifie sous Chrome headless sur une copie de « Reconfort » :
modale, televersement, import, synthese, couche de tiges sur la carte.

## nemetonshiny 0.146.2 (2026-09-24)

#### Fixed — l’ortho ne s’affichait pas dans Marculus

Le fond ortho arrivait bien dans les GeoPackages, mais Marculus ne
proposait jamais « Ortho » : le bouton de fond passait de Satellite a
OSM. Avec `TILING_SCHEME=GoogleMapsCompatible`, GDAL declare une matrice
de tuiles pour chaque zoom de 0 a 19, alors que les tuiles n’existent
qu’aux zooms 13 a 19. A la premiere ouverture, Marculus reprojette la
table (`TileReprojection.reproject()`, NGA geopackage 6.7.4) en
parcourant toutes les matrices. Sur une matrice vide,
`TileDao.getBoundingBox(zoom)` vaut `null`, puis `getTileGrid()` leve
une `NullPointerException`, que `ouvrirOrtho()` avale.

Reproduit sur PC avec la bibliotheque NGA de bureau (`geopackage-core`
6.6.7, la version de Marculus) sur un fond de « Reconfort » : meme
exception, meme pile d’appels. Sans les 13 matrices vides, la
reprojection reussit (106 tuiles, zooms 13 a 19).

- Les matrices vides sont retirees a la construction du fond
  (`.marculus_ortho_matrices_pleines()`, qui demande `RSQLite`). Sans
  `RSQLite`, le fond n’est pas expedie : une table illisible vaut moins
  que pas de fond du tout.
- Les fonds deja en cache (v0.146.0-0.146.1) sont repares a l’export par
  une seule requete SQL, sans rien retelecharger.
- Verifie sur les GeoPackages de « Reconfort » tels qu’ils partent vers
  le telephone : au moment de la release, 11 sur 20 avaient ete ouverts
  avec la bibliotheque NGA, tous avec succes. Les 9 derniers etaient
  encore en cours.

A savoir : la premiere ouverture d’un chantier reprojette encore ses
tuiles dans Marculus, meme si elles sont deja en Web Mercator. Sur PC,
cela prend 23 a 86 s selon le chantier ; ce sera plus long sur un
telephone. Un brief est adresse a Marculus pour sauter cette etape
inutile (`briefs/vers-marculus/2026-09-24-ortho-deja-web-mercator.md`).

## nemetonshiny 0.146.1 (2026-09-24)

#### Fixed — « Telecharger vers Marculus » ne telechargeait rien (v0.146.0)

Depuis la 0.146.0, le bouton visible prepare les fonds ortho puis clique
sur un `downloadButton` **masque**. Or Shiny suspend le rendu d’une
sortie invisible : ce lien restait `disabled`, sans `href`, et le clic
envoye par le serveur ne menait nulle part. Constate dans un harnais
sous Chrome headless (le vrai module, le vrai `custom.js`, le projet «
Reconfort ») : le lien Marculus avait un `href` vide, alors que ceux du
GeoPackage et du PDF, visibles, etaient rendus. La sortie est desormais
declaree `suspendWhenHidden = FALSE`. Verifie dans le meme harnais :
clic sur le bouton, puis clic automatique sur le lien, puis archive
telechargee (122 Mo).

#### Fixed — deux chantiers identiques s’ecrasaient dans le lot

Deux actions du meme type sur la meme UGF (« Reconfort » : deux
eclaircies sur trois UGF) donnaient le meme nom de contexte, donc le
meme fichier. Le second GeoPackage ecrasait le premier (20 contextes
pour 17 fichiers), et le telephone listait deux contextes
indiscernables. Les doublons prennent maintenant leur annee, par exemple
« … eclaircie (2027) », puis un rang, « (2030
[\#2](https://github.com/pobsteta/nemetonshiny/issues/2)) », quand
l’annee ne suffit pas. Resultat : 20 contextes, 20 GeoPackages, chacun
rattache a son fichier. Les noms uniques restent inchanges.

## nemetonshiny 0.146.0 (2026-09-24)

#### Added — fond ortho 20 cm dans chaque GeoPackage Marculus

Chaque chantier exporte vers Marculus emporte maintenant son **fond
orthophoto IGN 20 cm** (`HR.ORTHOIMAGERY.ORTHOPHOTOS`), sur les
parcelles de son UGF elargies de 50 m. Sur le telephone, la carte
propose alors **OSM -\> Satellite -\> Ortho** : Marculus n’offre « Ortho
» que si le GeoPackage contient une table de tuiles (`CarteScreen.kt`),
et il prend la premiere. Le fond est ecrit dans la grille Web Mercator
standard (`GoogleMapsCompatible`, zoom 19, soit ~20 cm a nos latitudes),
avec ses niveaux inferieurs jusqu’au zoom 13, pour qu’il reste visible
en dezoomant.

Un tel fond coute ~40 s par chantier : c’est trop pour un
`downloadHandler`, qui est synchrone. Le bouton « Telecharger vers
Marculus » prepare donc d’abord les fonds **manquants** dans une tache
de fond (`ExtendedTask`, toast de progression, bouton desactive), puis
declenche lui-meme le telechargement. Les fonds sont mis en cache par
UGF (`cache/layers/ortho_marculus/`), sous un nom qui porte l’empreinte
de l’emprise : une UGF redecoupee est reconstruite. Un fond qui echoue
(WMS injoignable) n’empeche pas l’export : ce chantier part sans ortho,
et un toast le signale.

Mesure sur « Reconfort » : **16 fonds prepares en 239 s** la premiere
fois (93 Mo de cache, tuiles WMS telechargees 4 par 4) ; l’export prend
ensuite **13 s** et produit 20 GeoPackages avec ortho, dans un zip de
111 Mo.

Nouveau service `R/service_marculus_ortho.R`, nouveau handler JS
`nemetonClickElement`, deux cles i18n. 25 tests pour le service (emprise
alignee sur la grille, cache, decoupage WMS, construction hors ligne,
echec sans fichier partiel, fond en cache non modifie) et 2 pour le
module. Les mutations sont detectees.

#### Fixed — la date de martelage exportee etait en l’an 1, 2, 3…

`annee_cible` est un **decalage** depuis l’annee en cours : le tableau
du Plan d’actions affiche `annee + annee_cible`. L’export Marculus en
faisait directement une annee civile, et les contextes arrivaient avec
une date de martelage au 1er janvier de l’an 1 a 13. Ils portent
maintenant l’annee reelle (2027 a 2046 sur « Reconfort »). Une valeur
qui est deja une annee civile (\>= 1000) est gardee telle quelle. Les
tests existants ne passaient que des annees civiles, ce qui masquait le
defaut : un test avec un vrai decalage est ajoute.

## nemetonshiny 0.145.0 (2026-09-24)

#### Fixed — « Telecharger vers Marculus » : 133 s -\> 9 s

Avec ses houppiers revenus, l’export de « Reconfort » (20 chantiers, 80
982 houppiers) prenait 133 s, pendant lesquelles la session Shiny
restait figee. Il prend maintenant **9,4 s**, pour un contenu identique
: les memes 63 887 houppiers repartis dans les memes GeoPackages, chacun
avec ses trois couches (`parcelle`, `desserte`, `houppier`). Trois
causes, mesurees :

1.  **Le decoupage des houppiers par chantier : 81 s, soit 96 % du
    profil R.** Pour chacun des 20 chantiers, `st_intersects()` testait
    les 80 982 houppiers en WGS84, donc via s2, qui reconstruisait une
    geometrie spherique pour chaque houppier a chaque appel. Les
    houppiers sont maintenant projetes en Lambert-93 **une fois**, puis
    testes contre toutes les emprises en **un seul** `st_intersects()`
    GEOS plan, l’index portant sur les 20 emprises : **2,3 s**. Nouveau
    helper `.marculus_houppiers_par_zone()` ;
    `marculus_write_action_gpkg()` recoit les houppiers deja filtres
    (`houppiers_filtres = TRUE`).
2.  **Les ecritures GeoPackage : 31,2 s d’horloge pour 2,7 s de CPU.**
    SQLite forcait une ecriture disque a chaque transaction. Ces
    GeoPackages sont temporaires (zippes puis effaces) :
    `OGR_SQLITE_SYNCHRONOUS = OFF`, passe par appel via `config_options`
    sans toucher a l’environnement de la session, ramene les ecritures a
    **1,4 s**.
3.  **`zip -9` : 6,5 s**, contre 1,7 s en `-6` (le niveau par defaut),
    pour une archive plus lourde de 0,7 % seulement (19,3 Mo).

Tests : repartition entre plusieurs chantiers (houppier a cheval garde
entier, chantier vide, CRS conserve), absence de refiltrage d’un
GeoPackage deja filtre, et options d’ecriture. Mutations detectees.

## nemetonshiny 0.144.2 (2026-09-23)

#### Changed — montee vers `nemeton` 0.199.2 : houppiers et RECONFORT

Plancher `Imports: nemeton (>= 0.199.2)`. Ce cycle consomme les reponses
du cœur aux deux briefs du jour
(`briefs/vers-nemetonshiny/2026-09-23-consolide-nemeton-0.199.2.md`).

**Houppiers : le contournement de la 0.144.1 est retire.** Le cœur a
trouve la cause : sous un plan `future` a 2 workers ou plus, celui de
l’app, lidR convertissait le CHM en
[`raster::raster()`](https://rdrr.io/pkg/raster/man/raster.html), et son
CRS PROJ4 ne valait plus EPSG:2154 pour `sf`. Le processus `callr`
reussissait parce qu’il demarrait en plan sequentiel. Le cœur 0.199.2
passe desormais une copie `stars` a lidR. L’app appelle donc de nouveau
`segment_houppiers(chm, aoi = emprise)`, dans le processus. Le processus
neuf et le filtrage d’emprise applicatif sont retires ; le cœur garde
entiers les houppiers qui touchent l’emprise (`emprise = "intersecte"`).
La reparation de l’emprise (`st_make_valid()` en 2154) est conservee.

Mesure sur « Reconfort », apres `load_project()` et sous
`plan(multisession, workers = 4)` : **80 982 houppiers en 54 s**, contre
185 s avec le contournement, qui segmentait toute la mosaique.

**RECONFORT : la couche de probabilite devient P(atteinte).** Le cœur
0.199.0 la decrit comme P(deperissant) + P(tres deperissant), sur
0-1000, « haut = mauvais », dans un fichier a une bande. Le libelle
devient « Probabilite d’atteinte » / « Probability of dieback », et
l’infobulle le decrit (elle parlait de « confiance de la classification
»). Les deux avis `[minmax] min and max values not available` ne
s’affichent plus a l’ouverture d’un projet.

A savoir : les runs RECONFORT existants ont ete produits avec des
probabilites ecretees a 255 (defaut iota2
[\#12](https://github.com/pobsteta/nemetonshiny/issues/12), corrige cote
cœur). Leur score continu est compresse, par exemple 24-58 au lieu de
1-100 sur « Reconfort », et leur P(atteinte) plafonne a 510. Le cœur le
signale une fois par session dans la console. Il faut relancer RECONFORT
sur ces projets : Fordead z5, Reconfort z9, Couchey z49 et Aumur z53.

## nemetonshiny 0.144.1 (2026-09-23)

#### Fixed — l’export Marculus repart avec sa couche de houppiers

Depuis fin aout, aucun projet ne produisait plus de houppiers au calcul
des indicateurs, et « Telecharger vers Marculus » partait sans couche
`houppier`. Chaque `compute_child.log` portait « Segmentation des
houppiers : st_crs(x) == st_crs(y) n’est pas TRUE ». Le cache de «
Fordead » datait du 26 aout ; « Reconfort » n’en avait jamais eu.

Deux causes se cumulaient.

1.  **L’emprise tombait a `NULL` sans rien signaler.** Un contour de
    parcelle qui se recoupe faisait echouer l’union en WGS84 (s2 : «
    Edge 0 crosses edge 3 »), et `.marculus_aoi()` avalait l’erreur. Le
    contour est maintenant repare (`st_make_valid()`) en Lambert-93
    avant l’union, et un echec produit un avertissement au lieu d’un
    `NULL` muet.
2.  **Le cœur echoue selon l’etat du processus.** Avec `nemeton`
    0.198.0, `segment_houppiers()` fait echouer lidR (`dalponte2016()`
    -\> `st_crop()`) des que le CHM est recadre. Il echoue meme sans
    emprise des que `load_project()` a tourne dans le meme processus, ce
    qui est toujours le cas au calcul. Dans un processus neuf et sans
    emprise, il reussit (mesure 5 fois). L’app appelle donc le cœur
    **dans un processus R neuf**
    ([`callr::r()`](https://callr.r-lib.org/reference/r.html), le CHM
    transmis par son chemin), sans emprise. Elle garde ensuite les
    houppiers qui **touchent** l’emprise, entiers, sans les decouper.
    Brief au cœur :
    `briefs/vers-nemeton/2026-09-23-houppiers-aoi-etat.md`.

Mesure sur « Reconfort » (25 dalles LiDAR HD a 0,5 m) : 280 218
houppiers segmentes en 185 s, 85 300 gardes dans l’emprise. Les 11
GeoPackages du lot portent desormais `parcelle` + `houppier`, par
exemple 7 321 houppiers pour la parcelle 1116.

**Pour en profiter**, il faut recalculer les indicateurs du projet : les
houppiers sont precalcules a ce moment-la, pas au telechargement.

Hors correctif, deux points s’expliquent par la conception : le fond
orthophoto n’est **jamais** exporte (Marculus prendrait une table de
tuiles pour son fond hors ligne, et les orthos du projet pesent des
gigaoctets), et la couche `parcelle` de chaque GeoPackage contient les
parcelles cadastrales **de l’UGF du chantier**, pas le parcellaire
entier du projet.

Tests : l’emprise d’un contour croise, l’appel sans emprise suivi de la
selection (houppier a cheval garde entier), et l’aiguillage entre
processus neuf (CHM sur disque) et processus courant (CHM en memoire).
Les mutations sont detectees.

#### Added — supprimer les actions selectionnees du Plan d’actions

Jusqu’ici, aucune action ne pouvait etre supprimee depuis l’app. Un
bouton « Supprimer la selection » apparait desormais dans la barre
laterale droite du Plan d’actions, sous « Ajouter une action ». Il
supprime les lignes selectionnees dans le tableau des actions. La
selection peut aussi venir d’un clic sur une UGF de la carte, qui
selectionne ses actions.

- Une modale de confirmation liste les actions visees : UGF, type,
  annee, avec au plus dix lignes suivies de « … et N autre(s) ». La
  suppression ne se fait qu’au clic sur le bouton rouge « Supprimer ».
- Les identifiants sont figes a l’ouverture de la modale. Un clic sur la
  carte pendant qu’elle est affichee ne change donc pas ce qui sera
  supprime.
- Chaque action supprimee laisse sa propre entree `delete` dans l’audit
  du plan, via le nouveau helper `delete_actions_from_plan()`. Celui-ci
  ignore les identifiants deja absents au lieu d’interrompre le lot.
- La garde de lecture seule (role ou verrou du projet) s’applique a
  l’ouverture de la modale ET au moment de la confirmation.
- Apres suppression, la selection est videe, dans le tableau comme en
  surbrillance sur la carte.

Couleurs conformes a la charte : `btn-outline-danger` dans la barre
laterale (action auxiliaire de prudence), `btn-danger` pour la
confirmation. Sept nouvelles cles i18n `action_plan_delete_*`. Cinq
tests : deux pour le service, trois pour le module (position et classe
du bouton, suppression effective, absence de selection et lecture
seule). Les deux mutations (garde retiree, suppression sans effet) sont
detectees.

## nemetonshiny 0.144.0 (2026-09-23)

#### Fixed — la note S4 `dbDataType` ne s’affiche plus a la sauvegarde en base

A la premiere sauvegarde d’un projet dans PostGIS, la console affichait
: « Note : methode avec la signature ‘DBIObject#sf’ choisie pour la
fonction ‘dbDataType’, signature cible ‘PqConnection#sf’ ». Ce n’est pas
une erreur : `sf` et `RPostgres` definissent tous deux cette methode, et
R signale une fois par session laquelle il retient. Le choix est le bon
(celui de `sf`, qui type la geometrie). Le
[`sf::st_write()`](https://r-spatial.github.io/sf/reference/st_write.html)
de `db_save_parcels()` est desormais entoure de
[`suppressMessages()`](https://rdrr.io/r/base/message.html), puisque
`quiet = TRUE` ne couvre pas un message du dispatch S4. Un test simule
la note ; sans le correctif, il echoue.

Les deux « Avis : \[minmax\] min and max values not available » qui
l’accompagnaient viennent du cœur, et un brief lui est adresse
(`briefs/vers-nemeton/2026-09-23-reconfort-include-range-inerte.md`).
Avec `include_range = TRUE`, `reconfort_cache_manifest()` appelle
[`terra::minmax()`](https://rspatial.github.io/terra/reference/minmax.html)
sans `compute = TRUE` sur des GeoTIFF iota2 qui ne stockent pas leurs
statistiques. Le calcul renvoie `NaN` et le manifeste garde les bornes
statiques. Sur `ltcp` zone 9, le score est donc affiche sur 1-100 au
lieu de sa plage reelle 24-58. Rien a changer cote app : elle passe deja
`include_range = TRUE`.

## nemetonshiny 0.143.31 (2026-09-23)

#### Changed — la « Carte FAST » garde son stack d’indice sur disque (cœur 0.198.0)

Suite de la 0.143.29 : l’app ne reconstruisait plus le stack en revenant
sur l’onglet, mais le payait encore a chaque nouvelle session et a
chaque changement d’indice, y compris pour revenir a un indice deja
affiche. Le cœur 0.198.0 met ce stack en cache (reponse au brief du
2026-09-23). La Carte FAST appelle maintenant
`build_index_stack(..., cache_result = TRUE)`. Le cache se trouve dans
`<projet>/cache/layers/index_stack`, emplacement par defaut du cœur, et
compte au plus 8 stacks d’environ 230 Mo chacun.

Mesure sur `armn` (327 scenes, NDVI, cache ecrit dans un repertoire
temporaire) : **39,5 s** au premier appel (calcul + ecriture, cache
disque froid), **0,22 s** au second, avec un resultat identique (noms,
dates, attribut `index`, valeurs).

`parallel` reste a `FALSE`, sur recommandation du cœur : sans plan
multisession permanent, furrr tourne en sequentiel et n’ajoute que le
cout de `wrap`/`unwrap`.

Les masques FAST sont desormais nommes par leur contenu, cote cœur :
deux calculs differents ne peuvent plus ecraser le meme fichier, et un
calcul identique reutilise le fichier existant. Le commentaire de
`.compute_fast_mask()` est mis a jour en consequence. L’app ne lisait
aucun nom de masque ; rien d’autre ne change.

Plancher `Imports: nemeton (>= 0.198.0)`. Deux tests : le contrat de
signature du cœur, et l’appel avec `cache_result = TRUE` sans `parallel`
(echoue si l’argument est retire).

#### Fixed — le job CI `coverage` echouait sur deux des gardes de la 0.143.30

`coverage` ne demarre que si `R-CMD-check` passe ; il etait donc saute
depuis le 14 septembre. Reveille par la 0.143.30, il echouait sur
`test-app_ui` et `test-service_python`. Leur garde testait l’existence
du dossier `../../R`. Sous covr, ce dossier existe, mais c’est celui du
paquet installe : il ne contient que `nemetonshiny.rdb`, aucun `.R`. Les
deux tests comptaient alors zero fichier et echouaient. Ils sont
maintenant sautes quand aucune source `.R` n’est trouvee. Verifie dans
trois situations : sources presentes (566 reussites), dossier absent et
dossier installe (0 echec, tests sautes).

## nemetonshiny 0.143.30 (2026-09-23)

#### Fixed — `R-CMD-check` reste rouge : cinq tests lisaient `R/` sans garde

La 0.143.29 a remis le job `tests` au vert, mais `R-CMD-check` echoue
toujours, et ce depuis le 14 septembre (`0fc7a5e1`), pas depuis le
plancher cœur 0.197.0 comme l’affirmait l’entree precedente. Les echecs
`normalize_indicator` s’y ajoutaient seulement.

La cause restante : cinq tests lisent les sources `R/*.R`
(`test-mod_ug`, `test-service_pipeline`, `test-service_tour`,
`test-app_ui`, `test-service_python`). Sous `R CMD check`, les tests
tournent contre le paquet installe, et ce repertoire n’existe pas. Ils
prennent maintenant la garde deja utilisee ailleurs dans le repo
(`skip_if_not(file.exists(...), "sources R absentes")`). Verifie dans
les deux cas : 566 reussites avec les sources, 0 echec et les cinq tests
sautes sans elles.

Deux caracteres non-ASCII hors commentaire (`R/mod_home.R`,
`R/mod_pipeline.R`) passent en `\uXXXX`, ce qui leve l’avertissement
correspondant du check. L’avertissement sur `lidR` et `opencanopy`,
appeles sans etre declares dans DESCRIPTION, reste ouvert.

## nemetonshiny 0.143.29 (2026-09-23)

#### Fixed — la Carte des actions n’etait pas cadree sur le projet

Apres un changement de projet, arriver sur « Plan d’actions » montrait
le monde entier, les UGF reduites a un point. Meme mecanisme que la
carte cadastrale (0.143.25). Le changement de projet se fait depuis un
autre onglet : la carte est alors masquee (`display: none`), donc ses
dimensions sont nulles. Le `fitBounds` qu’elle recevait y cadrait le
monde. La signature de bbox etait pourtant enregistree, si bien que rien
ne recadrait au retour.

Quand la carte est masquee, le cadrage est desormais **differe** au lieu
d’etre perdu. A l’arrivee sur l’onglet principal ET sur le sous-onglet «
Carte + Tableau », la carte recupere ses dimensions (`invalidateSize`),
puis le cadrage en attente s’applique. S’il n’y a aucun cadrage en
attente, la vue choisie par l’utilisateur est conservee.

Trois tests (dont deux `testServer`), verifies par mutation.

#### Fixed — la CI `R-CMD-check` etait rouge depuis le plancher cœur 0.197.0

Sept tests de `normalize_indicator` verifiaient encore les anciennes
bornes d’E1 (0,3) et d’E2 (0,75). Le cœur v0.197.0 les a alignees sur
1,32 (spec 048 §11), et le plancher `nemeton (>= 0.197.0)` date de
0.143.25 : les releases 0.143.25 a 0.143.28 sont donc parties avec un
`R-CMD-check` rouge (`release.yml` n’en depend pas). Tests mis a jour,
aucun code touche.

#### Fixed — revenir sur « Suivi sanitaire » ne relance plus les calculs

Chaque arrivee sur l’onglet relancait les deux calculs du Suivi, meme
quand rien n’avait change depuis la visite precedente : le toast «
calcul en cours », puis une interface figee. La cause est la meme dans
les deux modules. L’onglet actif fait partie des dependances de leur
observateur (c’est le garde qui evite de calculer depuis l’Accueil),
mais rien ne verifiait si les entrees avaient bouge.

Mesure sur le projet `armn` (327 scenes Sentinel-2, cache chaud) :

- **Carte FAST** : `build_index_stack()` reconstruisait le stack a
  chaque entree, **~9 s** bloquantes (8,75 / 9,20 / 9,37 s). Rien ne le
  met en cache.
- **Alertes FAST** : `compute_fast_alert_mask()` prenait **~0,12 s**,
  mais il reecrivait un masque, relancait le toast et repeignait la
  carte.

Chaque module garde maintenant la signature des entrees de son dernier
calcul reussi : projet, zone, dates, seuils, indice, mode, parametres de
tendance et rafraichissement pour les alertes ; cache, scenes et indice
pour la carte. Revenir sur l’onglet avec la meme signature ne declenche
plus rien. Un echec n’est pas memorise : le calcul est retente a
l’entree suivante. Tout changement d’entree recalcule comme avant, et le
calcul reste differe tant que l’onglet n’est pas ouvert.

Le commentaire de `.compute_fast_mask()` est corrige au passage : il
affirmait que le resultat etait reutilise sans recalcul. C’est faux pour
le masque 0-4, que le cœur reclasse et reecrit a chaque appel ; seul le
raster continu intermediaire est en cache.

Quatre tests (`testServer`) couvrent l’aller-retour d’onglet sans
recalcul, le recalcul sur changement d’indice, et la nouvelle tentative
apres un echec. Sans le correctif, ils echouent.

## nemetonshiny 0.143.28 (2026-09-19)

#### Fixed — le tremblement de l’etape « Plan d’action »

Derniere piece du tour guide, et la plus instructive : une boucle de
retroaction entre driver.js et bslib, mesuree dans l’app reelle.

La sidebar du Plan d’actions fait **793 px de haut dans une fenetre de
900**. driver.js pose son popover A COTE de l’element cadre ; face a un
element presque aussi haut que la fenetre, il n’a plus la place et le
sort de l’ecran (`top = -24`). La page se met alors a osciller entre
**avec** et **sans** barre de defilement — mesure :
`innerWidth - clientWidth` alterne 0 / 15 px, ~25 fois en 3,5 s. Chaque
bascule reveille le `ResizeObserver` de bslib, qui redispatche un
`resize`, que driver.js ecoute pour se recadrer. La boucle s’entretient
seule : **326 evenements en 6,4 s**, le cadre oscillant de 1 a 2 px et
le popover de 15 px. C’est exactement le tremblement signale, et il ne
touchait QUE cette etape (mesure comparative : 0 evenement sur toutes
les autres ancres).

L’etape est desormais ancree sur la carte « Tableau des actions »
(322x641) et non sur la sidebar qui la contient, avec
`position = "left"` : le popover se cale a gauche de la carte au lieu
d’etre pousse hors champ. Meme mesure apres correctif : **0 evenement**,
geometrie strictement stable, popover entierement visible. Sur le tour
complet, le compteur global tombe de ~130 a 60.

Regle a retenir, consignee dans `service_tour.R` : **une ancre de tour
ne doit pas remplir la fenetre**. `action_table_card()` gagne un
parametre `card_id` optionnel pour permettre d’ancrer la carte entiere —
en-tete compris, donc au-dessus du voile.

## nemetonshiny 0.143.27 (2026-09-19)

#### Fixed — le tour guide entrait dans des onglets que l’app lui interdit

Suite du correctif 0.143.26. Une fois le tour reste vivant, deux etapes
sur onze restaient muettes, et l’onglet vise s’affichait avant de sauter
en arriere — « ca tremble puis ca reste bloque ». Les deux symptomes ont
la meme racine, mesuree dans l’app reelle pilotee en Chrome sans tete.

**L’app interdit Synthese et les familles tant que le projet n’est pas
`completed`** : `app_server` renvoie alors la navigation sur l’Accueil.
Le tour auto-demarre precisement dans cet etat (visiteur sans projet) et
proposait malgre tout ces deux etapes. Il declenchait donc son propre
renvoi : l’onglet s’affiche, l’aller-retour serveur le ramene sur
l’Accueil, et le cadre saute. Le predicat de restriction vit desormais
dans `service_tour.R` (`.tab_requires_completed_project`) et sert **aux
deux** cotes — la garde de navigation ET le filtre des etapes — de sorte
que les deux listes ne peuvent plus diverger. Sans projet termine, le
tour couvre l’Accueil, le Plan d’action, le Terrain et le Suivi ; les
deux etapes reviennent des que le projet est calcule.

**Une ancre de tour doit etre STATIQUE.** Present dans le DOM ne suffit
pas : un `uiOutput` porte par un onglet jusque-la masque est SUSPENDU
par Shiny — il ne rend rien avant un aller-retour serveur. Il mesure
donc 0 de haut a l’instant ou driver.js cadre l’etape, `canHighlight()`
est faux, et driver saute l’etape **en silence** : le popover reste sur
l’etape precedente, ce que l’utilisateur lit comme un tour bloque.
Mesure a l’instant du cadrage : `synthesis-project_summary` = 447x0 et
`famille_carbone-maps_row` = 1408x0, la ou une carte ou une sidebar
statique donne sa vraie geometrie. Les deux ancres deviennent donc
`synthesis-summary_card` (la carte « Synthese du projet ») et
`famille_carbone-family_header` (l’en-tete de famille). Un test
verrouille la regle pour toutes les ancres : aucune ne doit etre un
conteneur `shiny-html-output`.

Verification, meme scenario qu’avant/apres dans l’app reelle : 9 etapes,
`isActivated` vrai de bout en bout, chaque cadre colle a son ancre
(stage = element + 10 px), plus aucun retour d’onglet, et fin de tour
propre (voile et cadre retires, popover masque).

## nemetonshiny 0.143.26 (2026-09-19)

#### Fixed — le tour guide mourait a la deuxieme etape

Deux defauts distincts, tous deux visibles des l’ouverture du tour.

**1. Plus rien n’etait cliquable apres « Suivant ».** driver.js
(embarque dans cicerone) ecoute les clics sur `window` : des qu’un
element est mis en avant, tout clic hors du popover et hors de cet
element ferme le tour (`allowClose` vaut TRUE). Or la bascule d’onglet
du tour passe par un clic synthetique sur le lien de nav, emis
**pendant** `on_highlight_started` — donc alors que l’etape precedente
est encore l’element courant. driver appelait `reset()`, puis la suite
de `highlight()` reaffichait quand meme popover et cadre : l’etape 2
s’affichait, mais `isActivated` etait repasse a FALSE et ni « Suivant »,
ni « Fermer », ni les fleches du clavier ne repondaient plus. La
premiere etape y echappait seulement parce qu’aucun element n’etait
encore mis en avant. Le clic est desormais etouffe au niveau de
`document`, **apres** le handler delegue de Bootstrap : l’onglet bascule
normalement, l’evenement n’atteint jamais driver.js. Mesure dans Chrome
: `isActivated` reste TRUE a l’etape 2, l’onglet bascule, et la
re-mesure `resize` (qui exige `isActivated`) redevient effective.

**2. Le titre « Rechercher une commune… » restait sous le voile.**
L’etape etait ancree sur `home-search_collapse`, le corps repliable
**seul** : l’en-tete de la carte ne faisait pas partie de l’element mis
en avant et restait donc assombri. L’ancre est desormais
`home-search_card`, la carte entiere.

## nemetonshiny 0.143.25 (2026-09-18)

#### Changed — trois echelles changent cote cœur : L1, T1, E1/E2 (spec 048)

Implemente le brief `2026-09-18-l1-sens-inverse.md`. Plancher
`Imports: nemeton (>= 0.197.0)`.

Le brief est clair sur le perimetre app : **aucune logique metier a
ecrire**. Une chose a faire, une a ne surtout pas faire, une a dire.

**A faire — invalider.** `INDICATOR_SENSE_VERSION` passe de 2 a 3. Un
`indicators.parquet` calcule avant reste parfaitement LISIBLE : memes
colonnes, memes types, aucune erreur au chargement.
`compute_all_indicators()` le relirait donc, constaterait le travail
fait, et sauterait le recalcul en propageant des `famille_paysage`,
`famille_temporelle` et `famille_energie` faux. L’invalidation unique se
declenche a la premiere ouverture apres la montee de version.

**A ne pas faire — reinverser.** Le cœur rend deja L1 dans le bon sens ;
une inversion cote app annulerait la correction **en silence**. Verifie
: l’app n’inverse rien, et un test le gele — il balaie tout `R/` a la
recherche d’un `100 - <indicateur>` sur L1, T1, E1, E2 ou la famille
Paysage. Le piege est reel : les slugs de la famille L sont **croises**
(spec 045), et inverser `indicateur_l1_sylvosphere` retournerait le
**morcellement** sur les jeux non migres.

**A dire — et c’est ce qui manquait vraiment.** L’utilisateur voyait son
projet repasser en brouillon sans explication : le seul signal etait un
`cli` dans la console, que personne ne lit depuis l’interface.
`load_project()` porte desormais `indicators_invalidated` sur le SEUL
chargement qui vient de jeter le parquet perime, et un bandeau nomme les
trois familles qui changent en avertissant qu’une comparaison avec les
scores precedents n’aurait pas de sens.

Ce que l’utilisateur va constater : **Paysage baisse** sur les parcelles
morcelees ou bordees de bati ; **Ancienneté cesse d’etre saturee** (tout
ce qui depassait 100 ans valait 100, desormais 150 ans -\> 75) ;
**Energie baisse** sur les peuplements ordinaires et s’aligne sur P1 —
E1, E2 et P1 doivent afficher la meme valeur sur une parcelle donnee.
`N3`, `famille_naturalite`, `L2` et `L3` sont inchanges ; si `N3` bouge,
c’est qu’une double inversion s’est glissee quelque part.

#### Fixed — les puces du message d’invalidation etaient concatenees

`cli_alert_warning()` concatene un vecteur au lieu d’en rendre les
puces. Seul `cli_warn()` rend les `i =` sur des lignes distinctes.

#### Fixed — la carte cadastrale ne se recadrait plus au retour d’onglet

Basculer sur « Carte UGF » puis revenir sur « Carte cadastrale »
laissait la carte decentree du projet.

`navset_card_tab` masque les panneaux en `display: none` : la carte
leaflet y a des dimensions **nulles**. Tout ce qui lui arrive pendant ce
temps — proxy, polygones, recadrage — s’applique a un conteneur de
taille zero, et au retour la vue reste fausse. La carte UGF traitait
deja son cas (`mod_ug.R:929`) ; la carte cadastrale, non :
**`input$main_tabs` n’etait observe nulle part**.

`mod_map_server()` prend desormais un `active_tab` (defaut `NULL`, les
appelants existants sont preserves) et, au retour sur l’onglet, redonne
ses dimensions a la carte **puis la recadre**. Les deux comptent :
`invalidateSize()` seul restaure la taille, pas la vue.

Le cadrage vise les **parcelles du projet** d’abord, la commune a
defaut, et ne fait rien s’il n’y a ni l’une ni l’autre — un `sf` vide
compte comme absent. Cette decision sort en helper pur
`.map_bbox_recadrage()` : l’observateur vit derriere un
[`later::later()`](https://later.r-lib.org/reference/later.html) et un
proxy leaflet, la decision se teste sans rien de tout cela.

Tests : 3 cas, verifies par mutation. Trois mocks de `mod_map_server`
dans `test-05mod_home.R` ont suivi la signature — un mock qui ment sur
le contrat qu’il imite ne protege rien.

#### Fixed — le tour guide cadrait a cote, surtout a la premiere ouverture

Deux causes, toutes deux des mesures prises trop tot.

**A chaque changement d’onglet.** `.tour_switch_tab_js()` cliquait le
lien de nav et laissait driver.js cadrer dans la foulee. Or cliquer
ACTIVE l’onglet, il ne le MESURE pas : un `.tab-pane` masque est en
`display: none`, ses elements ont des dimensions **nulles** jusqu’a ce
que le navigateur l’ait pose, et Bootstrap 5 ajoute une transition
`.fade` par-dessus. Le cadre visait donc une geometrie qui n’existait
pas encore — sur la plupart des etapes.

On attend desormais `shown.bs.tab`, emis par Bootstrap **apres** la
transition, puis on force une re-mesure via un evenement `resize` :
driver.js l’ecoute deja (`bind()` -\> `onResize()` -\> `refresh()`) et
ne re-mesure que si un tour est actif. Aucun interne de cicerone n’est
touche, donc rien a reprendre si le paquet evolue.

**A la premiere ouverture.** Le demarrage reposait sur un
`setTimeout(500)` aveugle apres `collapse('show')` — lui-meme **anime**.
La page se reorganisait sous le tour pendant qu’il se cadrait, et c’est
la premiere ouverture qui souffrait le plus, quand rien n’est en cache
et que tout arrive ensemble. Le declenchement est desormais pilote par
`shown.bs.collapse` sur les deux sections.

Les deux correctifs portent un **repli temporise**, et ce n’est pas de
la prudence decorative : une section (ou un onglet) **deja ouverte
n’emet aucun evenement**, et l’attente ne se resoudrait jamais — le tour
ne demarrerait plus du tout. Une garde d’idempotence evite que repli et
evenement declenchent la re-mesure deux fois.

Tests : 3 cas, verifies par mutation. La premiere version de l’assertion
etait **vacante** — elle cherchait la chaine « shown.bs.tab », qui
figure aussi dans le `removeEventListener`, et passait donc en retirant
l’ecoute. Elle vise maintenant `addEventListener('shown.bs.tab'`.

## nemetonshiny 0.143.24 (2026-09-18)

#### Added — le verdict « CHM suspect » du cœur est enfin lu

`nemeton` (\>= 0.191.1) estampille `attr(x, "chm_suspect")` sur la
sortie de `segment_houppiers()` : « ce modele de hauteur est
vraisemblablement une prediction ratee se faisant passer pour une coupe
rase ». Il le disait **dans le vide** — `grep chm_suspect R/` ne rendait
rien.

- Le verdict est **capture avant le sous-ensemble `sf`**, qui detruisait
  les attributs, puis persiste dans les metadonnees du projet.
- **Le cas VIDE le porte aussi** : zero houppier + CHM suspect est
  precisement la combinaison qui doit remonter, et c’etait celle qui
  disparaissait dans un `NULL` muet.
- **Un verdict negatif est ecrit comme un positif** : sans cela, un
  projet anciennement suspect garderait son bandeau apres correction du
  CHM.
- Un bandeau d’avertissement s’affiche dans la Synthese, nommant
  l’indicateur fausse (P1, volume bois) et la hauteur maximale du
  modele.
- L’ecriture est best-effort : un calcul d’indicateurs ne meurt pas
  parce que son verdict n’a pas pu s’ecrire.

**Portee reelle, et elle est plus etroite que prevu** : le garde-fou ne
mord que sur un projet **sans couverture LiDAR**. Avec du LiDAR,
`resolve_project_chm(validate = .chm_exploitable)` a deja ecarte l’ortho
plate en amont — c’est pour cette raison que le projet « Fordead » est
en `chm_source: lidar_hd` et n’affiche aucun faux zero. Sans repli
possible, en revanche, l’app montrait un volume nul comme s’il
s’agissait d’une mesure, et rien ne distinguait « il n’y a pas d’arbres
» de « le modele n’en a pas vu ».

#### Added — `R/service_python.R` : un registre, un runner, une regle

Quatre stacks Python cohabitent dans l’app (opencanopy, FORDEAD,
RECONFORT, rvt-py) et `reticulate` lie un interpreteur **une fois par
processus**, sans jamais le relier. Aucun reglage global ne peut les
servir tous, parce que leurs exigences se **contredisent** : opencanopy
veut `RETICULATE_PYTHON` epinglee sur son env conda, FORDEAD la veut
**absente** — le coeur le documente
(`nemeton/R/fordead_python.R:336-343`), une variable definie « ecrase
silencieusement `use_python()` / `use_virtualenv()` meme avec
`required = TRUE` ».

La seule reponse qui les concilie est architecturale, et l’app la
pratiquait deja sans la nommer : **un moteur = un processus,
interpreteur epingle a la creation**. Elle est desormais ecrite,
outillee et testee.

- **`engine_python(moteur)`** — registre `.PYTHON_ENGINES` moteur -\>
  interpreteur, en remplacement de `.resolve_opencanopy_python()` qui ne
  savait resoudre qu’Open-Canopy. Ordre : option explicite, variable
  d’env, env conda, balayage des racines d’installation. Rend `NA` sur
  un moteur inconnu **sans lever** : un moteur absent est un mode
  degrade, pas un crash.
- **`run_with_python(moteur, func, args, on_line)`** — la forme
  generique du sous-processus `callr` que le chemin CHM pratiquait
  depuis la spec 005. `R_ENVIRON_USER = ""` n’y est pas decoratif : sans
  lui, une `RETICULATE_PYTHON` restee dans le `.Renviron` de
  l’utilisateur ecraserait l’epinglage qu’on vient de poser, et l’enfant
  lierait le mauvais interpreteur **en annoncant le bon**.
- **Un test gele l’etat connu** : toute NOUVELLE liaison `reticulate` en
  processus fait echouer `test-service_python.R`, en nommant le fichier
  fautif. Seul `service_rvt.R` est sur la liste, avec la raison ecrite
  (il tourne dans un worker `future` ou aucun autre moteur Python ne
  tourne).

`.run_opencanopy_chm()` passe par le runner generique. Son repli **en
processus** ne s’emprunte plus que si l’isolation n’a pas pu avoir lieu
du tout (env introuvable, `callr` absent) — pas si le pipeline lui-meme
a echoue, auquel cas le rejouer en session echouerait pareil en abimant
la session au passage.

Les trois tests de resolution migrent vers `engine_python()` : le
contrat a change de nom et de place, il n’a pas disparu.

#### Note — d’ou vient ce chantier

D’un echec en direct : un script lance depuis cette session s’est heurte
a `reticulate is already bound to a different Python` (un env **uv
ephemere**, que `reticulate` s’attribue quand rien n’est configure).
C’est tres probablement aussi l’explication du «
`ModuleNotFoundError: No module named 'torch'` non reproductible » que
le brief `opencanopy` du 2026-08-27 signalait au §6 sans pouvoir
l’expliquer : ce n’est pas un etat de session mysterieux, c’est
`reticulate` qui gagne la course a la liaison.

## nemetonshiny 0.143.23 (2026-09-17)

#### Changed — RECONFORT s’arrete vraiment : la garde de version tombe

`nemeton` **v0.196.0 est publiee** (tag du 2026-09-14 15:14 ; `pak`
resout bien `@*release` vers elle). Les deux gestes annonces en 0.143.21
sont faits :

- la garde [`formals()`](https://rdrr.io/r/base/formals.html) qui
  n’envoyait `cancel_path` que si le coeur installe l’acceptait
  **disparait**, avec son commentaire. L’argument est passe directement,
  comme pour FAST et FORDEAD ;
- le plancher passe a `Imports: nemeton (>= 0.196.0)`.

Ce que ca change a l’usage : jusqu’ici l’argument etait **omis en
silence** sur un poste en 0.195.0 — le bouton « Arreter » de RECONFORT
ecrivait bien son flag, mais personne ne le lisait. L’arret cooperatif
devient effectif.

Verifie de bout en bout : coeur installe en 0.196.0, `cancel_path`
present dans ses `formals`, plancher satisfait, `ExtendedTask`
construite.

## nemetonshiny 0.143.22 (2026-09-17)

#### Fixed — un moteur Sante annule ne passe plus pour une reussite

Le coeur rend `status = "cancelled"` **sans lever** (`monitoring.R:489`,
`fordead_pipeline.R:812`, `reconfort_pipeline.R:862`). Pour une
`ExtendedTask` c’est donc un `"success"`, et la chaine enregistrait **«
ok »** pour une etape que l’utilisateur venait d’arreter : le rapport
final annoncait une reussite la ou rien n’avait ete produit.

Defaut **preexistant sur FAST et FORDEAD** ; etendu a RECONFORT par le
cablage de `cancel_path` en v0.143.21. Corrige pour les trois d’un coup,
l’observateur etant commun. La decision sort en helper pur
`.sante_pipeline_statut()` — elle se teste sans session ni
`ExtendedTask`, la ou l’observateur en manipule trois.

#### Added — les rejets du curseur de chaine ne sont plus muets

Le protocole en tete de `service_pipeline.R` nomme son propre mode de
defaillance : un module qui ne repond pas « bloque la chaine sur cette
etape, sans rien afficher a l’utilisateur ». Les trois rejets de
`pipeline_record()` etaient exactement dans ce cas — silencieux.

Ils avertissent desormais, en nommant l’etape fautive ET le curseur
attendu :

- **reponse hors run** — etape absente de la selection ;
- **reponse hors de son tour** — le resultat est enregistre mais le
  curseur RESTE en place ; c’est le cas le plus traitre, la chaine
  parait avancer alors qu’elle est bloquee ;
- **seconde reponse** — une decision deja prise ne se rejoue pas.

Le chemin nominal reste **silencieux** : un avertissement de plus y
noierait les trois autres. Un test le verrouille.

Constate sur Aumur le 2026-09-16 : chaine figee sur une etape alors que
la synthese, les 12 commentaires de famille et le plan de 15 actions
avaient tous abouti. L’etat de chaine vivant en memoire, il ne restait
rien a lire apres coup. Ces avertissements sont la trace qui manquait —
la persistance de l’etat reste a faire.

#### Added — l’etat de la chaine est ecrit sur disque

`data/pipeline_state.json`, a cote de `progress_state.json`, reecrit a
**chaque transition**. Il nomme l’etape courante, l’index, et le statut
de chacune des etapes avec ses horodatages :

``` json
{ "index": 2, "total": 3, "current_step": "desserte", "done": false,
  "steps": [ { "id": "indicateurs", "status": "ok",      ... },
             { "id": "desserte",    "status": "running", ... },
             { "id": "ia_plan",     "status": "pending", ... } ] }
```

C’est exactement ce qui manquait le 2026-09-16 : la chaine d’Aumur figee
sur une etape, tout le travail abouti, et **rien a lire** pour savoir
laquelle. L’etat ne vivait qu’en memoire.

Trois choix qui comptent :

- **Un setter unique.** Les quatre transitions passaient par une
  assignation directe de `rv$state` ; elles passent maintenant par
  `.poser_etat()`, qui assigne ET persiste. Il aurait suffi d’un oubli
  pour que le journal mente. Un test verrouille l’absence d’assignation
  directe.
- **Best-effort, par construction.** Un run ne doit pas mourir parce que
  son journal n’a pas pu s’ecrire (disque plein, projet en lecture
  seule, dossier efface). L’echec est signale une fois, la chaine
  continue. Symetriquement, un journal corrompu rend `NULL` avec un
  avertissement : il ne devient pas un second incident.
- **Ce n’est PAS un format de reprise.** La chaine ne se reprend pas, et
  relire ce fichier ne relance rien. Il sert au diagnostic, et le dit.

Les fonctions vivent dans `service_pipeline.R` (regle
[\#2](https://github.com/pobsteta/nemetonshiny/issues/2)) ; la
serialisation est separee de l’ecriture (`pipeline_state_payload()`),
donc testable sans toucher au disque.

## nemetonshiny 0.143.21 (2026-09-14)

#### Added — RECONFORT s’arrete pour de vrai

`nemeton` 0.196.0 donne un `cancel_path` a `run_reconfort_dieback()`. Le
bouton « Arreter » de RECONFORT ecrit desormais `reconfort_cancel.flag`,
que le coeur scrute **aux frontieres de phase** : IOTA2 decoupe cote
Python, il n’y a pas de point d’arret plus fin. La phase en cours va
donc a son terme — une classification peut demander plusieurs dizaines
de minutes — puis le run sort avec `status = "cancelled"`, workdir
conserve et relisible par une relance.

Les trois moteurs Sante (FAST, FORDEAD, RECONFORT) s’arretent maintenant
de la meme facon. L’asymetrie signalee en v0.143.19 est levee.

- **Le bouton ne disait rien** ; il dit maintenant « **arret demande**
  », pas « arrete ». Ce sont deux moments distincts, et les confondre
  reproduirait en plus discret le bouton menteur que ce correctif
  supprime. Le toast final arrive avec l’evenement `reconfort:cancelled`
  ou le resultat, et partage son id pour remplacer le premier au lieu de
  s’empiler.
- **Le flag residuel est purge avant chaque lancement.** Sans cela, le
  garde-fou anti-« phantom cancel » du coeur verrait le flag present a
  l’entree, DESARMERAIT l’annulation, et le run suivant deviendrait
  ininterruptible.

#### Fixed — un run annule serait passe pour un succes

Le handler de resultat RECONFORT ne testait pas `result$status`. C’etait
sans consequence tant que le coeur ne rendait que `"completed"` (un
echec abortait) ; des le branchement de `cancel_path`, un run annule
serait entre dans la branche de succes, avec `n_alerts = NA` — donc un
`sprintf` sur `NA` — et un `$rasters` NULL passe au sous-module carte.
Une branche `cancelled` precede desormais.

#### Fixed — le chronometre du toast de succes affichait 0 depuis toujours

Le handler lisait `result$duration_sec` ; le coeur rend `elapsed_sec`
dans ses quatre formes de retour, et ce depuis bien avant 0.196.0.
`duration_sec` est le nom de FORDEAD, pas celui de RECONFORT. Bug
present, sans rapport avec l’annulation, corrige en passant.

#### Note — garde temporaire, et plancher NON bumpe

`nemeton` 0.196.0 **n’est pas encore releasee** : elle vit sur la
branche `feat/reconfort-cancel-path`, sans tag. Or `Remotes: @*release`
ne tire que les tags — un poste frais installerait 0.195.0, ou passer
`cancel_path` leverait « unused argument » et casserait tout le run.

`cancel_path` n’est donc transmis que si le coeur installe l’accepte
(`"cancel_path" %in% names(formals(...))`), meme idiome que le
`progress_callback` d’opencanopy. Le plancher reste
`nemeton (>= 0.195.0)`.

**A faire des que la release cœur est publiee** : retirer la garde et
son commentaire dans `service_monitoring.R`, bumper le plancher a
`(>= 0.196.0)`.

## nemetonshiny 0.143.20 (2026-09-14)

#### Removed — `mod_home_ui` etait defini deux fois, dont une morte

`app_ui.R` portait une copie de `mod_home_ui` intitulee « Placeholder »,
**masquee** par celle de `mod_home.R` (collation alphabetique : le
dernier charge gagne). Elle n’atteignait donc jamais l’ecran — tout en
restant lisible comme si elle etait la vraie, avec ses propres boutons «
OSM » / « Satellite » et un `uiOutput("map_placeholder")` que rien ne
rendait. Ces boutons avaient d’ailleurs survecu au passage au
LayersControl (v0.143.19) : personne ne les avait vus, puisqu’ils ne
s’affichaient pas.

Les 114 lignes partent. Un test verrouille l’unicite des trois
`mod_*_ui` definies dans ce fichier.

**Le piege inverse vit dans le meme fichier**, et il a failli couter
cher : `mod_synthesis_ui` et `mod_family_ui` portent le meme titre «
Placeholder » mais sont, elles, les implementations **VIVANTES** — elles
n’existent nulle part ailleurs. Les supprimer « par coherence » aurait
casse deux onglets. Le titre roxygen ne dit rien de l’etat reel d’une
fonction ; seule la collation le dit, et c’est ce que le nouveau test
mesure.

## nemetonshiny 0.143.19 (2026-09-14)

#### Fixed — un arret est un arret : le bandeau fantome de l’ingestion S2

« Arreter les calculs » (Tableau des actions) laissait a l’ecran le
bandeau `Tuile Sentinel-2 ... (37/396)` de `mod_monitoring`, chronometre
compris, qui continuait de compter bien apres l’arret. Il etait
**inclosable** : `duration = NULL` et `closeButton = FALSE`. Seul un
rechargement de page en venait a bout.

La cause n’etait pas une notification oubliee mais une notification
**recreee chaque seconde** : l’observer du chrono
(`invalidateLater(1000)`) republie `ingest_progress` tant que
`fast_run_start()` est non-NULL. Un `removeNotification()` seul aurait
ete annule une seconde plus tard — c’est `fast_run_start(NULL)` qui
coupe la source.

Fond du probleme : deux chemins d’annulation qui s’ignoraient.
`mod_home` observait `app_state$cancel_computation` seul ;
`mod_monitoring` n’en avait aucune connaissance
(`grep cancel_computation R/mod_monitoring.R` : zero occurrence).

#### Changed — `cancel_computation` devient LE signal d’arret de l’app

Les corps des trois handlers d’annulation de `mod_monitoring` sont
extraits en helpers (`.reset_fast_run()`, `.reset_fordead_run()`,
`.reset_reconfort_run()`), appeles a la fois par leur bouton d’onglet et
par un observer sur `app_state$cancel_computation`. Symetriquement, les
trois boutons posent desormais ce signal : arreter l’ingestion arrete
aussi la chaine.

- **Les helpers ne reposent JAMAIS le signal** — seuls les boutons le
  font. Sans cette regle, l’observer bouclerait sur lui-meme.
- **`fast_prewarm_progress` rejoint la liste des toasts effaces** : elle
  n’etait retiree que par l’event `fast_prewarm:complete`, donc un arret
  pendant le prechauffage la laissait a l’ecran par le meme mecanisme.
- **`mod_home` ne dit plus « Calcul annule » quand rien ne tournait** :
  le toast est desormais garde par `computing_project_id()`, sinon
  arreter une simple ingestion S2 aurait annonce l’annulation d’un
  calcul inexistant.

Asymetrie preexistante signalee au passage : RECONFORT n’ecrit aucun
`reconfort_cancel.flag` la ou FAST et FORDEAD en posent un. Son
annulation libere l’UI sans pouvoir interrompre le worker. Non corrige
ici — le coeur ne poll aucun flag de ce nom.

Tests : 2 nouveaux cas, verifies par mutation (neutraliser l’observer
partage fait tomber 9 assertions).

#### Changed — le choix du fond de carte rejoint le bouton « couches »

Onglet **Selection**, sous-onglets **Carte cadastrale** (`mod_map`) et
**Carte UGF** (`mod_ug`) : les deux boutons « OSM » / « Satellite » de
l’entete sont remplaces par le `LayersControl` natif de Leaflet, dans la
carte — le meme geste que partout ailleurs (FAST, FORDEAD, RECONFORT,
desserte, action plan, echantillonnage…).

Ce que ca supprime, au passage : l’ancien montage demandait **trois**
mecanismes pour ce que le controle natif fait seul — un `clearGroup()` +
`addProviderTiles()` via `leafletProxy`, un message JS maison
`toggleBasemapButtons` pour l’etat actif, et un `rv$basemap` cote
serveur. Ce dernier n’etait **jamais lu**, seulement ecrit, dans les
deux modules.

Deux details qui comptent :

- **`baseGroups` re-applique sa PREMIERE entree a chaque remontage du
  widget.** OSM reste donc en tete pour que le defaut ne change pas. Les
  deux rendus sont statiques (aucune lecture reactive), donc le choix de
  l’utilisateur survit.
- **Cote UGF, `baseGroups` est declare DEUX fois.** Le rafraichissement
  des couleurs de groupe fait `clearControls()` puis re-cree le controle
  : l’omettre la ferait disparaitre le choix du fond des la premiere
  mise a jour de legende.

#### Removed — code mort devenu orphelin

- `initBasemapToggle()` et le handler `toggleBasemapButtons`
  (`inst/app/www/js/custom.js`), plus les regles `.basemap-btn` /
  `.basemap-btn-active` (`custom.css`) : plus aucun code R ne produit
  ces elements ni n’envoie ce message.
- `rv$basemap` dans `mod_map` et `mod_ug`.

Tests : le contrat d’UI s’inverse (les boutons ne doivent PLUS y etre)
et deux tests serveur qui se terminaient par `expect_true(TRUE)` — donc
ne testaient rien — sont remplaces par une vraie inspection du widget
rendu (deux `addProviderTiles`, un `addLayersControl`, `baseGroups` et
leur ORDRE, les deux fournisseurs). Verifie par mutation : inverser
l’ordre des fonds fait tomber 2 assertions.

#### Changed — les etapes Sante de la chaine portent le nom des moteurs

Boite de dialogue « Lancer tous les calculs » :

| Avant                       | Apres               |
|-----------------------------|---------------------|
| Sante — surveillance rapide | **Sante — FAST**    |
| Sante — diagnostic FORDEAD  | **Sante — FORDEAD** |

Les deux autres etapes Sante nommaient deja leur moteur (RECONFORT,
creation des zones) : la liste devient homogene, et l’intitule
correspond a ce que l’utilisateur lit partout ailleurs dans l’onglet
Suivi sanitaire.

Cote anglais, `Health — rapid surveillance` et
`Health — FORDEAD diagnosis` suivent le meme alignement. Seules ces deux
entrees de `TRANSLATIONS` changent ; les cles
(`pipeline_step_sante_fast`, `pipeline_step_sante_fordead`) et leur
unique consommateur (`service_pipeline.R:76-77`) sont inchanges.

Les autres occurrences de « diagnostic FORDEAD » dans l’app sont de la
prose distincte (infobulles, messages d’erreur, nom de couche) et
gardent leur formulation.

## nemetonshiny 0.143.18 (2026-09-14)

#### Fixed — l’analyse IA ne depend plus d’un modele hors palier

`mistral-large-latest` etait le modele Mistral par defaut **depuis le
premier commit du paquet**, et l’analyse IA renvoyait
`HTTP 403 Forbidden. This model is not available in your subscription tier`.
Mesure contre l’API le 2026-09-14 : le modele existe toujours
(`GET /v1/models/mistral-large-latest` -\> 200), il est simplement ferme
aux paliers d’entree. Le defaut passe a `mistral-medium-latest`, son
successeur dans la gamme.

Le modele Anthropic par defaut, `claude-sonnet-4-5-20250929`, est
remplace par `claude-opus-5` : l’ancien identifiant est de generation
precedente, et l’API n’attend plus de suffixe de date sur les
identifiants courants.

#### Added — repli automatique de modele sur refus de palier ou de quota

Sur la meme cle, toute la gamme « premier » repond 403 ou 429 avec
`x-ratelimit-limit-req-minute: 0`, tandis que la famille ministral garde
du quota (750 / 188 / 30 req/min pour 3b / 8b / 14b). L’analyse IA
bascule desormais seule sur `ministral-14b-latest`, puis 8b, puis 3b, et
revient au modele configure des que le palier le redonne — sans
intervention.

Trois garde-fous :

- **Seuls 403 et 429 declenchent le repli.** Une cle invalide, une panne
  reseau ou un incident fournisseur remontent inchanges : reessayer avec
  un autre modele remplacerait le diagnostic par l’echec d’un second
  appel voue au meme sort.
- **Quand aucun repli n’aboutit, c’est l’erreur D’ORIGINE qui remonte**
  — le refus du modele configure, pas celui d’un remplacant que
  l’utilisateur n’a jamais choisi.
- **Un repli reussi se voit** (`ia_modele_repli`) : ces modeles « edge »
  sont nettement moins fins pour une analyse par profil expert, et une
  analyse degradee ne doit pas passer pour une analyse nominale.

La logique vit dans `R/service_llm.R` (regle
[\#2](https://github.com/pobsteta/nemetonshiny/issues/2) : pas de
logique applicative dans un `mod_*.R`) et s’accroche a
`create_llm_chat()`, donc les **six** sites d’appel en beneficient sans
etre touches — `$chat()` est la seule methode qu’ils utilisent sur
l’objet.

## nemetonshiny 0.143.17 (2026-09-04)

#### Changed — « Tableau des actions » contient enfin les actions

Le bloc s’appelait « Tableau des actions » depuis la v0.143.14 et n’en
contenait qu’une seule : « Tout calculer ». Les autres — « Voir les
resultats », « Reessayer », « Lancer le calcul » — flottaient
**au-dessus**, sans conteneur, alors qu’elles relevent de la meme
famille de geste.

Elles sont desormais **dans** le bloc, au-dessus du lancement de chaine
: elles dependent de l’etat du projet (brouillon / en erreur / calcule)
et sont donc les plus proches de ce que l’utilisateur vient de faire,
tandis que « Tout calculer » est toujours la et ferme la liste.
Consequence directe : elles suivent le repli du bloc, ce qui n’etait pas
le cas avant.

`mod_pipeline_ui()` prend un argument `actions_ui = NULL` — le bloc
porte le chrome (entete, repli, chevron), l’appelant garde SON
namespace. Le defaut `NULL` rend exactement l’ancien bloc, verrouille
par un test.

#### Changed — « Tout calculer » cede l’emphase pleine

Consequence du regroupement : deux boutons verts se touchaient dans le
bloc, « Voir les resultats » (`btn-success`) et « Tout calculer »
(`btn-primary`) — meme vert `#1B6B1B`, les deux classes ayant ete
fusionnees. La regle normative des couleurs dit une seule action
principale par vue.

C’est « Tout calculer » qui passe en `btn-outline-primary`, et deux
raisons concordent. D’abord l’usage : a l’etat `completed`, consulter
les resultats est le geste ATTENDU, relancer toute la chaine est
l’exception. Ensuite la mecanique, qu’on n’avait pas regardee — **ce
bouton ne lance rien**, il ouvre la modale de selection des etapes. Le
vrai CTA du lancement, c’est « Lancer la chaine » DANS la modale, et il
reste vert plein. Le niveau tertiaire etait donc le bon depuis le debut.

L’intention reste positive : la bordure porte le sens, ce n’est ni du
neutre (`outline-secondary`) ni de la prudence (`outline-danger`). Et
toujours pas d’ambre — elle signale une provenance, pas un niveau
d’action.

#### Fixed — Deux tests du chronometre tombaient sous charge

`.fmt_elapsed()` fait `as.integer(difftime(...))`, qui **tronque**. Deux
tests posaient `start <- Sys.time() - N` puis attendaient la valeur
exacte : juste seulement si moins d’**une seconde** s’ecoulait avant
l’appel. Machine chargee, la suite tombait sans qu’aucun code de
production soit en cause.

`.fmt_elapsed(start, now = Sys.time())` : l’horloge devient injectable,
le defaut laisse la production strictement inchangee (verifie par un
test dedie). Les roles sont separes — `test-utils-notif.R` verifie le
**formatage exact** sur une horloge figee, bornes de troncature
comprises (65,999 s -\> `01:05`, 66 s -\> `01:06`), assertions qu’on ne
pouvait pas ecrire avec une horloge qui bouge ; `test-mod_monitoring.R`
verifie seulement que le chrono est **cable** sur `start`. Mutation :
debrancher le chrono fait toujours tomber 5 assertions.

## nemetonshiny 0.143.16 (2026-09-03)

#### Added — Le log de l’enfant plafonne, pour savoir POURQUOI un run s’arrete

Complement direct de la v0.143.15. L’archivage du NDJSON sauvait la
trace structuree — *jusqu’ou* un run etait alle. Il manquait *pourquoi*
il s’etait arrete :
[`nemeton::run_memory_capped()`](https://pobsteta.github.io/nemeton/reference/run_memory_capped.html)
lancait l’enfant avec `stdout = ""`, c’est-a-dire « heriter du parent »,
et le parent est un worker `future` que `parallelly` demarre avec
`OUT=/dev/null`. Le traceback partait a la source.

Le cœur `0.195.0` accepte desormais `log_path`. Les **quatre** chemins
plafonnes de l’app le posent dans le repertoire projet :

| Chemin                    | Fichier                       |
|---------------------------|-------------------------------|
| Suivi sanitaire FORDEAD   | `data/fordead_child.log`      |
| Suivi sanitaire RECONFORT | `data/reconfort_child.log`    |
| Calcul des 31 indicateurs | `data/compute_child.log`      |
| Moteur de reGeneration    | `data/regeneration_child.log` |

Le nom est stable pour qu’on sache ou regarder sans chercher, et la
rotation a lieu au **demarrage** (`.prev-<horodatage>`, cinq conservees)
: le cœur gardant le fichier meme en cas de succes, sans rotation le run
suivant ecraserait la trace du precedent — le defaut meme qu’on venait
de corriger sur le NDJSON.

Garde de capacite sur
[`formals()`](https://rdrr.io/r/base/formals.html), comme pour
`package`/`options` ailleurs : sur un cœur anterieur l’argument est
retire de l’appel, qui redevient exactement celui d’avant.

Plancher `Imports: nemeton (>= 0.195.0)`, **et l’ordre a compte**. Bumpe
une premiere fois alors que `0.195.0` ne vivait que sur le `main` du
cœur, il a rendu l’app non-installable — `Remotes: @*release` ne resout
que les **tags**, et la derniere release etait `v0.194.0` :

``` R
* deps::.: Can't install dependency pobsteta/nemeton@*release (>= 0.195.0)
```

Remis a `0.193.0` (la garde de capacite rendant le bump facultatif),
puis retabli une fois `v0.195.0` publiee et
`pak::pkg_deps("…@*release")` verifie.

**Ce que ca donne en pratique.** Le run RECONFORT de Couchey qui
echouait depuis le 31 aout est alle au bout le 2026-09-03 a 21:19 —
10/10 phases, 14 min 48 s, les trois rasters masques 2025 produits.
Trois verrous devaient tomber ensemble : l’adoption des zones
(v0.143.12) qui a rendu les 203 scenes reprises du cache, la levee du
blocage CNES, et le redecoupage en 7 chunks du cœur 0.195.0 (pic 11 Go
au lieu de 11,5 fatals). Les correctifs de tracabilite (v0.143.15 et
celui-ci) n’ont pas fait passer le run — ils ont rendu les trois causes
visibles.

#### Changed

- `.prune_failed_traces()` devient `.prune_run_traces()` et prend un
  `motif` : deux familles de traces se bornent maintenant de la meme
  facon, les `.failed-<ts>` (NDJSON archive a l’echec) et les
  `.prev-<ts>` (log tourne).

## nemetonshiny 0.143.15 (2026-09-03)

#### Fixed — La trace d’un run en échec était effacée au moment où elle devenait utile

Projet **Couchey**, 2026-09-03 : RECONFORT échoue après **20 h 19** de
calcul (`exit 1`). Il ne restait rien pour comprendre — ni le NDJSON de
progression, ni le message d’erreur de l’enfant. Le diagnostic a dû être
reconstitué depuis les fichiers laissés par IOTA².

`.cleanup_progress_file()` était appelé à l’identique sur les trois
sorties — succès, annulation **et erreur** — et faisait
[`unlink()`](https://rdrr.io/r/base/unlink.html) sur le `.json` comme
sur le `.ndjson`. Or le NDJSON est la seule trace structurée d’un run :
une ligne par item, une par phase, du début à la fin. Sur le chemin
d’erreur, on détruisait la preuve à la seconde près où elle servait.

Les chemins d’**échec** (FAST, FORDEAD, RECONFORT) **archivent**
désormais au lieu d’effacer : `<fichier>.failed-<AAAAMMJJ-HHMMSS>`, dans
le répertoire projet, avec un message `cli` qui dit où. Cinq archives
conservées par fichier de base — borner évitait de remplacer un problème
de diagnostic par un problème de ménage. Succès et annulation effacent
comme avant : il n’y a rien à comprendre, et l’annulation est un geste
délibéré.

Le `.tmp` (artefact d’écriture atomique) est jeté dans tous les cas, et
un renommage refusé retombe sur la suppression — laisser l’original en
place ferait relire cette trace par le run suivant comme si elle était
la sienne.

**Ce que ça ne répare pas.** La sortie de l’enfant plafonné part dans le
`/dev/null` du worker `future`
([`nemeton::run_memory_capped()`](https://pobsteta.github.io/nemeton/reference/run_memory_capped.html)
hérite du parent), donc le *message* d’erreur reste perdu. Le NDJSON dit
jusqu’où on est allé, jamais pourquoi ça s’est arrêté. Correctif demandé
au cœur : `specs/BRIEF-nemeton-trace-enfant-plafonne.md`.

## nemetonshiny 0.143.14 (2026-09-02)

#### Changed — « Tout calculer » : le bloc, le bouton, et ce qui se passe au clic

Trois retouches de la section de lancement enchaîné, dans la sidebar de
l’onglet Sélection.

**L’entête du bloc ne répète plus le bouton.** Le bandeau et le bouton
juste en dessous portaient le même texte, « Tout calculer », à six
pixels d’écart. Le bloc s’appelle désormais **« Tableau des actions »**
(`pipeline_section_title`, FR/EN) : le bloc nomme ce qu’il regroupe, le
bouton nomme l’action.

**Le bouton se grise pendant la chaîne.** Il redevient cliquable à la
clôture, qu’elle vienne de la fin naturelle ou de l’arrêt manuel. Une
garde serveur double le grisage : un clic resté en vol au moment du
démarrage ne peut plus écraser le run en cours — ses réponses auraient
été rejetées sur le `run_id`, laissant une chaîne orpheline tourner sans
rien piloter.

**Un toast « Tous les calculs en cours… » s’affiche en bas à droite**
pendant toute la durée de la chaîne, et se retire à la clôture. C’est le
retour immédiat qu’exige la règle des actions longues ; le coin
bas-droite est celui de toutes les notifications de l’app depuis la
v0.32.0.

Le grisage et le toast sont pilotés par l’**état du run**, pas par le
clic sur « Tout calculer » : ce clic-là n’ouvre que la modale de
sélection des étapes, et griser dès l’ouverture aurait laissé un bouton
mort à celui qui annule la modale.

## nemetonshiny 0.143.13 (2026-09-02)

#### Fixed — Le moteur tournait sur 2018 / 2022 sans le dire, quand E-OBS était sauté

Constaté sur le projet **Lajoux**, où la détection E-OBS est
indisponible : l’étape s’affiche en *Sautée* avec la bonne consigne («
saisir les années manuellement »), puis le gel R7 et le moteur
s’affichent en **vert** — sans que rien n’indique sur quelles années ils
ont tourné.

`annees_pipeline()` n’est rempli que si l’étape E-OBS **réussit**.
Sautée, il reste `NULL`, et le `%||%` des deux lanceurs retombe sur les
champs du formulaire, dont les valeurs d’usine sont `2018` et `2022` :

``` r
year_moyenne  = annees$moyenne  %||% na_null(input$year_moyenne),
year_canicule = annees$canicule %||% na_null(input$year_canicule),
```

Un run pouvait donc décrire une année moyenne et une canicule sans
rapport avec le climat du projet, et être rapporté réussi. C’est
exactement le mode de défaillance annoncé à la livraison de la chaîne
(v0.143.0) — le câblage traitait le cas « E-OBS réussit » et n’avait
rien prévu pour « E-OBS est sauté ».

Les deux étapes consommatrices enregistrent désormais les années
qu’elles vont utiliser et **d’où elles viennent**. Quand elles
proviennent du repli, le rapport le dit : « Années non détectées : lancé
sur 2018 (moyenne) et 2022 (canicule), valeurs des champs. »

**On ne bloque pas** : quelqu’un a pu saisir ses années exprès, et un
résultat sur des années choisies vaut mieux qu’un refus. Ce qui manquait
n’était pas un garde-fou, c’était la visibilité.

`detectees` n’est vrai que si les **deux** années viennent d’E-OBS — une
seule suffirait à laisser passer un repli silencieux sur l’autre. Un
test le verrouille, vérifié par mutation.

## nemetonshiny 0.143.12 (2026-09-02)

#### Fixed — Le correctif des zones se sabotait à son premier usage

La v0.143.11 devait empêcher la recréation des zones de suivi. Au
premier lancement qui l’a suivie, sur Couchey, elle les a recréées quand
même : ids 49-52, et les **106 marqueurs de reprise** qu’on venait de
consolider à la main sont repartis en orphelins.

La cause est dans la garde elle-même. `.zones_a_jour()` exigeait le
fichier de clé — mais ce fichier n’est écrit qu’**après** un
enregistrement réussi. Au premier run, il n’existe pas encore : la garde
répondait « périmées » sur des zones parfaitement valides. Une fois par
projet, le correctif détruisait exactement ce qu’il protégeait.

Des zones qui existent, portent une strate `_tot` et n’ont pas de clé
sont désormais **adoptées** : on écrit la clé des sources courantes et
on ne recrée rien. L’adoption amorce la clé, donc le run suivant passe
par le chemin normal.

**Le risque assumé**, écrit dans le code : si les UGF ont changé depuis
que ces zones ont été construites, on adopte une géométrie périmée. Il
est borné — l’ancien comportement recréait les zones à *chaque* run,
donc des zones présentes viennent forcément du run précédent ;
l’adoption n’arrive qu’une fois par projet ; et le bouton de l’onglet
réenregistre toujours. Ne pas adopter coûtait des gigaoctets et des
heures de re-téléchargement, à chaque projet.

Deux tests, dont un qui vérifie que l’adoption ne transforme pas «
aucune zone » en « à jour » — sinon les moteurs santé partiraient sur un
`zone_id` qui ne désigne rien.

## nemetonshiny 0.143.11 (2026-09-01)

#### Fixed — Les zones de suivi étaient recréées à chaque run, et emportaient tous les caches

`build_project_monitoring_zones()` a `replace = TRUE` par défaut, et ce
`replace` **supprime** les lignes du projet avant de réinsérer : les
identifiants changent à chaque appel. L’étape « Santé — création des
zones de suivi » l’appelait à chaque lancement de la chaîne.

Tout ce qui est indexé sur l’identifiant devenait donc orphelin —
`cache/layers/{fordead,reconfort}/output_zone_<id>/`. Mesuré sur
Couchey, un seul projet, trois runs :

| répertoire       | marqueurs de reprise | taille |
|------------------|----------------------|--------|
| `output_zone_37` | 106                  | 3,5 Go |
| `output_zone_41` | 81                   | 2,8 Go |
| `output_zone_45` | 2                    | 1,4 Go |

**6,3 Go sous des zones qui n’existent plus** — vérifié en base, seule
la 45 subsiste.

Et le disque n’est pas le pire. Les marqueurs d’idempotence de
l’ingestion RECONFORT (`ingested/*.done`, qui font sauter une scène déjà
ingérée) vivent sous ce répertoire. Un nouvel identifiant = un
répertoire vide = **tout re-téléchargé**. La reprise après arrêt était
déjà écrite côté cœur ; c’est le renouvellement des identifiants qui
l’annulait, run après run.

La chaîne saute désormais l’étape quand les zones sont à jour. **Deux
conditions, et les deux comptent** : la clé dit que les sources n’ont
pas bougé (UGF, tènements, BD Forêt) ; la présence en base dit que les
zones existent encore et portent bien une strate `_tot`. Sauter sur la
seule clé servirait un `zone_id` qui ne désigne plus rien.

Le compromis est celui du contrôle d’intégrité : la clé porte sur les
**sources**, pas sur la simple présence des zones — sauter alors que les
UGF ont changé laisserait une géométrie de zone périmée. Et le bouton de
l’onglet réenregistre toujours.

#### Fixed — Le contrôle d’intégrité était rejoué en entier, réseau inchangé

51 min sur Couchey (17 056 tronçons), à **chaque** lancement de la
chaîne. Le résultat était pourtant déjà sur le disque :
`run_desserte_integrite()` écrit `integrite.rds`, et ce fichier est relu
— mais uniquement pour réafficher le panneau à la réouverture du projet.
Le lancement, lui, ne le consultait jamais.

Ce n’était pas une décision, c’était un chaînon manquant :

``` r

.load_cached_desserte(project_path, params)   # clé : les paramètres
.load_cached_integrite(cache_dir)             # aucune clé
```

Le lecteur de la desserte compare les paramètres et refuse un cache
calculé autrement. Celui de l’intégrité ne prend aucune clé : il sait
répondre « le fichier existe », pas « il est encore valable ».
Impossible de s’en servir pour décider de sauter.

`.desserte_integrite_cle()` la fournit — `mtime` + taille du GeoPackage
du réseau et de l’AOI, écrits en **sidecar** (`integrite_cle.rds`) pour
que les caches existants restent lisibles. La chaîne saute l’étape quand
la clé correspond, et le rapport dit pourquoi.

**Le sens de l’erreur est délibéré** : un `mtime` qui bouge sans que le
contenu change fait *recalculer* — on perd du temps. Jamais l’inverse :
servir un verdict d’intégrité périmé après une régénération du réseau
serait bien pire que d’attendre 51 min.

**Le bouton de l’onglet relance toujours.** Un geste explicite de
l’utilisateur veut dire « recalcule », pas « ressers-moi ce que tu as ».
Un test verrouille cette séparation (vérifié par mutation : déplacer la
garde dans le lanceur le fait tomber).

## nemetonshiny 0.143.10 (2026-09-01)

#### Fixed — RECONFORT emportait la session entière ; il tourne maintenant sous plafond

Run Couchey du 2026-09-01. RECONFORT s’arrête net à l’item **82 sur
203** de son ingestion, étape `crop`, **sans écrire le moindre événement
d’erreur**. Un worker qui lève écrit son erreur ; celui-là n’a rien
écrit — il a été tué.

Une minute après le dernier événement :

    07:11:35  Killed …/app-rstudio-1113027.scope due to memory pressure for
              user@1000.service being 56.87% > 50.00% for > 20s with reclaim activity
    07:11:35  systemd-oomd killed 9 process(es) in this unit

Neuf processus : RStudio, la session R, les workers. `systemd-oomd` ne
tue pas le processus fautif, il tue **le scope entier** — et il tue sur
la **pression**, pas sur un plafond absolu : le scope était à 14,5 Go
sur une machine de 31 Go.

**Pourquoi rien ne l’a arrêté.** Le cœur ne plafonnait que le
sous-processus Python (`.reconfort_run_py()` →
`.reconfort_cap_memory()`), au motif que c’est lui le gourmand. Mais la
boucle d’ingestion des 203 scènes Sentinel-2 est du **R pur**
(`reconfort_ingest.R`, événements `reconfort:ingest_item`), et le run
est mort **avant** d’atteindre Python. Le raisonnement laissait cette
phase à découvert.

RECONFORT passe donc en **enfant plafonné**, comme FORDEAD depuis la
v0.106.6 : `nemeton::run_memory_capped("run_reconfort_dieback", …)`. Un
dépassement tue l’enfant **seul**, avec une erreur attrapable, au lieu
d’emporter la session.

> Le plafond `NEMETON_MEMORY_MAX` du poste ne pouvait de toute façon
> rien faire ici : il gouverne `run_memory_capped()`, que RECONFORT
> n’utilisait pas. Et réglé à 16 Go sur une machine tuée à 14,5 Go, il
> n’aurait jamais mordu en premier.

#### Changed — Le parent ne rejoue que le push ntfy

Sous isolation, **l’enfant écrit lui-même** les fichiers `.json` /
`.ndjson`. Rejouer le callback composite dupliquerait chaque ligne
NDJSON — le piège déjà payé sur FORDEAD. La moitié « push » est donc
isolée dans `.build_reconfort_ntfy_callback()`, strict miroir de son
équivalent FORDEAD, avec la dédup par phase côté **parent** : elle
survit à la mort et au redémarrage de l’enfant.

Le callback composite n’est plus construit dans ce worker :
contrairement à FORDEAD, il n’émet aucun heartbeat propre et n’en avait
donc pas l’usage.

## nemetonshiny 0.143.9 (2026-09-01)

#### Fixed — « objet ‘con’ introuvable » : les deux `on.exit` du worker étaient dans le mauvais ordre

Cause trouvée, reproduite, corrigée. Le défaut qui faisait échouer les
moteurs Santé **après** qu’ils aient fait et persisté leur travail — 13
h 40 et 4 h 10 au premier run, `ingest_run.json` à `done`, 183 scènes,
51 alertes en base.

Chaque worker enregistre `.release_worker_memory()` en tête de corps,
puis la fermeture de sa connexion :

``` r

on.exit(.release_worker_memory(), add = TRUE)   # 1er enregistré
con <- get_monitoring_db_connection(db_url = db_url)
on.exit(close_monitoring_db_connection(con), add = TRUE)   # 2e
```

Les handlers `on.exit` s’exécutent dans l’**ordre d’enregistrement**. Or
la libération fait `rm(list = ls(envir = env), envir = env)` sur la
frame du worker — `con` compris. Quand le second handler s’évalue, la
liaison n’existe plus.

Les trois fermetures passent donc devant, par `after = FALSE`.
**Déplacer la libération en fin de corps** aurait été plus lisible, mais
l’aurait retirée des chemins d’échec précoce (le
[`stop()`](https://rdrr.io/r/base/stop.html) « Monitoring DB not
configured » juste au-dessus) — là où rendre la mémoire compte le plus.

#### Fixed — Le même défaut sur la connexion de log, silencieux depuis le début

`close(.ws_log_conn)` est enregistré au même endroit, après la
libération. Son handler est enveloppé dans un `tryCatch` : il échouait
donc **sans rien dire**, et la connexion de log du worker n’était jamais
fermée. Même correctif.

#### Pourquoi la recherche statique ne pouvait pas le trouver

`con` n’est pas une variable globale : c’est une locale parfaitement
liée au moment où le code l’écrit. Le défaut est **temporel**, pas
lexical — la liaison est détruite entre l’enregistrement du handler et
son évaluation.
[`codetools::findGlobals`](https://rdrr.io/pkg/codetools/man/findGlobals.html)
sur l’intégralité des deux espaces de noms ne pouvait rien voir, et
aucun `testServer` ne rejoue un worker `future`.

D’où les deux tests : un qui isole le mécanisme, un qui lit le fichier
et exige `after = FALSE` sur les trois fermetures (vérifié par
mutation).

> **Le piège qui a faussé la première reproduction**, noté parce qu’il
> se reproduira : un handler qui n’**utilise** pas la locale ne la force
> jamais — évaluation paresseuse — et ne casse pas. Ma première
> tentative concluait « pas de bug » pour cette seule raison. Le test
> porte un [`force()`](https://rdrr.io/r/base/force.html) explicite ;
> sans lui, il passerait sur le code fautif.

## nemetonshiny 0.143.8 (2026-09-01)

#### Fixed — Le rapport nommait l’erreur, pas l’endroit

Run Couchey du 2026-08-31 : le correctif v0.143.6 a bien fait remonter
la cause des deux échecs Santé — **« objet ‘con’ introuvable »** — mais
pas l’endroit. Une recherche statique dans les deux paquets
([`codetools::findGlobals`](https://rdrr.io/pkg/codetools/man/findGlobals.html)
sur les deux espaces de noms, défauts auto-référents, code construit
dynamiquement) n’a rien donné : le message seul ne dit pas dans quel
appel.

Or [`conditionCall()`](https://rdrr.io/r/base/conditions.html) le nomme,
et il **traverse la frontière `future`** — vérifié : un
[`stop()`](https://rdrr.io/r/base/stop.html) levé dans un worker rend
bien son appel au parent. Le rapport de la chaîne l’affiche désormais («
… — dans `g(42)` »).

Sans lui, localiser un échec de moteur long coûte un run de plusieurs
heures de plus. C’est le même raisonnement qu’en v0.143.6, poussé d’un
cran : le message disait *quoi*, il dit maintenant *où*.

#### Ce que la vérification de bout en bout a montré

Le chemin a été rejoué sur un vrai `future`, pas sur un stub : un worker
qui **réussit et persiste**, puis lève depuis son `on.exit`. Le parent
reçoit
`objet 'con' introuvable — dans fermer_connexion(con_inexistant)`.

Une erreur levée dans l’`on.exit` d’un future fait donc échouer la tâche
**alors que le corps a rendu son résultat**. C’est exactement la forme
observée sur Couchey — `ingest_run.json` à `done`, 183 scènes, et la
tâche en erreur. La queue du worker devient la piste principale ; la
cause reste à confirmer au prochain run.

## nemetonshiny 0.143.7 (2026-08-31)

#### Changed — Le repli CHM en dur rendu au cœur, qui le fait mieux

`nemeton` v0.192.2 avait appris à résoudre `cache/layers/opencanopy/` ;
v0.193.0 lui ajoute l’argument qui manquait, `validate`. Les deux
morceaux du contournement que l’app portait depuis la v0.141.x rentrent
donc chez eux.

`.project_chm()` ne sonde plus aucun chemin. Il passe son garde de
contenu au résolveur, qui **saute** un candidat refusé et continue à la
source suivante :

``` r

nemeton::resolve_project_chm(path, validate = .chm_exploitable)
```

Le repli inter-sources n’est pas seulement préservé, il est **meilleur**
: il balaie les six candidats CHM du cœur dans leur ordre (LiDAR HD
d’abord, ADR-007) au lieu des trois noms de fichiers écrits à la main —
dont un, `chm.tif`, n’existe pas dans ce répertoire : le témoin
s’appelle `chm_1_5m.tif`. La liste était fausse d’un tiers.

Ce qui **reste ici**, et doit y rester : `.chm_exploitable()`. C’est un
garde de **contenu**, pas de chemin, et son seuil de 5 m est celui de
`segment_houppiers()`, pas une constante du cœur. Le cœur rend le
premier chemin qui matche sans regarder ce qu’il y a dedans ; un modèle
de hauteur plat (le cas « Fordead ») le satisfait, et le plan
d’échantillonnage tirerait sans strate de hauteur.

#### Fixed — Le prédicat pouvait lever, ce qui coûtait désormais les sources suivantes

Tant qu’il était appelé par nous, une erreur dans `.chm_exploitable()`
ne coûtait qu’un candidat. Passé en `validate`, elle **remonte au cœur
et arrête la résolution** sur ce candidat au lieu de passer au suivant —
exactement ce que l’argument sert à éviter.

Le `tryCatch` ne couvrait que `spatSample()`. Or cette fonction peut
aussi **rendre** un `data.frame` sans colonne, sur lequel le `v[[1]]`
qui suivait levait « subscript out of bounds ». Le prédicat est
désormais total.

Son `TRUE` du doute change aussi de sens, et le commentaire le disait
encore à l’ancienne : il ne veut plus dire « on laisse le cœur trancher
» — le cœur ne tranche plus après nous, il **accepte**. Le choix reste
le bon (un faux positif coûte deux minutes de segmentation, un faux
négatif coûte la source), mais il n’est plus délégué.

#### Changed — Plancher `nemeton (>= 0.193.0)`

Bumpé maintenant, et pas à v0.192.2 : tant que la boucle en dur était
là, le plancher aurait documenté une dépendance que le code ne prenait
pas encore.

## nemetonshiny 0.143.6 (2026-08-31)

#### Fixed — Le rapport disait « Erreur » sans jamais dire laquelle

Troisième run sur Couchey : la Santé part enfin (les zones sont vues),
mais deux moteurs échouent — surveillance rapide après **13 h 40**,
FORDEAD après **4 h 10** — et le rapport n’affiche qu’un « Erreur » nu.

Or `task$result()` **re-lève** l’erreur du worker : c’est le seul
endroit où son message existe encore. Le jeter revenait à imposer un
relancement de plusieurs heures pour apprendre ce qu’on savait déjà à la
seconde près.

`pipeline_task_error()` l’extrait désormais, sur les **sept** réponses
concernées (trois moteurs Santé, quatre de reGénération). Il couvre
aussi les moteurs qui rendent un échec **par valeur** — contrat
`list(status = "error", reason =, detail =)` de plusieurs moteurs de
l’app — que regarder les seules conditions aurait ratés. Les tracebacks
Python de FORDEAD (reticulate) sont tronqués à 300 caractères et mis sur
une ligne, pour ne pas noyer le rapport.

#### Ce que l’inspection du projet a montré

Les deux moteurs « en erreur » avaient travaillé et produit :

|                                   |                                  |
|-----------------------------------|----------------------------------|
| `ingest_run.json`                 | statut **done**, **183 scènes**  |
| Table `alert`, zone `couchey_tot` | **51 alertes `fordead_dieback`** |

L’échec est donc survenu **après** le travail utile. Sa cause reste
inconnue — c’est précisément ce que le correctif ci-dessus permettra de
savoir au prochain run.

## nemetonshiny 0.143.5 (2026-08-30)

Deux messages trompeurs, tous deux désignant la conséquence en cachant
la cause.

#### Fixed — « Échec du typage » sur le meilleur résultat possible

Sur Couchey, le typage de la desserte s’affichait **en rouge** : « Le
réseau ne contient aucune route à vectoriser ». Inspection du cache : le
moteur avait parfaitement réussi.

|                              |                 |
|------------------------------|-----------------|
| Routes **nouvelles** à créer | **0**           |
| Réseau existant              | 17 056 tronçons |
| Parcelles desservies         | **76 sur 76**   |
| Coût du réseau glouton       | **0**           |

Le réseau existant dessert déjà toutes les UGF : aucune route nouvelle
n’est nécessaire.
[`foretaccess::vectoriser_reseau()`](https://pobsteta.github.io/foretaccess/reference/vectoriser_reseau.html)
travaille sur `reseau$lignes` — les routes *nouvelles* — et abandonne
quand il n’y en a aucune. Un excellent résultat rapporté comme une
panne.

Ce cas est désormais distingué **avant** l’appel, par un statut propre :
toast d’information (« Aucune route nouvelle à typer : le réseau
existant dessert déjà toutes les parcelles ») et étape comptée *Sautée*,
non *Échec*, dans le rapport de la chaîne.

#### Fixed — « structure de végétation manquante » quand c’est la grille LiDAR qui manque

microclimf annonçait une structure de végétation absente alors que le
projet portait bien un `lai_prosail.tif`. La vraie cause : `lidar_mnt/`,
`lidar_mnh/` et `lidar_nuage/` étaient **vides**, donc
`resolve_regen_lidar_grid()` rendait `NULL` et le bloc était sauté avant
tout essai. Le repli LAI Sentinel-2 ne vit qu’**à l’intérieur** du bloc
grille — il n’était jamais atteint, et n’aurait de toute façon pas suffi
: microclimf a besoin du MNT/MNH pour son maillage.

Trois causes, trois messages désormais : « grille LiDAR HD manquante
(MNT/MNH non téléchargés) », « clé CDS/ERA5 absente », ou les deux. «
structure de végétation manquante » reste réservé à son cas réel —
grille présente, mais ni nuage LiDAR ni LAI.

#### Fixed — Deux fixtures de test décrivaient un réseau impossible

`.typage_write_reseau_obj()` et la fixture de
`test-desserte_visualisation.R` construisaient un `foretaccess_reseau`
**sans champ `lignes`**. Elles décrivaient donc, sans le vouloir, le cas
limite ci-dessus. Un vrai objet réseau porte toujours ce champ ; les
fixtures le fournissent maintenant.

## nemetonshiny 0.143.4 (2026-08-30)

#### Changed — La section « Tout calculer » se replie comme les autres

Même mécanisme que les blocs voisins de la sidebar *Sélection* (projets
récents, recherche) : carte à en-tête `bg-secondary` cliquable, chevron,
`data-bs-toggle="collapse"`. Dépliée par défaut, pour que le bouton
reste visible sans clic préalable.

Le gain n’est pas cosmétique : le panneau de progression liste
**dix-sept étapes** et repoussait tout le reste de la sidebar hors de
l’écran dès la chaîne lancée.

Quatre tests, dont un qui vérifie que le bouton et le panneau sont bien
**à l’intérieur** du bloc repliable — les laisser en dehors les rendrait
insensibles au repli, et un test portant sur la seule présence des
attributs passerait quand même.

## nemetonshiny 0.143.3 (2026-08-30)

#### Fixed — Santé sautée alors que les quatre zones venaient d’être créées

Second run complet sur Couchey : l’étape « création des zones de suivi »
réussit en 9 s — et les trois moteurs se sautent quand même sur « Aucune
zone de suivi enregistrée ». Vérification en base : les quatre zones
(`couchey_tot`, `_feu`, `_res`, `_mix`) étaient bien là. La création
n’était pas en cause.

La garde interrogeait `input$zone_id`, un `selectInput` alimenté par
`updateSelectInput()` — qui ne remonte au serveur **qu’après un
aller-retour client**. Juste après la création des zones, il était
encore vide.

**Troisième occurrence du même piège dans cette chaîne** — après les
années E-OBS (v0.143.0) et `use_corrected` (v0.143.0), tous deux
pourtant identifiés et commentés. Les gardes lisent désormais les zones
**en base** via `fordead_zone_id()` et transmettent la zone résolue aux
trois moteurs, qui la préfèrent à leur menu.

#### Fixed — Le rapport masquait la vraie cause de l’échec IA

L’étape affichait « Le lancement a été refusé par l’onglet (prérequis
manquant) », un message faux : les prérequis étaient réunis
(`indicators_sf` à 76 lignes, `MISTRAL_API_KEY` définie).

La raison réelle n’arrivait jamais. `raison <<- ...` était écrit dans le
**bloc** d’un `tryCatch` — or ce bloc s’évalue dans le frame
**appelant**, si bien que le `<<-` sautait par-dessus l’observer pour
chercher la variable dans le namespace du paquet. Elle restait `NULL`,
d’où le repli sur le message générique. Les deux cas ont été isolés pour
lever le doute : dans un *handler* `error = function(e)` le `<<-`
fonctionne (son enclos est le frame de la fonction) ; dans le *bloc*, il
fuit.

Ce que cela masquait : **l’appel Mistral lui-même échouait**. Son
message partait dans un toast de 8 secondes — invisible au milieu d’un
run de plusieurs heures. Il remonte désormais au rapport, détail
compris.

## nemetonshiny 0.143.2 (2026-08-29)

Trois défauts trouvés grâce au premier run complet sur Couchey, et la
création des zones de suivi ajoutée à la chaîne.

#### Fixed — « Perspective IA : Réussie » en 1 seconde, sans rien avoir généré

Le rapport du run affichait l’étape en vert, durée **1 s**, pour ce qui
demande treize appels LLM. L’appel avait échoué : le `tryCatch`
affichait un toast et rendait `NULL`, mais la fonction **continuait
jusqu’à `invisible(TRUE)`**. La chaîne rapportait donc « Réussie » pour
une perspective inexistante.

Un faux positif silencieux est pire qu’un échec : il fait croire que le
travail est fait. Une synthèse vide fait désormais échouer l’étape, avec
le message d’erreur du LLM.

C’est aussi ce qui expliquait le **Plan d’actions sauté** juste après :
il se construit sur les commentaires que la synthèse n’avait pas écrits.

#### Fixed — Les 12 commentaires de famille n’étaient pas générés

Le switch « toutes les familles » de l’onglet *Synthèse* est décoché par
défaut (`<input type="checkbox">` sans `checked`), et la chaîne le
suivait — alors que le libellé de son étape annonce « synthèse + 12
familles ». Elle l’impose désormais (`remplir_familles = TRUE`), le
bouton manuel gardant son comportement.

#### Fixed — Les sauts ne disaient pas ce qu’il fallait corriger

« Le lancement a été refusé par l’onglet (prérequis manquant) »
n’apprend rien. Les trois moteurs Santé annoncent maintenant **« Aucune
zone de suivi enregistrée pour ce projet »** — la clé i18n existait
depuis la v0.143.0 sans être utilisée nulle part. La génération IA
remonte sa raison réelle (clé API manquante, avec le nom de la variable
attendue ; ou absence d’indicateurs) au lieu d’un booléen.

#### Added — Création des zones de suivi dans la chaîne

Nouvelle étape 12, juste avant les trois moteurs Santé, qui exigent tous
un `zone_id`. Elle emprunte le même chemin que le bouton
d’enregistrement de l’onglet ; `build_project_monitoring_zones()` étant
en upsert, la relancer recrée les zones du projet plutôt que d’en
accumuler. Elle se saute proprement quand la base PostGIS n’est pas
configurée.

La chaîne compte donc **17 étapes**.

#### Ce que le run a confirmé

La mécanique tient : reGénération a enchaîné ses cinq étapes — années
E-OBS 3 min 18, précipitations 1 min 43, température 3 min 22, gel R7 1
h 38 min 53, moteur 7 min 56 — et la chaîne est allée du début à la fin
sans se bloquer.

## nemetonshiny 0.143.1 (2026-08-29)

#### Fixed — La chaîne restait bloquée sur « Indicateurs / En cours »

Premier essai réel sur Couchey : le calcul des indicateurs s’est terminé
normalement, mais la chaîne n’a jamais avancé — l’étape 1 est restée «
En cours » et les quinze suivantes « En attente ».

La réponse au pipeline était posée depuis `poll_fn`, la boucle de
progression qui tourne dans un callback
[`later::later()`](https://later.r-lib.org/reference/later.html) — donc
**hors contexte réactif**. La lecture du `reactiveVal` y lève
`Operation not allowed without an active reactive context` ; l’erreur
remonte, la réponse n’est jamais posée, et la chaîne attend indéfiniment
une étape pourtant terminée. C’est le mode de défaillance annoncé à la
livraison de la v0.143.0, réalisé par le premier branchement écrit —
dans un fichier qui documente déjà ce piège deux fois, le poller isolant
toutes ses autres lectures.

Reproduit hors Shiny en trois lignes avant correction, plutôt que
corrigé de tête.

**Second défaut, trouvé en cherchant si le premier était isolé.** Dans
les six autres modules, les lectures sont en contexte réactif — donc
légales — mais elles abonnaient l’observer de statut à la mémoire de
requête. Poser la requête le redéclenchait, et s’il portait encore le
`success` d’un run précédent, il répondait **avant que le moteur ne
redémarre** : l’étape aurait été rapportée réussie sans avoir tourné.
Toutes les lectures sont désormais isolées.

Un test lit les sources des sept modules et refuse toute lecture non
isolée d’une mémoire de requête (vérifié par mutation). Test de source
assumé : le défaut est un contexte d’exécution, qu’aucun `testServer` ne
reproduit.

## nemetonshiny 0.143.0 (2026-08-28)

Jalon : un seul bouton lance les seize calculs de l’application, puis
les generations IA.

#### Added — « Tout calculer » : un bouton, seize etapes, un rapport

Dans la sidebar de l’onglet *Selection*, sous le bouton de calcul des
indicateurs. Une modale demande le perimetre (les seize etapes,
cochables) et le **profil de l’analyste** parmi les quinze profils
experts, qui s’applique a toutes les generations IA de la chaine. Un
panneau suit l’avancement etape par etape, avec un bouton d’arret ; a la
fin, un rapport donne pour chaque etape son issue et sa duree.

Une etape en echec **n’interrompt pas** la chaine : le rapport distingue
`reussie` / `echec` / `sautee` / `annulee`. « Trois sautees faute de
configuration » ne doit pas se lire « trois en echec ».

L’ordre n’est pas cosmetique — il encode ce qui alimente quoi :

| \# | Etape | Pourquoi la |
|----|----|----|
| 1 | Indicateurs | toutes les vues les lisent |
| 2 | Accessibilite — correction LiDAR | l’analyse consomme le reseau corrige des qu’il existe |
| 3 | Accessibilite |  |
| 4-6 | Desserte, typage, integrite | typage et controle lisent le cache du moteur |
| 7 | reGeneration — annees moyenne / canicule | **determine** les annees que le gel et le moteur consomment |
| 8-9 | reGeneration — precipitations, temperature (E-OBS) | bornees par les annees ci-dessus |
| 10 | reGeneration — gel R7 | enrichit le resultat courant |
| 11 | reGeneration — moteur |  |
| 12 | Sante — surveillance rapide | son ingest remplit le cache Sentinel-2 |
| 13-14 | Sante — FORDEAD, RECONFORT | tous deux lisent ce cache |
| 15 | Perspective IA | synthese + les 12 commentaires de famille en une passe |
| 16 | Plan d’actions IA | se construit sur les commentaires que (15) vient d’ecrire |

**Choix d’architecture.** L’orchestrateur ne lance aucun moteur lui-meme
: il poste `app_state$pipeline_request` et le module proprietaire repond
sur `app_state$pipeline_answer`. Chaque moteur est un `ExtendedTask`
dont les arguments viennent des inputs de son onglet (moteurs coches,
buffer, reseau corrige, periode S2) ; un orchestrateur qui les
appellerait directement devrait tout redupliquer et divergerait des
qu’un onglet gagne une option. Le corps de chaque observer de bouton est
donc extrait en fonction locale, appelee par le bouton **et** par la
chaine — aucun bloc duplique.

**Le piege de l’aller-retour client, rencontre trois fois.** Plusieurs
etapes publient leur resultat par `updateNumericInput()` / `renderUI()`,
qui ne remontent au serveur qu’apres un aller-retour navigateur :
l’etape suivante y lirait encore la valeur precedente. Trois valeurs
sont donc passees en argument explicite plutot que relues dans les
champs :

- les annees moyenne / canicule, prises sur `eobs_task$result()` — sans
  quoi le moteur reGeneration aurait tourne sur les defauts codes en dur
  (2018 / 2022), **sans que rien ne le signale** ;
- `use_corrected`, calcule depuis `corrected_available()` (qui lit le
  disque) — sans quoi l’analyse d’accessibilite serait partie sur le
  reseau brut apres deux a trois heures de correction devenue inutile ;
- le profil expert des generations IA.

`NULL` conserve partout le comportement exact des boutons manuels.

**Le mode de defaillance a connaitre.** Tout chemin de code qui a
reconnu une requete DOIT repondre. Un module qui se tait bloque la
chaine sur son etape, sans rien afficher. Les gardes internes (pas de
zone monitoring, pas de cle API, lecture seule) faisaient exactement
cela : la fonction de lancement sortait tot, aucune tache ne demarrait,
personne ne repondait. Les seize branchements testent desormais la
valeur de retour du lancement et repondent `sautee` quand il n’a pas eu
lieu. Les deux generations IA rendaient meme `TRUE` en dur — une
perspective jamais generee aurait ete rapportee « reussie ».

**Tests.** 74 sur `service_pipeline.R`, sans Shiny : ordre du registre,
reponse tardive qui ne doit ni ecraser l’etape suivante ni decaler le
curseur, reponse d’un run annule puis relance, `sautee` distinguee de
`echec`, annulation. Plus un test qui verifie que **chaque etape
declaree a un ecouteur** dans le module annonce — le filet contre
l’etape orpheline qui bloquerait la chaine. Verifies par mutation.

#### Restent hors chaine

Optimisation, OSM et detection (panneaux d’analyse annexes de la
desserte), RVT et pre-build CVAT (preparation d’annotation).

## nemetonshiny 0.142.3 (2026-08-28)

#### Fixed — La carte UGF restait vide au premier passage sur son sous-onglet

Passer de « Carte cadastrale » à « Carte UGF » n’affichait ni les
tènements ni les UGF ; il fallait aller sur « Tableau UGF » puis revenir
pour que la carte se peuple.

`output$ug_map` était **la seule des six cartes leaflet de l’app à
rester suspendue** quand son onglet est caché (`suspendWhenHidden` vaut
`TRUE` par défaut). « Carte UGF » étant un sous-onglet non-défaut, la
carte n’existe pas encore côté client au moment où l’observer de dessin
émet ses `leafletProxy()` — et leaflet **jette silencieusement** les
messages adressés à une carte absente du DOM
(`Couldn't find map with id ug-ug_map` dans la console du navigateur).
Après un aller-retour par « Tableau UGF », la carte existe et le
redessin déclenché par la navigation s’applique : d’où le contournement
que l’utilisateur avait trouvé.

Le repo documentait déjà ce piège pour les cinq autres cartes — cf.
`mod_monitoring_fordead_map.R` : « peut rester suspendu / s’initialiser
à taille 0 → clics et leafletProxy inopérants ». `mod_ug` posait
l’option sur son tableau et ses deux compteurs, mais avait oublié sa
carte. Le coût est nul : ce `renderLeaflet` ne produit qu’une carte vide
(fond de plan, contrôle de couches, barre de dessin) ; les polygones
restent gouvernés par la garde de visibilité de la v0.142.2 et ne sont
toujours pas dessinés onglet fermé.

Le test **intercepte l’appel réel** à `outputOptions()` plutôt que de
grepper le fichier source : `outputOptions(output, "ug_map")` ne répond
rien sous `testServer()`, et un test textuel passerait sur un appel
commenté (vérifié par mutation).

> Part de responsabilité de la v0.142.2, honnêtement : la garde de
> visibilité livrée alors n’a pas créé cette course, mais elle l’a
> vraisemblablement rendue systématique. Auparavant l’observer de dessin
> tournait à chaque invalidation de `projet_ug`, ce qui multipliait les
> occasions qu’un envoi tombe après la création de la carte ; la garde
> les réduit au seul bump de navigation. Non vérifié par la mesure — les
> tentatives E2E n’ont pas abouti (chromote ne parvenait plus à se
> connecter à l’app de test).

#### Changed — Brief cœur : `resolve_project_chm()` ignore `cache/layers/opencanopy/`

`specs/BRIEF-nemeton-resolve-chm-opencanopy.md`. Le résolveur du cœur
sonde `cache/layers/chm/` sous le label « Open-Canopy CHM », alors que
`download_chm_opencanopy()` écrit dans `cache/layers/opencanopy/` : le
CHM du projet est invisible et les appelants travaillent sans modèle de
hauteur, en silence. Deux modules de l’app ont déjà dû contourner le
même défaut (`service_marculus.R`, puis `mod_sampling.R` en v0.142.2).

Le brief signale deux pièges que l’implémentation naïve manquerait : un
candidat sans `file` mosaïquerait en VRT les deux orthophotos et les
quatre indices spectraux qui cohabitent dans ce répertoire ; et les
nouvelles entrées doivent atterrir **après** `"LiDAR HD MNH"`, la liste
actuelle plaçant `cache/layers/chm` en tête alors que le LiDAR local est
la meilleure source (ADR-007).

Il porte aussi l’entrée `PLAN.md` de la v0.142.2, que cette session ne
peut pas écrire côté cœur (règle 12).

## nemetonshiny 0.142.2 (2026-08-28)

#### Fixed — Le chargement d’un projet récent reconstruisait sept fois la même géométrie UGF

Cliquer sur un projet récent (mesuré sur Couchey : 75 UGF, 223
tenements) laissait la boucle Shiny bloquée une dizaine de secondes
entre le clic et l’affichage des parcelles cadastrales, la notification
« Projet chargé » comprise (mesure consolidée plus bas).

Le chargement lui-même n’y était pour rien : `load_project()` rend la
main en ~280 ms et la portion synchrone de l’observer en ~440 ms. Le
profilage Rprof du thread principal a montré la vraie cause — **sept
reactives indépendantes appellent `ug_build_sf()` dans le même flush** :
`ug_sf_4326` (mod_action_plan), `units_sf` (mod_desserte,
mod_accessibility, mod_sampling, mod_regeneration, via
`.resolve_project_aoi_2154`), `ugf_sf_r` (mod_monitoring_pixel_map) et
le rendu carte de mod_ug. Leurs sorties portent
`suspendWhenHidden = FALSE`, donc elles se rendent même onglet fermé.
Chaque appel refait un `st_make_valid()` + `st_union()` PAR UGF : ~3 s
pièce, sept fois de suite, sur le thread unique.

`ug_build_sf()` est pure — son résultat ne dépend que de `projet$ugs` et
`projet$tenements`. Elle est désormais mémoïsée, avec pour clé le **hash
du couple (ugs, tenements)** et non l’id du projet : toute mutation du
domaine (réaffectation d’un tenement, renommage ou scission d’UGF,
migration de schéma) change le hash et invalide l’entrée, sans qu’aucun
appelant ait à purger quoi que ce soit. Le hash coûte 0,4 ms contre 2950
ms de reconstruction. Cache borné à trois entrées.

Dans l’app : un recalcul, sept lectures de cache. Mesure de bout en bout
consolidée pour les trois correctifs de ce cycle : voir plus bas.

Trois tests verrouillent le comportement (`test-domain_ug.R`), dont un
qui **compte les reconstructions réelles** via un mock de
`.ug_build_sf_impl()` : comparer deux résultats d’une fonction
déterministe ne prouverait rien sur le cache (vérifié par mutation —
cache neutralisé, le test échoue).

#### Fixed — La géométrie UGF était reconstruite tenement par tenement

Deuxième volet du même chemin critique. `ug_geometry()` faisait un
`st_make_valid()` **par UGF**, soit 75 appels sur ~3 tenements chacun là
où un seul appel sur les 223 fait le même travail. Les opérations `sf`
paient un coût fixe par *appel* — aller-retour R ↔︎ GEOS/s2, relecture
des paramètres du CRS — qui domine largement le coût par géométrie sur
de petits lots : **695 ms en 75 appels contre 97 ms en un seul**. Même
piège que `st_area()` en boucle.

La dissolution passe par un nouveau `.ug_geometries()` qui valide tout
le projet en une passe ; `ug_geometry()` en devient le cas à un élément
et garde sa signature. `ug_build_sf()` : **2988 ms → ~950 ms**, et
toujours une seule fois grâce au cache du volet précédent.

Équivalence vérifiée contre l’implémentation d’origine sur le projet
Couchey : 75/75 UGF géométriquement identiques (`st_equals`), différence
symétrique nulle par paire, attributs et CRS identiques, écart de
surface 3 × 10⁻⁷ m².

#### Fixed — Le fallback planaire s2 bavardait dans la console R

Les blocs « Spherical geometry (s2) switched off » / « although
coordinates are longitude/latitude, st_union assumes that they are
planar » / « switched on » qui défilaient au démarrage venaient de ce
même chemin : sur les 75 UGF de Couchey, **une seule** a des sommets
auto-tangents que s2 refuse de dissoudre, et `ug_geometry()` bascule
alors sur GEOS planaire. La bascule est délibérée et sans effet sur le
résultat — `sf_use_s2()` émet simplement un
[`message()`](https://rdrr.io/r/base/message.html) à chaque changement
d’état global.

Ce bruit n’apprenait rien à l’utilisateur et se répétait à chaque appel
de `ug_build_sf()` (donc sept fois par chargement de projet, avant le
cache). Il est désormais muselé à la source (règle stricte 9 : pas de
[`message()`](https://rdrr.io/r/base/message.html) en prod). L’état de
`sf_use_s2()` reste restauré à la sortie. Mesure après correctif : zéro
message émis par un `ug_build_sf()` complet.

#### Fixed — La carte UGF se dessinait pendant que son onglet était fermé

Le rendu des tenements de `mod_ug` passe par `leafletProxy()` dans un
`observe()` — que rien ne suspend quand l’onglet est caché. Or leaflet
**jette silencieusement** les polygones envoyés à une carte absente du
DOM, ce que le module documentait déjà : son observer de re-zoom les
redessine à l’ouverture de l’onglet via `rv$redraw_counter`.

Ce travail était donc intégralement perdu — mais payé sur le thread
unique, dans le flush même qui doit afficher les parcelles cadastrales :
**2 × 370 ms** au chargement d’un projet récent. Le dessin est
maintenant conditionné à la visibilité du sous-onglet (et non à un
drapeau « déjà ouvert » : si l’onglet est affiché quand les données
arrivent, on dessine tout de suite). Deux tests verrouillent les deux
moitiés de la garde, vérifiés par mutation dans les deux sens.

#### Mesure de bout en bout des trois correctifs ci-dessus

Chrome piloté, du clic sur le projet récent à l’apparition des parcelles
cadastrales, sur Couchey (75 UGF, 223 tènements). Trois exécutions par
configuration, machine par ailleurs au repos, code de référence
réinjecté par
[`assignInNamespace()`](https://rdrr.io/r/utils/getFromNamespace.html)
pour que les deux configurations partagent tout le reste :

| Configuration | Mesures (s)           | Médiane    |
|---------------|-----------------------|------------|
| Avant         | 13,67 / 12,68 / 13,16 | **13,2 s** |
| Après         | 5,11 / 5,61 / 5,10    | **5,1 s**  |

Soit **2,5×**, avec une dispersion de ±0,5 s.

> Un chiffre de « 29,5 s » a circulé en cours de session : il provenait
> d’une exécution **sous
> [`Rprof()`](https://rdrr.io/r/utils/Rprof.html)** (échantillonnage à
> 20 ms) et sur une machine chargée, deux biais qui gonflent la mesure.
> Les runs isolés de ce type variaient de 6,6 à 9,7 s côté « après » —
> c’est ce qui a motivé le passage à un banc répété plutôt qu’à un run
> unique. Les mesures unitaires citées plus haut (`ug_build_sf`,
> `st_make_valid`, nombre d’appels) sont, elles, prises hors profileur
> et reproduites.

#### Fixed — « Générer les placettes » échouait sur tout projet dont le MNT est en degrés

`create_sampling_plan(): Stratification-valid candidate pool (0) is below n_base (88). 2108 of 2108 candidates fell on NA pixels of the CHM / MNT rasters.`
— le message accusait la couverture des rasters ; la cause était une
**unité**.

`prep_sampling_raster()` compare la résolution du raster à
`target_res_m = 5`, en mètres. Rien ne garantissait que le raster soit
métrique : `resolve_project_dem()` rend le `cache/layers/dem.tif` de
Couchey en EPSG:4326, dont la résolution vaut 0,00025 **degré**. La
règle `cur_res < target_res_m` était donc toujours vraie et le facteur
d’agrégation valait `round(5 / 0,00025)` = **20 003** : le MNT sortait
de la fonction en **une seule cellule** (118 × 318 → 1 × 1). Tous les
candidats tombaient ensuite sur du NA. Le buffer de 200 souffrait du
même flou d’unité.

La fonction aligne désormais le raster sur le CRS de la zone
(Lambert-93, métrique) avant tout raisonnement en mètres — ce dont le
cœur a de toute façon besoin, puisqu’il calcule le TPI avec
`terra::focalMat(mnt, d = 100)`, où 100 s’exprime dans les unités du
CRS. Elle sort de `mod_sampling_server()` en `.prep_sampling_raster()` :
elle ne porte aucun état Shiny et son contrat d’unités est précisément
ce qui devait être testé.

La comparaison de CRS utilise `==` (sémantique) et non
[`identical()`](https://rdrr.io/r/base/identical.html), qui compare
aussi le libellé `$input` — un raster déjà en Lambert-93 mais décrit «
RGF93 v1 / Lambert-93 » plutôt que « EPSG:2154 » aurait été reprojeté à
chaque appel, pour rien et en resamplant.

Vérifié sur Couchey : le MNT sort en EPSG:2154, 169 × 302 à 20 m, 48 292
cellules utiles, et le plan se génère (GRTS). Quatre tests, vérifiés par
mutation.

#### Fixed — Le CHM Open-Canopy du projet était ignoré par le plan d’échantillonnage

Même écran, défaut distinct.
[`nemeton::resolve_project_chm()`](https://pobsteta.github.io/nemeton/reference/resolve_project_layers.html)
sonde `cache/layers/chm/`, alors que `download_chm_opencanopy()` dépose
ses livrables dans `cache/layers/opencanopy/`. Sur Couchey, un CHM
parfaitement exploitable — EPSG:2154, 0,2 m, hauteurs jusqu’à 32 m —
était donc invisible et le plan tirait **sans strate de hauteur**, en
silence.

`service_marculus.R` avait déjà rencontré et traité ce cas pour la
segmentation des houppiers. Son helper est repris ici plutôt que
dupliqué, et renommé `.project_chm()` / `.chm_exploitable()` — ce n’est
pas un helper Marculus : il sert les deux appelants qui ont besoin du
meilleur modèle de hauteur disponible, LiDAR HD prioritaire, Open-Canopy
en repli, chaque candidat devant porter de la hauteur.

Le plan de Couchey passe de « erreur bloquante » à 112 placettes
stratifiées hauteur × topographie.

> Cause racine côté cœur, **non corrigée ici** (règle 12 : ce dépôt
> n’écrit pas dans `nemeton`) : le résolveur devrait connaître
> `cache/layers/opencanopy/`. Le contournement applicatif ci-dessus
> tient sans lui.

## nemetonshiny 0.142.1 (2026-08-27)

#### Changed — le plancher cœur passe à 0.192.0, et l’icône « fiche » devient garantie

`nemeton 0.192.0` est publiée (tag `v0.192.0`, `main` du cœur à
`7c9714c`) : les colonnes `doc_url` / `doc_lang` de `indicator_labels()`
existent désormais dans une release stable.
`Imports: nemeton (>= 0.189.0)` → `(>= 0.192.0)`.

Ce n’est pas un bump « pour suivre » : la v0.142.0 consommait déjà cette
API, mais contre un cœur antérieur elle ne trouvait rien et l’icône
restait invisible — la fonctionnalité était livrée sans être
atteignable. Le plancher transforme ce silence en garantie.

**Les tests n’ont plus de skip de version.** Les trois
`skip_if_not("doc_url" %in% names(ind))` de `test-mod_family-doc-icon.R`
deviennent des `expect_true()`. Un cœur sans ces colonnes n’étant plus
installable, un test qui se saute silencieusement n’aurait plus rien à
excuser : il masquerait une installation cassée au lieu de la signaler.

#### Le cœur n’a pas livré une fiche, il en a livré 41

Le brief annonçait « aujourd’hui un seul indicateur a une fiche : C1 ».
`nemeton 0.192.0` en documente en fait **les 41**, des douze familles,
chacune avec sa vignette pkgdown présente (vérifié : 41 déclarations, 41
`.Rmd`). L’icône apparaît donc sur **chaque** indicateur, pas seulement
sur C1 — et **sans une ligne de code modifiée** dans l’app. C’était tout
l’intérêt de ne rien câbler sur C1 : la livraison massive du cœur est
absorbée telle quelle.

**Un test avait pourtant figé la liste, et il a rougi.** L’exemple du
brief prenait C2 comme cas négatif
(`expect_null(get_indicator_doc("C2"))`), ce que le même brief
interdisait deux paragraphes plus loin — « le test ne doit pas figer la
liste des indicateurs documentés \[…\] sinon il tombera au rouge à la
première fiche ajoutée, pour une bonne nouvelle ». C’est exactement ce
qui s’est produit. Le cas négatif porte désormais sur des codes que le
cœur ne connaît pas (`"unknown_indicator"`, `"ZZ"`), les seuls stables :
tout vrai code d’indicateur est susceptible de gagner une fiche.

#### Ce qui ne change PAS — les gardes défensives restent

J’avais annoncé leur retrait comme corollaire du bump. C’était une
erreur de lecture, corrigée ici plutôt que propagée :

- **Le test de longueur sur `doc_url` dans `doc_icon()` reste.** Sa
  raison n’a jamais été seulement le cœur ancien. Il protège la *forme*
  de `row` : une tranche vide (`ind[ind$code == "ZZ", ]`, un code
  inconnu) rend `character(0)`, et `is.na(character(0))` vaut
  `logical(0)`, qu’un `if` refuse. Ce cas ne dépend d’aucune version du
  cœur. `doc_icon()` étant documentée comme acceptant une ligne de
  `indicator_labels()` telle quelle, le retirer aurait été une
  régression déguisée en nettoyage.
- **`pick()` reste** dans `.build_indicator_families()` : c’est le
  helper générique déjà employé par `bilingual()`, pas une précaution
  ajoutée pour les fiches. L’en écarter pour les seules colonnes `doc_*`
  aurait introduit une incohérence.

Seuls les commentaires qui justifiaient ces deux gardes par « cœur \<
0.192.0 » sont réécrits : la raison invoquée était devenue fausse, la
garde ne l’est pas.

## nemetonshiny 0.142.0 (2026-08-27)

#### Added — Une icône « fiche » à côté du « i », pour les indicateurs documentés

Spec 052 côté cœur (`brief-nemetonshiny.md`). Dans l’onglet **Familles
d’indicateurs**, chaque indicateur porte déjà un « i » qui ouvre son
infobulle. Les indicateurs qui disposent d’une **fiche longue** — une
vignette pkgdown du cœur — gagnent une seconde icône juste à côté, qui
l’ouvre dans un nouvel onglet. Un seul indicateur en a une aujourd’hui,
**C1 — Biomasse carbone**.

**Rien n’est câblé sur C1.** Ni l’URL, ni la liste des indicateurs
documentés, ni la langue des fiches ne sont écrites ici : les quatre
viennent des colonnes `doc_url` / `doc_lang` / `doc_url_fr` /
`doc_url_en` que
[`nemeton::indicator_labels()`](https://pobsteta.github.io/nemeton/reference/indicator_labels.html)
expose depuis la v0.192.0, lues par `.build_indicator_families()` comme
le sont déjà libellés et infobulles. Le jour où le cœur publie une
deuxième fiche, l’icône apparaît sans qu’on touche à l’app — et c’est
aussi pourquoi les tests vérifient le *mécanisme* sur C1 (documenté) et
C2 (non documenté) plutôt qu’un décompte de fiches, qui rougirait à la
première bonne nouvelle.

**La fiche peut être servie dans l’autre langue, et l’interface le
dit.** Quand elle n’existe pas dans la langue courante, le cœur rend
l’autre plutôt que rien — une fiche dans la mauvaise langue vaut mieux
que pas de fiche. C’est le cas de C1 aujourd’hui : en anglais,
l’infobulle du lien se lit *« Open the detailed fact sheet (new tab) (in
French) »*. La mention disparaîtra d’elle-même le jour où la traduction
sera écrite côté cœur.

Trois clés i18n (`indicateur_fiche_ouvrir`, `langue_fr`, `langue_en`),
un `<a target="_blank" rel="noopener noreferrer">` — la fiche est
longue, l’ouvrir en place perdrait l’état du calcul en cours.

**Le plancher `Imports: nemeton` reste à 0.189.0.** La release cœur
0.192.0 n’est pas encore publiée ; épingler dès maintenant rendrait
l’app non-installable. Sur un cœur antérieur, `doc_url` n’existe pas,
aucune entrée n’est construite et l’icône ne s’affiche simplement pas —
l’absence de fiche est la condition d’affichage, pas une erreur. Deux
gardes tiennent ce cas : le test de longueur sur `doc_url` dans
`doc_icon()` (sans lui, `is.na(NULL)` rend `logical(0)`, qu’un `if`
refuse) et le repli en `NA` de `pick()` côté config.

**Un piège à connaître en recette** : `doc_url` est déjà correcte avant
que la PR cœur ne soit mergée, mais la page pkgdown n’est déployée qu’au
push sur `main` du dépôt cœur. Tant que ce merge n’a pas eu lieu, le
lien mène à un 404 transitoire.

## nemetonshiny 0.141.1 (2026-08-27)

#### Fixed — Un douzième test d’arbre source rendait encore R-CMD-check rouge

`test-indicator-families-defork.R` lisait `../../R/app_config.R` en
direct. `R/` ne survit pas à l’installation : sous `R CMD check`, les
tests tournent depuis `<pkg>/tests/` et
[`readLines()`](https://rdrr.io/r/base/readLines.html) échoue sur «
cannot open the connection ». Le passage en `helper-sources.R` livré en
v0.140.1.9001 avait traité onze fichiers et manqué celui-là — il
suffisait à lui seul à faire échouer le job `R-CMD-check` de `main` sur
la release v0.141.0, pendant que la suite locale et le job `tests`
(arbre source) annonçaient 12 566 PASS.

Même remède que les onze autres : `chemin_source()` +
`skip_sans_sources()`. Le test s’exécute en local, saute sous le paquet
installé.

## nemetonshiny 0.141.0 (2026-08-27)

Jalon : le cycle dev 0.140.1.9001 → .9004 est consolidé ici. Le MINOR
vient du retour au cœur du rattachement du reliquat (brief cœur
v0.189.0) ; le reste est correctif.

#### Changed — Le rattachement du reliquat retourne au cœur

Implémente `briefs/vers-nemetonshiny/2026-08-26-cœur-v0.189.0.md`.
`Imports: nemeton (>= 0.189.0)`.

La règle livrée en v0.140.0 — chaque bout de parcelle cadastrale sans
numéro rejoint la parcelle forestière avec laquelle il partage la plus
longue frontière, une parcelle sans voisin devient sa propre UGF — vit
désormais dans `croiser_parcelles_onf(rattacher_reste = TRUE)`. L’app la
portait faute de mieux, en écart assumé à sa propre règle « aucune
logique métier » ; elle est rendue. `.onf_rattacher_reste()` et
`.onf_singleparts()` disparaissent.

Le cœur signale au passage un piège que mon implémentation frôlait : sur
un mélange POLYGON/MULTIPOLYGON, `sf::st_cast("POLYGON")` ne garde que
le **premier** polygone de chaque multipartie, sans erreur ni
avertissement — 13,74 ha sur 50,34 évaporés dans sa première écriture.
Le passage par MULTIPOLYGON d’abord est obligatoire.

**La part forestière change de source.** Elle était lue sur la table de
croisement (`surface_ha` et `hors_ugf`) ; avec le rattachement, plus
aucune ligne ne porte `hors_ugf = TRUE` et la table ne peut plus dire
quelle part d’une parcelle était numérotée. Elle est maintenant mesurée
**en amont**, en intersectant directement cadastre et parcellaire — ce
qui est aussi plus juste : la table de croisement a subi le calage et
l’absorption des échardes, qui déplacent de la surface pour des raisons
étrangères à la couverture forestière.

Le message « X ha de votre sélection hors forêt publique » décrivait une
situation qui n’existe plus. Il dit maintenant ce que le rattachement
**a fait** : « X ha non numérotés par le parcellaire ONF ont rejoint les
parcelles forestières voisines ».

#### Fixed — Les houppiers retrouvent leur emprise et leur résolution

Le contournement de v0.140.0 — `aoi = NULL` et `max_cells = 5e6` forcé —
était le seul chemin mesuré comme fonctionnel, faute de connaître la
cause. Le cœur matérialise désormais le raster lui-même
(`nemeton 0.189.0`) : l’appel redevient normal, avec emprise, et sans
budget de cellules imposé. On récupère les 0,50 m au lieu de 2 m, et la
segmentation cesse de porter sur 1 169 ha de dalles pour 637 ha de
parcelles.

#### Added — S3 population : la grille INSEE est câblée

`load_insee_population_source()` entre dans le résolveur de couches, au
même titre que `bdforet` ou `roads`. Sans elle, S3 restait `NA` partout
— voulu depuis `nemeton 0.187.0`, qui ne fabrique plus aucune valeur :
l’ancien chemin « proxy » rendait `surface_du_tampon × 100 hab/km²`, un
nombre qui variait plausiblement avec la taille de l’unité et passait
donc pour une mesure.

Le câblage tient en deux points, et le second est celui qui décide : la
couche est déclarée et téléchargée comme les autres, **et** la grille
est passée à `indicateur_s3_population()` par injection nommée. Le
dispatcher du cœur filtre les arguments sur les formals de la fonction
cible, or celle-ci déclare `population_grid` mais ni `layers` ni `...` :
sans cette injection la grille ne l’atteint jamais et S3 reste `NA`, en
silence.

Le cache national du cœur (`~/.cache/nemeton/insee/`, ~52 Mo, une fois
par machine) porte la grille entière ; le cache projet reçoit l’extrait
découpé, pour qu’un recalcul ne relise pas 52 Mo afin d’y retailler les
mêmes cellules.

S3 change aussi de grandeur côté cœur — une **densité** (hab/km² dans 5
km) au lieu d’un effectif, sur échelle logarithmique. L’ancienne
normalisation saturait à 10 000 habitants quand Couchey en compte 46 110
: 100/100 pour une bourgogne rurale, et pour presque toute forêt
française.

#### Fixed — L’export Marculus partait sans desserte quand seule l’Accessibilité avait tourné

`.marculus_desserte()` ne lisait que le cache de l’onglet Desserte
(`cache/desserte/*.gpkg`). Un projet dont le réseau vient de l’onglet
Accessibilité — même acquisition
[`foretaccess::acquire_desserte()`](https://pobsteta.github.io/foretaccess/reference/acquire_desserte.html),
rangée dans `cache/accessibility/accessibilite.gpkg` — n’a jamais ce
répertoire : la fonction rendait `NULL`, `marculus_write_action_gpkg()`
n’écrivait alors aucune table `desserte`, et l’opérateur ouvrait sur son
téléphone une couche vide alors que le réseau était sur le disque.
Constaté sur le projet Fordead, dont la couche `desserte` de
l’Accessibilité porte 1 748 tronçons.

L’Accessibilité sert désormais de **repli** — lue seulement quand les
quatre couches de l’onglet Desserte ne donnent rien. Repli et non union
: quand les deux onglets ont tourné, ils redisent la même BD TOPO, celle
de l’onglet Desserte en plus corrigée ; les cumuler doublerait le réseau
sur le téléphone. La lecture d’une couche est sortie en
`.marculus_read_desserte()`, partagée par les deux sources, ce qui donne
au repli la reprojection en 4326 et la colonne `nom` absente traitées
comme ailleurs.

#### Fixed — La notification du moteur reGénération oubliait la phase au pire moment

`.regen_read_phase()` jetait tout `engine_status.json` vieux de plus de
2 min, et la notification bas-droite retombait alors sur « Moteur
reGénération en cours… ». Or le cœur n’émet qu’**un** événement par
année ERA5 (`.rsen_moyenne_categorie()`), puis télécharge douze mois
auprès de Copernicus sans un mot — mesuré à 1 h 36 pour l’année 2020 du
projet Fordead, ~8 min par mois. La phase réelle était donc effacée
précisément pendant le plus long moment du run, celui où l’on se demande
si quelque chose tourne encore.

La lecture ne juge plus : elle porte l’âge de la dernière écriture
(`stale_s`), et le libellé dit le silence au lieu de le cacher — «
Microclimat — étés canicule 2022 (1/1) — dernier signe de vie il y a 27
min ». Le seuil de 2 min est conservé, mais il déclenche un aveu, plus
un oubli. La protection contre une phase fantôme d’un run précédent ne
tenait de toute façon pas à cette péremption : `engine_status.json` est
effacé au lancement et en fin de tâche, et le poll est gardé par
`rv$engine_running`.

#### Added — Compteur de mois ERA5 (en attente du cœur)

`regen_expo:era5_mois` est mappé et affiché — « Microclimat — étés
canicule 2022 — mois 3/12 ». Chaque morceau du libellé est optionnel :
tant que le cœur n’émet pas cet événement, la branche est morte et rien
ne change. Le brief cœur correspondant est
`specs/BRIEF-nemeton-era5-progression-mensuelle.md`, qui demande aussi
la reprise d’un cache ERA5 partiel — aujourd’hui un run tué au mois 7
rend l’année entière irrécupérable, `mcera5::request_era5()` refusant un
`.zip` déjà présent.

#### Fixed — R-CMD-check était rouge depuis trois jours, pour une raison mécanique

Douze runs consécutifs en échec sur `main` du 23 au 26 août, pendant que
la suite locale annonçait 12 510 PASS. Onze tests lisent l’**arbre
source** — `R/mod_ug.R`, `CLAUDE.md`, `inst/app/www/css/custom.css` —
via `test_path("..", "..", ...)`, sans garde. En local
[`pkgload::load_all()`](https://pkgload.r-lib.org/reference/load_all.html)
les trouve ; sous `R CMD check` les tests tournent depuis le paquet
installé, où `../../R/` ne mène nulle part, et
[`readLines()`](https://rdrr.io/r/base/readLines.html) échoue sur
*cannot open the connection*.

Un helper nomme le motif une fois plutôt que de le recopier treize fois,
et il distingue deux situations qui ne se valent pas :

- **`inst/` survit à l’installation**, sous un autre chemin :
  `chemin_inst()` interroge
  [`system.file()`](https://rdrr.io/r/base/system.file.html) d’abord,
  donc le test **s’exécute** en CI au lieu d’y être sauté.
- **`R/` et la racine du dépôt ne survivent pas** :
  `skip_sans_sources()` saute, faute de mieux. Un test sauté vaut mieux
  qu’un test rouge, mais il ne vérifie plus rien là-bas — c’est le prix
  d’un test d’arbre source, payé sciemment.

Le test du helper lui-même a d’abord été écrit avec
`expect_error(..., class = "skip")` : le skip se **propage** et sautait
le test, sans évaluer une seule assertion. Réécrit en attrapant la
condition.

## nemetonshiny 0.140.1 (2026-08-26)

#### Fixed — Un test ne parsait plus, et ma vérification ne le voyait pas

`test-parcelles-csv.R` portait `grepl("^\s*#", ...)` au lieu de
`grepl("^\\s*#", ...)` : en R, `"\s"` n’est pas une séquence
d’échappement valide, et le fichier entier cessait d’être analysable.
R-CMD-check l’a vu,
`Error: '\s' is an unrecognized escape in character string`.

**Ce qui m’a échappé, et qui compte plus que la coquille** : je filtrais
la sortie de la suite sur `[failure]` et `[error]`. Un fichier qui ne
*parse* pas n’apparaît sous aucune des deux étiquettes — il est
simplement absent du compte. Ma vérification annonçait donc « 0 échec »
sur une suite dont un fichier n’avait pas été exécuté du tout.

Vérifié cette fois-ci sur les **106 fichiers de test**, un
[`parse()`](https://rdrr.io/r/base/parse.html) chacun.

## nemetonshiny 0.140.0 (2026-08-26)

#### Changed — Une UGF est faite de parcelles cadastrales, et de rien d’autre

Règle posée par Pascal : le parcellaire ONF n’est pas un filtre, c’est
une **source d’étiquettes**. On le croise pour récupérer le numéro de
parcelle forestière, puis on le jette. Ce qui fait foi, c’est le
cadastre — les UGF sont des regroupements, des découpes ou des parcelles
cadastrales entières, et **rien d’une parcelle cadastrale n’est jamais
écarté**.

L’UGF « Hors forêt publique » disparaît. Ses 49,68 ha à Couchey
n’étaient pas hors forêt : c’étaient des layons, des routes, les
interstices entre parcelles forestières adjacentes et le jeu de
numérisation le long des limites — de la forêt que le parcellaire ONF
n’avait simplement pas numérotée.

**Chaque bout rejoint la parcelle forestière avec laquelle il partage la
plus longue frontière.** Longueur, jamais surface ni distance : un layon
qui longe la parcelle 3 sur 400 m et effleure la 4 par un coin
appartient à la 3. Un contact ponctuel n’est pas un voisinage.

L’alternative « tout à l’UGF dominante » a été écartée sur mesure, pas
par principe. Sur `212000000A0036` — **une** parcelle cadastrale qui
porte **28** parcelles forestières — la dominante absorberait 10,89 ha
de bandes situées à l’autre bout de la parcelle, grossissant de 77 % sur
des terrains qu’elle ne touche pas. La règle du voisinage répartit ces
mêmes bandes entre 6 UGF.

Le reliquat est d’abord **éclaté en parties simples** : le cœur le rend
fusionné en une ligne par parcelle, et à cette granularité « le voisin »
n’a pas de sens.

Sur Couchey (21 parcelles, parcellaire ONF réel) :

|                                   | Avant                      | Après         |
|-----------------------------------|----------------------------|---------------|
| UGF                               | 75                         | **74**        |
| dont « Hors forêt publique »      | 1 (72 tènements, 49,68 ha) | **0**         |
| Tènements                         | 223                        | **165**       |
| Surface totale                    | 535,13 ha                  | **535,13 ha** |
| Écart de surface **par parcelle** | —                          | **0 m²**      |

Une parcelle du CSV qu’aucune parcelle forestière ne touche garde **sa
propre UGF**, nommée par sa référence cadastrale — jamais versée dans un
fourre-tout, qui ferait une unité de gestion qui n’en est pas une.

#### Changed — L’import CSV ne purge plus rien

Un CSV liste la forêt : ses parcelles **sont** la forêt, toutes. En
supprimer contredirait le fichier que l’utilisateur vient de fournir. Le
réglage « Supprimer les parcelles » reste offert au bouton ONF, où la
sélection est faite à la main sur la carte et peut déborder.

Ce qui réglait le défaut d’origine — une UGF « Hors forêt publique »
survivant à l’import — n’est plus la purge mais le rattachement. Rien
n’étant mis de côté, il ne reste rien à purger.

La purge lit désormais la part forestière **relevée pendant le
croisement** (`.onf_part_foret()`) et non plus l’UGF résiduelle, qui
n’existe plus.

#### Added — Le bloc « Calculs terminés » se replie comme les autres

Demande de Pascal. C’était le seul bloc du sidebar de Sélection qu’on ne
pouvait pas replier — et c’est celui qui porte le tableau des
indicateurs, donc le plus haut, celui qui repousse tout ce qui se trouve
dessous. Même en-tête cliquable, même chevron, ouvert par défaut :
replier reste un geste, pas un état initial.

#### Fixed — Les houppiers ne se calculaient sur aucun projet

Deux défauts indépendants, tous deux constatés sur le projet « Fordead »
dont le calcul d’indicateurs venait d’aboutir sans produire la moindre
couche houppier.

**Le mauvais modèle de hauteur était choisi.** La résolution ne
regardait que `cache/layers/opencanopy/` et prenait le premier fichier
existant. Or les quatre rasters Open-Canopy de ce projet sont **plats**
— toutes leurs valeurs entre 0 et 0,20 m — pendant que le MNH LiDAR HD
du même cache affiche une médiane de 20,7 m. La segmentation tournait
142 s pour rendre 0 houppier, en silence, à la fin de chaque calcul. La
résolution passe par
**[`nemeton::resolve_project_chm()`](https://pobsteta.github.io/nemeton/reference/resolve_project_layers.html)**,
le résolveur canonique — celui du plan d’échantillonnage — qui préfère
LiDAR HD à Open-Canopy. Plus un garde : un modèle de hauteur sans
hauteur n’en est pas un.

**lidR refuse un raster qui vit sur disque.** Segmenter une dalle isolée
le dit mot pour mot : *« Cannot segment the trees from a raster stored
on disk »*, et une reproduction minimale le confirme — le même CHM
synthétique segmente en mémoire et échoue dès qu’on le relit depuis un
GeoTIFF. C’est aussi pourquoi forcer l’agrégation marche :
[`terra::aggregate()`](https://rspatial.github.io/terra/reference/aggregate.html)
rend son résultat en mémoire.

Le `st_crs(x) == st_crs(y) is not TRUE` que deux rapports ont pris pour
la cause en est un symptôme : lidR convertit un raster sur disque en
`RasterLayer`, et
[`sf::st_crs()`](https://r-spatial.github.io/sf/reference/st_crs.html)
d’un `RasterLayer` rend un proj4string là où un `SpatRaster` rend un WKT
propre. Les deux ne comparent plus égal, et le `st_crop()` interne de
lidR s’arrête là.

Contournement : passer le raster du résolveur **sans emprise**, avec
`max_cells = 5e6` au lieu du défaut 2e7. Mesuré sur Fordead (80 M
cellules, 637 ha), c’est le seul des quatre chemins essayés qui aboutit
— **224 614 houppiers en 870 s**, `h_max` médian 25,5 m, mis en cache
pour l’export Marculus. Le prix est double et assumé : la résolution
(0,50 m travaillé à 2 m, ce qui suffit pour pré-remplir la hauteur d’une
tige, où l’on veut l’apex au-dessus de la tige et non la forme du
houppier) et l’emprise (on segmente la dalle entière, chaque contexte
étant de toute façon découpé sur ses parcelles à l’export).

**Un cas reste inexpliqué et le brief le dit** : un raster croppé puis
agrégé à la main revient `inMemory = TRUE`, avec des CRS identiques
entre apex et modèle, et échoue quand même. Ni la mémoire ni le CRS n’en
rendent compte. Deux briefs déposés côté cœur ; le contournement partira
quand le cœur matérialisera le raster, et l’app retrouvera l’emprise et
les 0,50 m.

#### Fixed — Les messages de purge annonçaient un seuil qui n’était plus le sien

Le constat qui a ouvert tout ce lot : après un import CSV à Couchey, une
UGF « Hors forêt publique » subsistait — 72 tènements, 49,68 ha — sans
explication. La purge n’était pas en panne. Le seuil vaut **0 %**,
c’est-à-dire « ne retirer que ce que la forêt ne touche pas du tout »,
et les **21 parcelles du projet touchent toutes** de la forêt publique,
la plus faible à 5,05 %. Il n’y avait rien à prendre.

La bonne réponse n’était donc pas de purger davantage mais de
**rattacher** — c’est l’objet de ce lot. Reste de ce diagnostic : les
messages annonçaient « sous 10 % » **en dur**, la valeur d’un défaut qui
a changé depuis. Ils affichent le seuil réel, et les deux chemins qui
croisent le parcellaire rendent compte de la même façon.

## nemetonshiny 0.139.0 (2026-08-25)

#### Changed — Chaque indicateur est enregistré deux fois : brut et normalisé

Un indicateur répond à deux questions — « combien ? » dans son unité
propre, et « où cela situe-t-il cette UGF ? » sur 0–100. N’en garder
qu’une coûtait des deux côtés : chaque consommateur renormalisait à sa
façon, et l’écran montrait un NDVI de **0,227** à côté de 75 %
d’ancienneté comme si les deux échelles n’en faisaient qu’une.

La normalisation est celle du **cœur, par indicateur**
(`normalize_indicator()`), à bornes **absolues** : deux projets restent
comparables. Un min-max ferait de chaque projet son propre étalon, et le
même peuplement scorerait différemment selon ses voisins.

C’est aussi la fonction qu’emploie `create_family_index()` — et celle-là
**préfère les colonnes `_norm` quand elles existent**. Persister le
jumeau fait donc coïncider la valeur stockée et la valeur affichée, au
lieu de laisser deux vérités dériver. **Conséquence visible** : l’onglet
Familles d’indicateurs passe des valeurs brutes aux valeurs normalisées.

Seuls les indicateurs que le cœur **déclare** reçoivent un jumeau.
`normalize_indicator()` rend un indicateur inconnu **inchangé** ; écrire
cela en `_norm` serait un mensonge par omission — la colonne annoncerait
0–100 en portant des mètres ou des habitants, et la vue Famille la
préférerait au brut.

**Côté base** : `inst/sql/migration_006_indicateurs_norm.sql` (31
colonnes, idempotent). **À appliquer à la main** : les fichiers
`migration_00N.sql` ne sont pas joués par l’application, seul
`schema.sql` l’est. L’écriture devient donc **tolérante** à un schéma en
retard — sans ce filtre, `dbWriteTable(append = TRUE)` échouerait et
l’on perdrait *aussi* les valeurs brutes, pour une colonne d’appoint
manquante.

#### Changed — Un indicateur non calculé ne prend plus de colonne

Une colonne entièrement vide occupait une carte, une colonne du tableau
et une case du radar pour ne rien dire — et laissait croire à une mesure
**nulle** plutôt qu’à une mesure **absente**. Elle sort de la vue.

Les colonnes de **statut** des indicateurs écartés sont conservées :
sans elles, la raison de leur absence disparaîtrait avec eux, et le
bandeau explicatif n’aurait plus rien à lire.

#### Fixed — L’import CSV croisait mais ne purgeait pas

Implémente
`briefs/vers-nemetonshiny/2026-08-25-csv-purge-hors-foret.md`.
`onf_purger_hors_foret()` n’était appelée qu’à **une seule ligne de
toute l’application** — celle du bouton ONF. L’import CSV laissait donc
subsister l’UGF « Hors forêt publique » : 74 tènements, 50,15 ha sur les
535,59 ha de Couchey.

**Le correctif naïf aurait été faux**, et le brief le disait : la purge
retire des **parcelles**, pas seulement des tènements. `save_ug_data()`
seul les laisserait revenir au prochain chargement — le défaut payé en
v0.130.7. Le chemin CSV passe donc par `.onf_commit(with_parcels = …)`,
le même mécanisme testé que le bouton.

Le réglage vient du **projet**, pas d’une coche propre à ce chemin : il
vaut quel que soit ce qui crée les UGF. Mais l’utilisateur vient de
fournir ces parcelles dans son fichier — l’import **dit** donc ce qu’il
retire.

#### Fixed — Quatre défauts signalés sur capture

- Le bloc ONF des paramètres **n’avait pas de cadre** : `mt-3` au lieu
  du `mt-3 p-2 border rounded` de ses voisins.
- Des `<strong>` s’affichaient **tels quels** dans les infobulles.
  `info_popover()` n’interprète pas le HTML — les infobulles du dépôt
  sont du texte, et j’étais le seul à y avoir mis des balises.
- **L’import CSV ne rafraîchissait pas les sous-onglets de Sélection.**
  Il émet bien `restore_project`, mais les parcelles de `mod_home` sont
  une réactive **locale** que lui seul peut poser : la carte et la
  recherche de commune suivaient le signal, les sous-onglets qui lisent
  `parcels()` continuaient d’afficher le projet **précédent**. Relais
  ajouté, avec un test qui vérifie aussi qu’un signal sans parcelles ne
  vide pas l’écran.
- L’infobulle distingue enfin **deux découpes opposées** que son libellé
  confondait : écarter le parcellaire ONF hors cadastre n’est pas
  retirer les bouts de parcelle cadastrale hors forêt — ça, c’est la
  purge.

#### Vérifié sur données réelles

Sur Couchey, 218 tènements et 530 ha : **0 m² de tènement hors de sa
parcelle cadastrale**. L’UGF est bien une partie ou un regroupement de
parcelles cadastrales, jamais autre chose — le parcellaire ONF n’apporte
que le numéro de parcelle forestière et son groupe.

## nemetonshiny 0.138.1 (2026-08-23)

#### Changed — Les houppiers se calculent avec les indicateurs, plus au téléchargement

La segmentation coûte **173 s**, et borner l’emprise n’y change presque
rien (162 s sans) — la dalle est lue et ré-échantillonnée dans les deux
cas. Dans un `downloadHandler`, c’étaient 173 s de session gelée à
chaque export.

Elle part donc à la **fin du calcul des indicateurs**
(`precompute_houppiers()`) : on y est déjà dans l’enfant plafonné, après
un travail qui se compte en heures, et le CHM vient d’être produit. Le
résultat est mis en cache dans `cache/layers/houppiers/houppiers.gpkg`,
et l’export se contente de le **lire** — le lot redevient affaire de
secondes.

**Best-effort de bout en bout.** Un projet sans CHM, un cœur sans
`segment_houppiers()`, une segmentation qui échoue : aucun de ces cas ne
fait échouer un calcul d’indicateurs qui, lui, a abouti. Un projet
calculé avant ce mécanisme n’a simplement pas la couche, et le
GeoPackage reste valide — le téléphone ne pré-remplit alors pas les
hauteurs.

#### La couche n’apparaîtra pas encore, et ce n’est pas le câblage

Deux obstacles, tous deux côté cœur, tous deux signalés :

**`v0.184.0` n’est ni taguée ni publiée.** `gh release list` s’arrête à
`v0.183.1` : tant que le tag n’existe pas, `Remotes: @*release` ne peut
rien en tirer. Le plancher n’est donc pas relevé et le câblage dégrade
en silence.

**Et l’arbre de développement du cœur a régressé aujourd’hui.**
`segment_houppiers()` rendait 22 435 houppiers en 173 s ce matin sur la
version locale `0.184.0` ; sur `0.184.0.9000`, le même appel sur la même
donnée échoue en `st_crs(x) == st_crs(y) is not TRUE`, levé depuis
`lidR`. Reproduit quatre fois. Ce n’est **pas** un problème d’emprise —
sans emprise, l’échec est le même — ni de CRS d’AOI : aligner l’AOI sur
le CRS exact du raster ne change rien. Brief déposé avec la reproduction
minimale (`briefs/vers-nemeton/2026-08-23-houppiers-regression-crs.md`).

Un repli sur la dalle entière est tenté quand l’appel avec emprise
échoue. Inutile aujourd’hui puisque les deux échouent — correct dès que
la régression sera levée.

## nemetonshiny 0.138.0 (2026-08-23)

#### Added — La couche `houppier` entre dans le lot Marculus

[`nemeton::segment_houppiers()`](https://pobsteta.github.io/nemeton/reference/segment_houppiers.html)
(cœur v0.184.0) est câblé : le téléphone pré-remplit désormais la
**hauteur** d’une tige — point-dans-polygone sur la position GNSS,
lecture de `h_max`, valeur modifiable.

Calculée **une fois par projet**, chaque contexte gardant les houppiers
que ses parcelles recouvrent. **Intersection et non découpe** : un
houppier à cheval sur la limite garde son contour entier — le rogner
déplacerait son centroïde et rétrécirait le polygone dans lequel une
tige doit tomber, l’estimation raterait justement les arbres de bord.

L’emprise est passée au cœur, le CHM en cache étant une dalle LiDAR HD
entière : sur Couchey, 1 674 ha de dalle pour 536 ha de parcelles, 46
158 houppiers sans emprise contre 22 435 avec. Le CRS est **retamponné**
avant écriture — le MNH porte le *nom* « EPSG:2154 » sans bloc
d’autorité, et la couche serait partie avec un CRS que le téléphone ne
sait pas rattacher.

**Réserve, à traiter avant de considérer le chemin utilisable** : 173 s
de calcul, et l’emprise n’y change presque rien (162 s sans). C’est trop
pour un `downloadHandler`, qui bloque la session. Le plancher n’est
**pas** relevé, `v0.184.0` n’étant pas encore taguée : le câblage
dégrade en silence sur un cœur plus ancien, et le GeoPackage reste
valide sans la couche.

#### Changed — Les calibrages du croisement ONF passent dans les paramètres

Domanialité retenue, purge des parcelles peu forestières et son seuil
quittent la barre de Carte UGF pour **Paramètres › Sources &
paramètres**, persistés par projet. Ce sont des calibrages, réglés une
fois par massif, alors que le bouton de croisement est un geste qu’on
répète. La sidebar garde le **rappel** des valeurs en vigueur et le
chemin pour les changer.

**Le seuil devient paramétrable, à 0 % par défaut, et la purge est
cochée.** Cela imposait de passer la comparaison de `<` à `<=` : avec
`<`, un seuil de 0 % ne supprimait *rien*, pas même une parcelle sans un
mètre carré de forêt — un réglage inerte à sa propre valeur par défaut.
À 0 %, ne partent donc que les parcelles que la forêt publique ne touche
pas du tout, ce qui rend la coche par défaut défendable.

S’y ajoute **l’écartement des débordements ONF hors cadastre** (coché
par défaut). Vraie intersection, pas un filtre : une parcelle forestière
à cheval est **coupée**, pas rejetée — la rejeter cacherait la forêt
réellement présente sur la parcelle.

#### Fixed — Un pipeline Open-Canopy interrompu ne fait plus tout recommencer

Le garde-fou du cache testait `chm_1_5m.tif`, un **témoin** que l’app
écrit *après* que le pipeline a rendu ; le pipeline, lui, écrit
`chm_predicted_1_5m.tif` au fil de l’eau. Toute interruption — OOM,
annulation, coupure — laissait donc un cache complet que le prochain
essai ignorait, et tout repartait du téléchargement des orthophotos.

Constaté sur Couchey : tué à sa dernière étape après **3 h 20 de CPU**,
11 Go de produits valides sur le disque, relance intégrale. Le livrable
du pipeline est désormais adopté quand il est là.

#### Fixed — Le bouton IA du Plan d’actions était resté vert

Signalé par l’utilisateur : « Générer les actions (IA) » portait
`btn-primary` et une baguette magique, là où la règle veut `btn-ia` et
les trois étoiles.

Le défaut est plus large que l’apparence. La v0.130.10 annonçait que
l’accent couvrait « toute l’app » en nommant quatre surfaces — Synthèse,
Plan d’actions, reGénération, Famille. **Trois avaient un test, celle-là
n’en avait pas** : rien ne vérifiait ce que l’entrée de NEWS affirmait.
Deux boutons sont corrigés, pas un : celui du panneau et celui qui
**lance** la génération dans la modale, le laisser vert aurait fait
passer d’un ambre à un vert pour un seul geste. Un test de source
énumère désormais les surfaces génératrices et interdit les deux
signalétiques que l’ambre a remplacées.

Et une clé i18n manquante, trouvée **en lançant l’app** : le bouton
d’enregistrement du nouveau bloc portait `i18n$t("save")`, une clé qui
n’existe pas — il affichait sa clé brute.

#### Changed — Les contextes sont nommés par leur parcelle forestière

« Couchey — ug_20260822203555_001 — coupe_rase » devient **« Couchey -
parcelle 1 - coupe_rase »**. L’identifiant interne ne dit rien à un
marteleur : sur un téléphone la liste des contextes est plate, et il
connaît sa parcelle forestière, pas le rang qu’elle occupe dans une
table. Le libellé est celui que le croisement ONF a déjà écrit.

Le nom de la forêt est retiré **quand il répète celui du projet** — «
Couchey - Forêt communale de Couchey — parcelle 1 » disait Couchey deux
fois pour rien. Sans libellé (projet ancien, groupe jamais nommé),
l’identifiant reste : il est pauvre, un milieu vide serait pire.

#### Changed — « Télécharger vers Marculus »

Le bouton d’export prend ce libellé en français, sur demande. L’anglais
garde *for*, plus juste : rien ne part vers Marculus, le bouton dépose
un ZIP dans les téléchargements.

## nemetonshiny 0.137.0 (2026-08-23)

#### Changed — Le bouton dit ce qu’il fait

**« Télécharger pour Marculus »**, et non plus « Envoyer vers Marculus
». Il n’y a pas d’envoi : le bouton fabrique un ZIP et le remet au
navigateur. Aucun canal de push n’existe, ni ici ni dans Marculus, qui
lit des fichiers depuis le stockage de l’appareil. Le nom promettait un
appairage que l’utilisateur aurait cherché en vain.

Le chemin le plus court reste d’ouvrir Nemeton **dans le navigateur du
téléphone** : le téléchargement atterrit alors directement dans les
*Téléchargements* de l’appareil. Sinon Quick Share, un cloud, ou le
câble.

#### Added — `gpkgNom` : de quoi apparier un chantier et sa carte

Chaque contexte du `.marsync` porte désormais le **nom de fichier** de
son GeoPackage dans le lot. Le champ est inerte aujourd’hui — c’est une
clé inconnue de toutes les versions publiées de Marculus, et
`JSONObject` ignore ce qu’il ne lit pas — mais c’est ce qui permettra à
l’application de faire elle-même l’appariement.

**Le problème qu’il vise.** Réceptionner un lot demande aujourd’hui
d’ouvrir le `.marsync`, puis de rattacher **treize GeoPackages à la
main**, depuis un sélecteur de fichiers, sur treize noms qui se
ressemblent (`..._ug_1_-_eclaircie.gpkg`, `..._ug_10_-_eclaircie.gpkg`).
Une erreur d’appariement **ne se voit pas** : le contexte s’ouvre, la
carte affiche une parcelle — la mauvaise — et les tiges se rattachent
spatialement à un périmètre qui n’est pas le leur. Le journal devient
faux sans jamais avoir été en erreur.

Deux propriétés garanties à l’émission, et testées : c’est un **nom
nu**, jamais un chemin
([`basename()`](https://rdrr.io/r/base/basename.html) est appliqué, un
`../` ne peut pas sortir), et il est **ASCII** — ni accent ni espace —
pour traverser un ZIP et un système de fichiers Android sans surprise.
Un test vérifie en outre que chaque contexte du lot désigne un fichier
**présent**, et que deux actions sur la même UGF ne partagent pas un nom
: le second écraserait le premier, et un contexte pointerait sur le
chantier de l’autre.

#### Added — Brief pour Marculus : réceptionner le lot

`specs/BRIEF-marculus-import-zip.md` demande une entrée « Importer un
lot (.zip) » : décompresser, fusionner les contextes, rattacher chaque
GeoPackage par son `gpkgNom`. Rien à synchroniser entre les deux côtés —
le champ étant inerte, Marculus peut l’implémenter quand il veut.

Le brief insiste sur trois choses qui doivent rester vraies : passer par
`fusionnerJson()` et **jamais** `importerJson()`, qui efface tout avant
d’insérer ; **refuser** une entrée d’archive contenant `..` plutôt que
l’assainir ; et traiter un lot amputé d’un fichier en créant quand même
le contexte — douze chantiers valent mieux que zéro, à condition de le
dire.

Il est écrit d’après le **code Android lu**, pas supposé, et trois
constats en découlent. Le `.marsync` **s’importe déjà** : le bouton
*Fusionner* ouvre le sélecteur sur `*/*` et appelle `fusionnerJson()` —
un opérateur qui décompresse le lot sur son téléphone crée ses contextes
sans qu’une ligne change, seuls les rattachements restent manuels. La
**plomberie ZIP existe** aussi (`lireJsonDepuisZip`), donc le travail
demandé est plus petit qu’annoncé. Mais ce lecteur-là alimente
`importerJson()`, la porte destructive : le brief demande de ne pas le
réutiliser tel quel, les deux imports devant cohabiter à quelques lignes
l’un de l’autre dans le même écran.

## nemetonshiny 0.136.0 (2026-08-23)

#### Added — Envoyer les chantiers de martelage vers Marculus

Un bouton **« Envoyer vers Marculus »** dans le bloc *Exports* du Plan
d’actions. Il produit un ZIP : **un GeoPackage par action qui désigne
des tiges**, plus **un fichier `.marsync`** portant tous les contextes
de martelage. Vérifié sur ForêtAccess : 13 actions éligibles → 13
contextes, 13 GeoPackages, desserte incluse, feuille pré-remplie de 10
essences.

**Quelles actions deviennent un contexte.** Le critère est le geste de
l’opérateur, pas l’intention sylvicole : il y a contexte quand quelqu’un
parcourra le peuplement en désignant les tiges une par une. Éclaircie et
coupe rase marquent ce qui part, dépressage ce qui reste, observation
couvre les tournées d’inventaire, qui comptent les tiges de la même
façon. Plantation, desserte, protection en sont absents — rien n’y est
désigné tige à tige, et un contexte vide sur le téléphone est pire que
pas de contexte.

**Le `.marsync` passe par la FUSION, jamais par la restauration.** Côté
téléphone, `fusionnerJson()` fait une union par UUID, non destructive et
atomique : les contextes se créent sans toucher aux tiges déjà saisies.
L’autre porte d’entrée, `importerJson()`, **efface** contextes, tiges et
configs avant d’insérer. Le fichier produit ici ne porte donc pas de
section `referentiels` — c’est la seule chose qui distingue les deux
formats, et la ressemblance serait coûteuse. Un test l’interdit.

**Les noms de couches sont un contrat, pas une convention.** Dans un
GeoPackage Marculus, c’est le **nom de la table** qui dit le rôle :
`desserte` pour les voies, `houppier` pour les hauteurs, et **tout autre
nom est lu comme des parcelles**, rattachement spatial des tiges
compris. Une couche mal nommée ne provoque pas d’erreur : elle remplit
le journal de parcelles fantômes. D’où des noms figés en constantes, et
un test qui vérifie que le fichier ne porte **que** `parcelle` et
`desserte`.

**La desserte est repliée en une seule couche.** L’onglet en produit
quatre — `desserte_existante` (BD TOPO corrigée), `reseau_cree` (les
pistes dessinées par le moteur), `osm_track` et `desserte_detectee`.
Marculus n’en lit qu’une. Ce qui survit au repli est la **provenance**,
dans la colonne `type` : un opérateur qui voit une piste à l’écran doit
savoir si elle existe au sol ou si elle est une proposition sur le
papier.

**Pas d’ortho dans le GeoPackage** — vectoriel seulement. Marculus
prendrait la première table de tuiles comme fond hors-ligne, et les
orthophotos du projet pèsent des gigaoctets.

#### Added — La feuille de martelage part pré-remplie, selon le profil de groupe

Les profils de `groupes_amenagement.yaml` portent désormais une liste
`essences`. ONF, CRPF et OFB ne martèlent pas les mêmes : le peuplier
ouvre une feuille CRPF et n’a rien à faire en tête d’une feuille ONF ;
l’aulne glutineux et le saule appartiennent aux zones humides suivies
par l’OFB. Le profil de groupe est ce que le projet porte de plus proche
d’un contexte sylvicole, et il est déjà choisi par ailleurs.

Les libellés sont écrits **accentués et en toutes lettres** — « Hêtre »,
« Sapin pectiné » — parce que le téléphone normalise (minuscules,
accents ôtés) avant de chercher son coefficient de cubage : la forme
lisible et la forme appariée sont la même chaîne. C’est lui qui dérive
ensuite le code ONF à trois caractères, pas nous. Un test interdit qu’un
code à trois lettres se glisse dans ces listes, et qu’un séparateur du
format (`RS`, `US`) apparaisse dans un libellé — il casserait l’encodage
de la matrice entière.

Une liste explicite passée à l’export prime toujours ; `character(0)`
reste possible et donne une feuille vide, que l’opérateur remplit.

**Deux champs restent vides, délibérément.** `cheminGpkg` est un chemin
dans le stockage privé du téléphone, que cette machine ne peut pas
connaître — l’opérateur rattache le GeoPackage à son contexte sur place.
Et le nom de commune affiché vient de `commune_geometry`, pas de la
colonne `commune` des parcelles : celle-ci porte le **code INSEE**, et
`48042` ne dit rien à un marteleur sur le terrain.

#### Ce qui manque encore — la couche `houppier`

L’estimation automatique des hauteurs demande une segmentation de
couronnes sur MNH (détection des apex + délimitation), c’est-à-dire de
la **logique métier** : elle appartient à `nemeton`, pas ici (règle 1).
Le GeoPackage produit est valide sans elle — le téléphone se contente
alors de ne pas pré-remplir les hauteurs. `lasR`, déjà dépendance de
l’app, expose ce qu’il faut (`chm`, `local_maximum`, `region_growing`,
`hulls`).

Brief émis : `specs/BRIEF-nemeton-houppiers-mnh.md`, avec le point de
vigilance mémoire — ré-échantillonner le MNH avant de segmenter, un
houppier faisant 3 à 10 m de diamètre quand le MNH de Couchey compte 418
M cellules à 0,20 m.

#### Added — Un brief unique de rattrapage pour le cœur

`specs/BRIEF-coeur-rattrapage-2026-08-23.md` regroupe les cinq briefs
ouverts en trois jours : journal `PLAN.md` (quatre releases), houppiers,
`pct_veg` d’`opencanopynemeton`, et le diagnostic OOM/SIGTERM (soldé,
reste à publier). Cinq documents dont deux déjà partiellement traités
font perdre plus de temps qu’ils n’en font gagner : il faut d’abord
établir lequel est encore vrai. Le document unique dit ce qui reste, et
désigne la seule urgence — `pct_veg`, la seule qui empêche aujourd’hui
un calcul d’aboutir.

## nemetonshiny 0.134.1 (2026-08-23)

Implémente
`briefs/vers-nemetonshiny/2026-08-23-reponse-oom-sigterm-scope.md`.
**Aucun plancher relevé** : le message vient du cœur, l’app ne fait que
le relayer.

#### Fixed — La prudence ne s’applique plus à ce qui est certain

Le cœur (`nemeton 0.183.1`) **nomme désormais le scope transitoire** et
demande son verdict à systemd, au lieu de l’inférer d’un code de sortie.
Quand il écrit « ran out of memory », le dépassement est **constaté**.
Or `.compute_error_message()`, livré en v0.134.0, recouvrait ce message
de sa formulation prudente — « la cause habituelle est le plafond
mémoire » — et rendait à l’utilisateur une incertitude que le cœur
venait de lever. Le brief l’avait vu venir ; c’est corrigé.

Quatre messages du cœur, quatre traitements distincts :

| Message du cœur | Ce qui est su | Ce qu’affiche l’app |
|----|----|----|
| `ran out of memory … (ceiling: X)` | `Result=oom-kill` : **certain** | le plafond a été dépassé, affirmatif |
| `was killed (signal N; verdict unavailable)` | tué, cause inconnue | le plafond est la cause *habituelle* |
| `failed … (systemd: "signal")` | systemd dit que ce **n’est pas** la mémoire | passe tel quel |
| autre | erreur R ordinaire | passe tel quel |

La formulation prudente **garde donc un objet**, mais un seul : le mode
dégradé, sans cgroup ni `systemctl`, où un scope arrêté et un `kill`
extérieur ont exactement le même visage qu’un dépassement. Le cas de
l’incident du 2026-08-22 — un `exit -15` nu, sur un cœur antérieur — y
reste rattaché, le plancher n’ayant pas bougé.

**Le plafond en vigueur remonte désormais à l’écran**
(`Plafond en vigueur : 10G`), extrait de l’une ou l’autre des deux
formulations du cœur. C’est lui qu’il faut relever ; le connaître évite
de deviner. Le code de sortie, lui, ne s’affiche toujours pas — il ne
dit rien à personne.

**Un faux positif est explicitement refusé** : quand systemd dit
`signal`, le cœur précise « This is not the memory ceiling » et l’app ne
le contredit pas. C’est exactement le piège que le cœur a évité en
refusant d’élargir la reconnaissance à `-15`, et il aurait été absurde
de le réintroduire par le bas.

#### Note de calendrier

`nemeton 0.183.1` n’est **pas encore publié** au moment de cette release
: le travail cœur est sur la branche `fix/oom-diagnostic-scope-result`,
ni mergé ni tagué. Cette version de l’app est donc prête *pour* lui,
sans en dépendre — aucun changement d’API, aucun plancher à relever, et
les quatre cas ci-dessus sont testés sur les chaînes exactes que le cœur
produira. Les deux premières lignes du tableau ne s’observeront en vrai
qu’une fois `v0.183.1` taguée, que `Remotes: @*release` la tire et
qu’elle soit installée.

#### Reliquat signalé par le cœur

`.reconfort_run_py()` (chaîne RECONFORT) ne rend qu’un code de sortie et
reste aveugle au même défaut. L’outillage cœur l’attend déjà (`unit =`,
`.capped_scope_result()`). Hors périmètre ici : ce chemin ne passe pas
par `.compute_error_message()`.

## nemetonshiny 0.134.0 (2026-08-23)

Implémente `briefs/vers-nemetonshiny/2026-08-22-plafond-memoire.md`.
Plancher relevé à `nemeton (>= 0.183.0)`.

#### Removed — Le plafond mémoire n’est plus décidé ici

L’app portait sa propre politique de plafond, et le cœur a tranché en sa
faveur sur le fond : **50 % de `MemTotal`**, exactement le chiffre que
`.compute_memory_max()` avait choisi contre le défaut cœur de 70 %. Le
constat qui l’avait motivé est confirmé — sur la station de référence,
70 % font 21 Go quand `systemd-oomd` avait déjà tué la session à 17,1
Go. *Un plafond qui se déclenche après l’exécuteur n’est pas un
plafond.*

Avoir raison ne justifiait pas de garder la copie. Elle avait produit
**trois plafonds dans la même session** : le calcul des indicateurs à 50
%, FORDEAD et la reGénération au défaut cœur de 70 %. Trois travaux
lourds, trois limites — la même classe de défaut que le fork
d’`INDICATOR_FAMILIES`, et elle se corrige au même endroit : en n’ayant
plus de copie du tout.

Partent donc `.compute_memory_max()`, `.total_memory_bytes()`
(`service_compute.R`) et `.capped_memory_max()`
(`service_monitoring.R`). Aucun site d’appel ne passe plus `memory_max`
: le cœur lit `NEMETON_MEMORY_MAX` lui-même, avec les mêmes valeurs de
désactivation. La variable continue d’être transmise au worker
(`.capture_worker_envvars()`) — c’est lui qui lance l’enfant, donc lui
qui résout le plafond.

Un test interdit désormais toute fraction de RAM dans les trois fichiers
concernés : le remède à trois plafonds n’est pas d’en choisir un
meilleur ici, c’est de n’en avoir aucun.

#### Fixed — « exit -15 » ne dit rien ; il dit maintenant « plafond mémoire »

Un calcul sur Couchey a été tué après 3 h 20 de CPU, 11 Go de cache et
**zéro indicateur**, avec pour tout diagnostic :

    "start_computation" failed in its capped child process (exit -15).
    ✖ ExtendedTask failed

Le journal système disait la vérité à la même minute :
`run-r11dc…scope: Failed with result 'oom-kill'`.

**L’écart vient de qui surveille quoi.** `processx` surveille le client
`systemd-run`, pas le R qui tourne dans le scope. Quand le plafond est
atteint, l’OOM killer tue le R (SIGKILL, −9), systemd démonte ensuite le
scope, et le client sort en **SIGTERM (−15)**. Or le cœur ne reconnaît
comme mémoire que −9 et 137 : le cas le plus fréquent tombait dans la
branche générique, à côté du message « ran out of memory (ceiling: …) »
qui aurait tout dit.

L’app traduit désormais elle-même (`.compute_error_message()`) :
processus tué → le plafond mémoire est la **cause habituelle**, et le
remède est nommé (`NEMETON_MEMORY_MAX`, emprise plus petite). Pas « la
cause » : un code de sortie ne permet pas de l’affirmer, et le message
ne prétend pas plus que ce qu’il sait. Le code de sortie, lui, ne
remonte plus à l’écran — il ne dit rien à personne.

Au passage, ce message était un `paste("Erreur de calcul:", …)` **en dur
en français**, contraire à la règle i18n depuis longtemps. Il est
bilingue, et le message brut du moteur est échappé avant affichage.

Deux briefs partent avec cette version — le vrai correctif est ailleurs
: `specs/BRIEF-opencanopy-pct-veg-values.md` (la ligne qui a provoqué
l’OOM) et `specs/BRIEF-nemeton-oom-sigterm-scope.md` (la reconnaissance
du −15).

## nemetonshiny 0.133.0 (2026-08-22)

#### Changed — Un import CSV remplace le projet courant, et le dit avant

L’import créait un projet **à côté** : rien n’était écrasé sur le disque
(`create_project()` forge toujours un id neuf), mais l’ancien projet
restait là, orphelin. Il est désormais **supprimé et remplacé dans
toutes ses composantes** — parcelles, UGF, indicateurs, commentaires,
exports.

**L’ordre n’est pas négociable, et c’est la partie qui compte.** Le
nouveau projet est créé, chargé, croisé avec l’ONF — *puis* l’ancien est
détruit. Tous les chemins d’échec (nom hors convention, cadastre muet,
aucune référence appariée, chargement raté) repartent **avant** ce
point, projet courant intact. Détruire d’abord aurait laissé
l’utilisateur sans rien sur un import à mi-chemin. Un garde porte
l’invariant plutôt qu’un commentaire : `.remplacer_projet_courant()` ne
détruit rien si le remplaçant n’a pas d’id, et un test le vérifie sur
quatre formes de remplaçant invalide.

**Le geste se dit avant de partir.** La modale affiche le nom du projet
qui va disparaître et ce qu’il emporte, et son bouton de confirmation
passe au **rouge** — la règle des couleurs réserve `btn-danger` à ce qui
détruit des données. Sans projet ouvert, ni bandeau ni rouge : il n’y a
rien à détruire, et une alerte qui crie au loup dès le premier import
cesse d’être lue.

#### Fixed — `project_id` ne suivait pas le projet importé

Trouvé en répondant à la question « l’import écrase-t-il un projet ? ».
Le handler posait `current_project` mais **jamais
`app_state$project_id`** — alors que l’autre créateur de projet
(`mod_project.R`) et le chemin d’ouverture normal (`mod_home.R`) posent
les deux. Trois conséquences, toutes silencieuses :

- **Les commentaires partaient dans le mauvais projet.**
  `save_comments()` lit `app_state$project_id` (`mod_synthesis.R:845` et
  `:883`, `mod_family.R:599`) : après un import, la perspective IA et
  les commentaires de famille du nouveau projet s’écrivaient dans le
  répertoire du **précédent**.
- **Le verrou ne suivait pas.** Tout son cycle de vie est branché sur
  `observeEvent(app_state$project_id)` (`app_server.R:364`) : l’ancien
  projet restait verrouillé en base, le nouveau ne l’était pas, et
  `readonly` continuait de décrire l’ancien.
- Les gardes qui comparent `project_id` (`mod_home.R:369`, `:490`,
  `mod_search.R:613`) raisonnaient sur le projet précédent.

Les deux valeurs bougent maintenant ensemble, dans le même helper —
c’est leur désynchronisation qui était le défaut, pas l’oubli d’une
ligne en particulier. `reset_project_state()` de `mod_home` est scindé
pour cela : `reset_computation_state()` remet à zéro le calcul, le
minuteur et les cartes de progression **sans toucher au projet
courant**, puisque le nouveau vient d’y être posé.

#### Changed — Le bouton d’import CSV rejoint le bloc « Tableau UGF » du sidebar

Il vivait en en-tête du tableau, dans le panneau principal, sur la même
ligne que le titre. Il est maintenant dans le **bloc repliable « Tableau
UGF » du sidebar gauche**, en tête, séparé par un filet des trois
actions qui le suivent.

**C’est un défaut de nommage qui l’a fait chercher au mauvais endroit.**
Deux surfaces portent le nom « Tableau UGF » : le bloc du sidebar et
l’onglet du panneau principal. L’entrée de la v0.132.0 disait « dans le
bloc *Tableau UGF* du sous-onglet du même nom » — une phrase qui ne
tranche pas entre les deux, et qui se lit spontanément comme le sidebar.
C’est là qu’on le cherche.

Le placement d’origine avait sa raison, et elle reste vraie : les trois
actions du sidebar (Fusionner, Diviser, Renommer) opèrent sur les
**lignes sélectionnées** du tableau, alors que l’import **crée un projet
entier** et remplace le courant. Mettre le bouton parmi elles risquait
de le faire lire comme une quatrième action de sélection. Cette
différence est donc **dite** plutôt que traduite par un éloignement : un
filet le sépare du groupe, et une mention sous le bouton — *« Crée un
projet entier — n’agit pas sur la sélection du tableau »* — porte la
portée là où le geste se déclenche.

Le bouton n’est **pas dupliqué** : deux points d’entrée pour un geste
qui remplace le projet courant, ce serait deux fois l’occasion de le
déclencher par erreur. Un test l’interdit explicitement, en plus de
celui qui vérifie sa présence dans le sidebar.

## nemetonshiny 0.132.1 (2026-08-22)

#### Changed — La famille F est décroisée dans le cœur, et l’app n’avait rien à corriger

`nemeton 0.182.0` (spec 049) remet la famille F d’aplomb : le créneau
**F1** portait le libellé « Risque d’érosion » et la colonne
`indicateur_f2_erosion`, et réciproquement. Les deux erreurs
s’annulaient à l’affichage — c’est décroisé, **F1 = fertilité, F2 =
érosion**.

**Aucune valeur ne bouge, aucun recalcul n’est nécessaire** — rien à
voir avec l’inversion de la famille R (v0.131.0), qui imposait, elle, de
tout recalculer. Les noms de colonnes étaient déjà justes et sont
inchangés.

**L’app a traversé le changement sans une ligne de code.** C’est le
bénéfice du dé-fork : depuis que `.build_indicator_families()` lit
[`nemeton::indicator_families()`](https://pobsteta.github.io/nemeton/reference/indicator_families.html)
/ `indicator_labels()` au lieu de porter sa copie, l’appariement code ↔︎
colonne ↔︎ libellé arrive du cœur ligne par ligne. Aucun axe n’écrit « F1
» en dur, aucune table locale ne suit le slug. Vérifié sur les quatre
contrôles du brief : axe F1 « Fertilité des sols » sur
`indicateur_f1_fertilite`, axe F2 « Risque d’érosion » sur
`indicateur_f2_erosion`, infobulle F2 qui parle bien de TWI, de pente et
de texture, et `famille_fertilite` inchangée — c’est une moyenne,
l’ordre des colonnes n’y entre pas.

Ce qui restait, ce sont **les commentaires**. Quatre blocs expliquaient
le choix de lire le cœur en s’appuyant sur le croisement de F comme sur
un fait présent (`app_config.R` ×2, `mod_family.R`, `utils_i18n.R`), et
deux fichiers de test en faisaient autant. Ils décrivaient la raison
d’être du code ; ils la contredisaient depuis hier. Réécrits au passé,
avec la conséquence dite : plus aucune famille n’est croisée (L
décroisée en v0.176.0, F en v0.182.0), donc ces tests ne peuvent plus
**distinguer** une lecture par colonne d’une lecture par slug — les deux
répondent pareil. Ce qu’ils verrouillent désormais, c’est la concordance
elle-même, et le principe qui survivra au prochain renommage : le
libellé décrit la colonne affichée, jamais le rang qu’elle occupe.

Un test **figeait le croisement comme fixture** :
`test-renommage-famille-L.R` affirmait `F1 -> indicateur_f2_erosion`
pour montrer, par contraste, que L ne l’était plus. Il tombait depuis la
publication du cœur — c’est le seul effet réel du décroisement de ce
côté, et le brief ne l’annonçait pas.

Plancher relevé à `nemeton (>= 0.182.0)` : aucune API nouvelle n’est
consommée, mais 0.182.0 est un correctif de justesse — une app qui
tourne sur un cœur plus ancien affiche une table F croisée.

#### Fixed — Deux tests rouges sur `main`, sans rapport avec la famille F

La suite complète en a révélé deux autres, tous deux figeant une UI qui
a bougé depuis.

`test-03mod_synthesis.R` cherchait l’icône `robot` du bouton « Générer
par IA ». Elle a laissé place aux trois étoiles et à `btn-ia` en
v0.130.9, quand l’accent ambre est devenu la marque du contenu généré.
Le test vérifie maintenant l’accent réel — les étoiles **et** la classe,
puisque c’est le couple qui porte la convention.

`test-info_popover.R` comptait **14** « i » d’information dans la vue
reGénération. Il en reste **10** : quatre ont suivi leurs réglages dans
*Paramètres › Sources & paramètres* en v0.128.0. Baisser le chiffre
aurait suffi à faire passer le test — et aurait perdu ce qu’il
garantissait. Les quatre « i » déplacés sont rendus **côté serveur**
(`output$regen_block`), donc hors de portée de `mod_sources_config_ui()`
: ils pouvaient disparaître sans qu’aucune assertion ne bouge. Un second
test les y retrouve désormais.

#### Added — Brief de rattrapage du `PLAN.md` cœur

`specs/BRIEF-nemeton-plan-md-0.125-0.132.md`. Le journal du `PLAN.md`
partagé s’arrête au 2026-08-14 (v0.124.0) ; **25 releases** se sont
accumulées depuis. La règle 12 interdit à cette session d’écrire dans le
dépôt cœur : le brief livre le texte à coller, les SHA, les cycles dev,
et cinq chantiers à faire correspondre aux cases réelles.

## nemetonshiny 0.132.0 (2026-08-21)

#### Added — Créer un projet depuis une liste CSV de parcelles cadastrales

Un bouton **« Importer un CSV de parcelles »** apparaît dans le bloc
*Tableau UGF* du sous-onglet du même nom, dans l’onglet Sélection. Il
crée un projet entier à partir d’un fichier, croise avec le parcellaire
ONF, et rafraîchit tous les sous-onglets de Sélection.

**Le format.** Un fichier `commune-code_insee.csv` — par exemple
`couchey-21200.csv` — contenant les références cadastrales séparées par
des points-virgules : `A1;A2;A3;…;AO212;AO220`.

**La commune est lue dans le NOM du fichier**, et c’est le point
sensible : son contenu n’en porte aucune trace, et `A1` existe dans
presque toutes les communes de France. Un nom hors convention est donc
**refusé**, jamais deviné — un INSEE erroné irait chercher le cadastre
d’une autre commune, où quelques références s’apparieraient par pure
coïncidence.

**L’appariement porte sur le couple (section, numéro entier).** Le
forestier écrit `A1`, le cadastre stocke `numero = "0001"` : comparer
les chaînes brutes ferait échouer toute la liste. Les sections à deux
caractères (`AO`, `ZB`, `0A`) sont prises en compte — le découpage lit
les **chiffres de fin**, il ne suppose pas une forme « lettres puis
chiffres ».

**Une liste partiellement résolue est un succès**, avec son rapport :
une parcelle a pu être fusionnée ou renumérotée depuis. Refuser l’import
serait excessif ; se taire serait pire — la surface obtenue passerait
pour la surface demandée. Les références introuvables sont listées.

Quatre échecs sont distingués — nom hors convention, fichier sans
référence, cadastre indisponible, aucune référence dans la commune —
parce qu’ils n’appellent pas le même geste. Le dernier signale presque
toujours un code INSEE qui ne correspond pas à la liste.

Le croisement ONF est **optionnel** (coché par défaut) et ses échecs
n’annulent pas l’import : le projet existe, simplement sans UGF
forestières.

Vérifié sur `couchey-21200.csv` : **23 références, 23 parcelles, 535,6
ha**.

## nemetonshiny 0.131.1 (2026-08-21)

#### Added — Le tableau de synthèse dit enfin dans quel sens se lit son Score

Une note sous le tableau : *« Score : plus il est élevé, plus la
situation est favorable — pour les douze familles. La famille R comprise
: un score haut y signifie une bonne résilience, donc peu de risque. »*

L’inversion de R1–R4 (v0.131.0) a rendu visible un manque plus ancien :
**aucune colonne `Score` ne disait sa direction**. Tant que « Risques &
Résilience » se lisait « haut = beaucoup de risque », l’intuition
suffisait. Elle donne maintenant le contraire de la vérité.

**Deux options ont été écartées.** Renommer l’axe du radar : il n’y en a
pas à renommer, le radar ne porte que des **lettres** (B, W, A, F, L, T,
R…), et non des noms de famille. Renommer la famille en « Résilience » :
son nom vient du cœur et reste **juste** dans l’onglet Famille, où l’on
voit les grandeurs brutes — `R1 = 100` y signifie bien un fort risque
incendie. Le renommer aurait rendu cet onglet faux à son tour.

Le partage retenu : **on compare des scores dans la Synthèse, on examine
des mesures dans l’onglet Famille.** Le risque reste le risque là où on
l’examine, et devient résilience là où on le compare — la note fait la
jonction.

Elle est placée **sous le tableau**, pas repliée dans un « i » : c’est
la clé de lecture des douze lignes, pas une précision facultative.

## nemetonshiny 0.131.0 (2026-08-21)

Implémente `briefs/vers-nemetonshiny/2026-08-20-sens-famille-risque.md`
(spec 048). Plancher relevé à `nemeton (>= 0.181.0)`.

#### Fixed — La famille R disait l’inverse de la vérité

`nemeton 0.181.0` inverse **R1** (feu), **R2** (tempête), **R3**
(sécheresse) et **R4** (abroutissement) à la normalisation, comme R5
depuis 0.99.1. Leur grandeur brute est « haut = mauvais » et passait
telle quelle sur le radar : une UGF très exposée obtenait un
`famille_risque` **élevé**, donc flatteur — et R5 pointait à l’opposé
des quatre autres dans sa propre famille.

L’app n’a **aucun calcul** à changer : le cœur rend désormais la valeur
dans le bon sens, et toute inversion applicative annulerait la
correction. Un test parcourt les sources pour s’assurer qu’aucune
n’apparaît — elle passerait inaperçue, le radar remontant sur les
massifs exposés sans qu’aucun test ne tombe.

Deux choses relevaient en revanche de l’app.

**Les indicateurs déjà calculés sont faux.** Un `indicators.parquet`
produit sous l’ancien sens reste parfaitement **lisible** : mêmes
colonnes, mêmes types, aucune erreur au chargement.
`compute_all_indicators()` le relirait donc, constaterait que le travail
est fait, et sauterait le recalcul en propageant des `famille_risque`
faux. Un marqueur `indicator_sense_version` déclenche une invalidation
**unique**, à la première ouverture du projet après la montée de
version. Il est posé même quand il n’y a rien à invalider, sinon le test
se rejouerait à chaque ouverture.

**La palette de la carte se retournait toute seule.** `famille_risque`
était peint en YlOrRd (jaune → rouge), ce qui convenait tant que « haut
= plus de risque ». Orienté « haut = bon », il aurait coloré en **rouge
les UGF les moins à risque**. Le brief annonçait que les couleurs ne
changeaient pas : c’est vrai du radar, pas de cette palette.
`famille_risque` en sort ; les codes bruts R1..R4 la gardent, leur sens
n’ayant pas bougé.

#### À savoir avant de comparer des scores

Un projet rouvert affichera un `famille_risque` **différent**, souvent
nettement plus bas sur les massifs exposés. Ce n’est pas une régression
: c’est la première fois que le radar dit vrai sur cette famille.

**Une comparaison de scores d’avant et d’après le 2026-08-21 n’a aucun
sens.**

## nemetonshiny 0.130.10 (2026-08-20)

#### Changed — L’accent IA couvre toute l’app, et entre dans la règle

Le bouton « Générer par IA » des vues **Famille** rejoint l’accent ambre
et les trois étoiles. Les quatre surfaces qui produisent du contenu
généré partagent désormais la même signalétique : Synthèse, Plan
d’actions, reGénération, Famille.

**Une ligne « Ambre » entre dans le tableau des couleurs de bouton du
`CLAUDE.md`.** Sans elle, la règle décrivait une hiérarchie — vert,
brun, blanc, goldenrod, rouge — où `btn-ia` n’existait pas : une
relecture y aurait vu une entorse esthétique, ce que la règle interdit
explicitement, et l’aurait « corrigée » en repassant le bouton en vert.

La ligne dit aussi **pourquoi** l’ambre échappe à la hiérarchie sans la
rompre : les cinq autres fonds répondent à « quel est le niveau d’action
? », l’ambre répond à « d’où vient ce contenu ? ». Et elle interdit de
l’employer pour une action de l’utilisateur — il perdrait son pouvoir de
signalement.

#### Fixed — L’accent se défaisait dès la première génération

`updateActionButton()` restaurait l’icône **robot** après chaque
génération, dans la Synthèse comme dans les vues Famille. L’accent posé
en v0.130.9 tenait donc jusqu’au premier clic, puis disparaissait.

Aucun test d’interface ne pouvait le voir : ces appels ne s’exécutent
qu’en session. Un test lit désormais les sources et exige qu’**aucune**
icône `robot` ne subsiste — c’est la seule façon d’attraper ce genre de
restauration.

Le bouton « insérer le conseil IA » de reGénération, qui avait échappé
au lot précédent, prend également l’accent.

## nemetonshiny 0.130.9 (2026-08-20)

#### Changed — Le bouton « Générer par IA » de la Synthèse prend l’accent ambre

Fond **ambre `#E8A33D`** et icône **trois étoiles**
(`bs_icon("stars")`), à la place du contour vert et de l’icône robot.

Cette couleur ne suit **pas** la palette sémantique des boutons — vert =
action principale, brun = secondaire, rouge = destructif — et c’est
délibéré : elle ne dit pas un *niveau d’action*, elle dit une
**provenance**. Ce que ce bouton produit est généré, pas mesuré. Un
accent réservé aux suggestions, prédictions et analyses automatiques.

Deux jetons entrent dans la palette, `--nemeton-ia` et
`--nemeton-ia-dark`, plus une classe `.btn-ia`.

**Le texte reste sombre**, et c’est mesuré : `#2C3E50` sur cet ambre
donne **5,09:1**, au-dessus du seuil AA de 4,5 ; du blanc tomberait à
**2,16:1**. Un bouton d’accent illisible n’accentue rien. Un test
verrouille ce choix.

#### Changed — Les panneaux IA du Plan d’actions et de reGénération suivent

Même accent sur les deux panneaux de dialogue : fond ambre et trois
étoiles, à la place du **bleu « information »** (Plan d’actions) et du
**vert « succès »** (reGénération). Un espace de conversation avec un
modèle n’est ni une information ni un succès.

Une classe `.bg-ia` accompagne `.btn-ia`, sur le même jeton. Trois
surfaces partagent désormais l’accent — le bouton de génération de la
Synthèse et ces deux panneaux — et un test vérifie qu’elles restent
alignées : une charte d’accent qui ne vaut que pour un écran n’est pas
une charte.

Le vert du bloc **« Tableau des actions »** n’est pas touché, et un test
le verrouille : c’est un bloc d’actions de l’utilisateur, pas une
surface IA. Confondre les deux viderait l’accent de son sens.

Reste inchangé pour l’instant : le bouton IA de la vue **Famille**.

## nemetonshiny 0.130.8 (2026-08-20)

#### Fixed — Une couche orange restait affichée sur la Carte UGF, impossible à masquer

Deux défauts se combinaient pour laisser un calque de polygones orange
en permanence sur la carte, y compris après la purge des parcelles hors
forêt — au point de faire croire que la couche « Dessin » peignait le
cadastre.

**La couche « Parcellaire ONF » n’avait pas de case.** Elle était
ajoutée par `addPolygons(group = "Parcellaire ONF")` mais absente des
`overlayGroups` des **deux** contrôles de couches (le rendu initial, et
celui recréé après `clearControls()` à chaque redessin). Sans case,
impossible de la décocher : elle restait visible quoi qu’on fasse.
Décocher « UGF » et « Tenements » ne changeait rien, ce qui laissait «
Dessin » — seule case encore cochée — comme coupable apparent.

**La prévisualisation n’était jamais effacée.** `rv$onf_preview` était
posé avant le croisement et jamais remis à `NULL` après. La surcouche
restait donc superposée au résultat indéfiniment, et affichait un
parcellaire forestier indépendant de ce que le projet contenait
réellement — trompeur en particulier après une purge.

Les UGF créées **sont** ce parcellaire : une fois le croisement fait, la
surcouche n’a plus rien à montrer que le résultat ne montre déjà.

#### Added — Dire pourquoi « Hors forêt publique » survit à une purge

Un second message suit désormais celui du nombre de parcelles supprimées
:

> N parcelle(s) rest(e/ent) partiellement forestière(s) : leur part hors
> forêt est conservée, sinon la parcelle serait trouée. C’est ce qui
> maintient l’UGF « Hors forêt publique ».

Le premier message rend compte de l’action demandée ; celui-ci désamorce
une lecture erronée du résultat. Une ligne « Hors forêt publique » qui
subsiste après avoir explicitement demandé la suppression se lit comme
un échec, tant qu’on ignore qu’elle porte les fragments des parcelles
mi-forestières.

Il ne s’affiche que s’il y a effectivement des parcelles partielles.

## nemetonshiny 0.130.7 (2026-08-20)

Implémente `specs/001-rafraichir-selection-parcelles.md`.

#### Fixed — L’onglet Sélection affichait des parcelles supprimées

Depuis la v0.130.6, la purge « hors forêt publique » retire réellement
des parcelles du projet. L’onglet **Sélection** continuait pourtant de
les afficher, et de les compter comme sélectionnées.

La raison : cet onglet ne lit pas `app_state$current_project`. Sa carte
tient son propre état, alimenté par un signal unique —
`app_state$restore_project` — que `mod_home` pose **au chargement d’un
projet**. Rien ne le reposait ensuite.

Un signal étroit est ajouté, **`app_state$parcels_changed`**, posé par
tout module qui modifie `projet$parcels` et écouté par la carte seule.
Elle redessine la couche et **restreint** la sélection aux parcelles
encore présentes — sans jamais en ajouter : le signal annonce une
modification, pas une sélection neuve.

**Pourquoi pas simplement reposter `restore_project`** — la solution
évidente, écartée pour deux raisons :

- il est aussi écouté par `mod_search`, qui peut relancer un appel à
  `geo.api.gouv.fr` : un rafraîchissement purement local déclencherait
  une requête réseau ;
- il exige `commune_code` et `department_code`, dérivés des parcelles
  par une logique propre à `mod_home` — les reconstruire ailleurs
  dupliquerait ce code, les omettre produirait une restauration
  partielle.

Un signal qui en dit plus que nécessaire finit par déclencher plus que
nécessaire.

Garde-fous : idempotence par horodatage (comme `restore_project`), et un
signal mal formé — sans `parcels`, à 0 ligne, ou sans colonne `id` —
laisse la carte intacte plutôt que de la vider. Mieux vaut un affichage
périmé qu’un affichage vide.

Sans purge, aucun signal n’est posé et rien ne change.

## nemetonshiny 0.130.6 (2026-08-20)

#### Added — Supprimer les parcelles que la forêt publique ne couvre pas

Une coche apparaît dans Carte UGF, sous le sélecteur de domanialité :
**« Supprimer les parcelles hors forêt publique (\< 10 %) »**,
**décochée par défaut**. Elle retire du projet, à l’issue du croisement,
les parcelles cadastrales que la forêt publique ne couvre pas ou couvre
à moins de 10 %.

Une parcelle que la forêt effleure à 3 % est un effet de bord de
numérisation, pas un peuplement à gérer — et la porter dans le plan
dilue tous les indicateurs calculés par unité.

**Le raisonnement porte sur la PARCELLE, jamais sur le tènement.** Une
parcelle forestière à 10 % ou plus est conservée **entière**, sa part
hors forêt comprise : supprimer cette part seule trouerait une parcelle
que l’utilisateur possède, et romprait le pavage. Ou la parcelle entière
part, ou rien.

Trois points de conception :

- **La part se lit sur `surface_m2`**, la surface cadastrale déjà
  répartie entre tènements par le découpage. Aucune géométrie n’est
  touchée — comparer des parts dans une même parcelle ne demande aucun
  recalcul d’aire.
- **Au seuil exact, la parcelle reste** (`<`, pas `<=`).
- **Pas de réglage exposé** : le seuil est un paramètre du service, 10 %
  par défaut, joignable par le code. L’exposer ferait arbitrer par
  l’utilisateur une question qui a une bonne réponse par défaut.

Les parcelles retirées le sont de `$parcels` **et** de `$tenements`, et
`parcels.gpkg` est réécrit — sans quoi elles reviendraient au prochain
chargement du projet, cette fois sans tènements.
`app_state$current_project$parcels` est mis à jour dans la foulée.

Décochée, la coche ne change rien : le comportement antérieur est
intact.

## nemetonshiny 0.130.5 (2026-08-20)

Implémente `briefs/vers-nemetonshiny/2026-08-20-t3-reference-year.md`
(§8). Aucun changement de plancher : `reference_year` existe dans le
cœur depuis l’origine.

#### Fixed — « Les 5 dernières années » veut enfin dire les 5 dernières années

L’indicateur **T3 (coupes rases)** recevait bien `window_years` et
`min_proba` depuis les réglages, mais **jamais `reference_year`**.
Laissé à `NULL`, le cœur déduisait alors l’ancrage de la fenêtre de **la
coupe la plus récente trouvée dans les UGF analysées**.

La fenêtre avait donc la bonne largeur, mais un point d’appui flottant :

- un massif dont les dernières coupes datent de 2021 était jugé sur
  **2017-2021** — une coupe de 2018 y comptait comme « récente » ;
- deux projets n’étaient **pas comparables** dès lors que leur coupe la
  plus récente différait, même réglés sur la même fenêtre.

Mesuré côté cœur sur les 94 UGF de La-Vieille-Loye / Chaux :
`reference_year` valait 2025 et excluait **565 des 1 260** pixels
détectés. Cohérent pour « la pression récente », mais ce 2025 venait des
données, pas d’un choix.

La fenêtre s’ancre désormais sur l’**année courante**. Un massif sans
coupe récente descend vers 0 au lieu d’être jugé sur une fenêtre
ancienne — ce qui est le résultat attendu : pas de coupe récente, pas de
pression.

**À prévoir** : les scores T3 déjà calculés peuvent changer sur les
projets dont la dernière coupe est antérieure à l’année courante. Ce
n’est pas une régression, c’est la correction ; mais si un banc compare
des sorties T3 d’avant et d’après, c’est le premier facteur à regarder.

Pas de troisième réglage exposé : ce serait faire arbitrer par
l’utilisateur une question qui n’a qu’une bonne réponse par défaut. Le
paramètre reste joignable par le code pour une analyse rétrospective.

## nemetonshiny 0.130.4 (2026-08-20)

#### Removed — Le tri des parcelles remonte dans le cœur

`nemeton 0.180.0` écarte lui-même les parcelles cadastrales qu’aucune
parcelle forestière ne rencontre, et expose le compteur via un attribut
`parcelles_concernees`. L’app cesse donc de le faire :
`.onf_parcelles_concernees()` et la réinjection maison sont supprimées,
`onf_projet_croise()` redevient un appel direct, et le compteur « N
parcelles sur M » est **lu** plutôt que recalculé. Plancher relevé à
`nemeton (>= 0.180.0)`.

Le résultat reste identique : 1 365 tènements, 86 UGF, même répartition,
mêmes surfaces (9 028 796 m²), 1 271/1 271 parcelles couvertes,
invariants verts.

**Une prédiction démentie, à consigner.** Le brief adressé au cœur
affirmait que déplacer ce tri restaurerait le pavage exact — les
0,001231 % d’écart introduits en v0.130.3 étant attribués à
l’aller-retour de projection qu’imposait la réinjection côté app. C’est
faux : le cœur émet bien les parcelles écartées sans aucune
reprojection, et l’écart reste **0,001231 %**.

La cause n’est donc pas identifiée. Une hypothèse non vérifiée : la
mesure somme des aires *géodésiques* (le cadastre IGN arrive en 4326)
des morceaux et les compare à l’aire du tout — or en géodésique la somme
des parts n’égale pas exactement le tout, et l’écart serait alors un
artefact de mesure plutôt que du calcul. À trancher si la question
ressort.

Le changement reste justifié par ses autres motifs : la question «
quelles parcelles rencontrent la forêt » est géométrique, donc métier
(règle 1), et l’app y perd 46 lignes plus une réinjection. Mais
l’argument présenté comme décisif dans le brief ne tenait pas.

## nemetonshiny 0.130.3 (2026-08-20)

#### Changed — Le bouton ONF ne demande plus de sélection préalable

« Créer les UGF avec le parcellaire ONF » détermine désormais lui-même
les parcelles cadastrales concernées : celles du projet qui rencontrent
le parcellaire forestier retenu. Un toast dit lesquelles — « N parcelles
cadastrales sur M touchent la forêt publique retenue ».

Le croisement ne porte plus que sur elles ; les autres sont réinjectées
entières sous « Hors forêt publique », ce qui est **mot pour mot** ce
que le croisement complet en aurait fait — une ligne, la parcelle
entière, `hors_ugf = TRUE` — obtenu sans calcul géométrique. Mesuré sur
La-Vieille-Loye : **181 parcelles retenues sur 1 271**, et le calcul
passe de **31,5 s à 11,1 s**.

Résultat vérifié identique au croisement complet : mêmes tènements (1
365), mêmes UGF (86), même répartition, mêmes surfaces (9 028 796 m²),
toutes les parcelles couvertes.

**Une nuance mesurée** : le pavage passe de 0,000000 % à **0,001231 %**
d’écart maximal — la réinjection impose un aller-retour de projection,
qui arrondit. C’est 40 fois sous la tolérance de `validate_tiling()`
(0,05 %), laquelle existe précisément pour absorber ce genre d’arrondi.
Le correctif propre est côté cœur (voir ci-dessous).

#### Changed — Domanialité : deux cases au lieu de trois choix

Le radio *Toutes / Domaniales / Communales et autres* devient deux
cases, **Domaniales** et **Communales et autres**, cochées toutes deux
par défaut. « Toutes » n’était que leur conjonction — une troisième
façon de dire la même chose, qui invitait à se demander en quoi elle
différait.

Le filtre agit **en amont** du croisement : ne cocher que « Domaniales »
restreint aussi les parcelles cadastrales auto-sélectionnées. Ne rien
cocher n’est pas « tout » mais une question sans objet, et le dit — sans
lancer de requête.

#### Changed — La note de calage passe dans un « i »

Le paragraphe permanent sous le sélecteur devient un popover à droite du
bouton, qui porte les deux explications : l’absence de sélection
préalable et le calage cadastral. Ce sont des explications qu’on lit une
fois, pas des valeurs qu’on surveille — et le paragraphe repoussait le
bouton vers le bas.

#### Dette identifiée — le tri des parcelles appartient au cœur

`.onf_parcelles_concernees()` répond à une question géométrique sur le
domaine, pas de présentation : sa place est dans `nemeton`, idéalement
**absorbée dans `croiser_parcelles_onf()`**. Le cœur pourrait alors
émettre directement les lignes `hors_ugf` des parcelles écartées, dans
le bon CRS — récupérant le gain de vitesse **et** le pavage exact, pour
tous ses consommateurs. Brief à suivre.

## nemetonshiny 0.130.2 (2026-08-19)

#### Changed — Le calage sur les limites cadastrales devient systématique

La coche « Caler les UGF sur les limites cadastrales » est retirée : le
calage s’applique désormais toujours. Une parcelle cadastrale couverte à
90 % ou plus par une UGF lui revient entière.

Le raisonnement : les limites forestières ONF sont **approximatives au
bord**. Une UGF dont le tracé ne suit pas la parcelle qu’elle recouvre
presque entièrement est un artefact de numérisation, pas une décision de
gestion — et laisser le choix à l’utilisateur revenait à lui demander
d’arbitrer une question technique qui n’a qu’une bonne réponse. Mesuré
sur La-Vieille-Loye : **170 → 124 tènements** et **13 → 41 bords
exactement cadastraux**.

Les deux garde-fous du cœur restent entiers : une parcelle réellement
partagée entre deux UGF **n’est pas** calée, et le reliquat « hors UGF »
ne peut jamais prendre une parcelle — supprimer de la forêt ne serait
pas une correction.

Le calage n’est pas silencieux pour autant : une note permanente
l’annonce sous le sélecteur de domanialité. Sans elle, une UGF dont le
bord suit le cadastre plutôt que le tracé ONF serait incompréhensible.

Côté service, `caler_sur_cadastre` passe à `TRUE` par défaut mais
**subsiste comme paramètre** : le comportement brut reste joignable et
testable.

#### Changed — Le bouton dit ce qu’il fait

« Croiser avec le parcellaire ONF » devient **« Créer les UGF avec le
parcellaire ONF »**. Le croisement est le moyen ; créer les UGF est le
but, et c’est ce que l’utilisateur cherche dans cette barre d’actions.

## nemetonshiny 0.130.1 (2026-08-19)

#### Removed — Le bouton « Importer le parcellaire ONF »

Carte UGF n’offre plus qu’**une** action ONF : « Croiser avec le
parcellaire ONF ». Le second bouton, qui *remplaçait* les parcelles du
projet par les parcelles forestières, est retiré.

Les deux partaient de la **même emprise** — la sélection cadastrale du
projet — et produisaient les **mêmes UGF**, avec les mêmes libellés. La
seule différence tenait à ce qu’« Importer » jetait au passage :

|  | Croiser | Importer |
|----|----|----|
| Parcelles cadastrales | conservées | écrasées |
| Composition d’une UGF | traçable | perdue |
| `part_ugf` (« vous ne détenez que 40 % de cette parcelle forestière ») | disponible | indisponible |

C’était donc un cas **dégradé** du croisement, destructif de surcroît,
qui coûtait un bouton, une modale de confirmation et une
prévisualisation dédiée.

Ce qu’on perd : obtenir les parcelles forestières *entières*, y compris
leurs parties hors de la sélection. C’est le comportement voulu pour un
propriétaire — une parcelle forestière qui déborde de son bien ne doit
pas entrer entière dans son plan de gestion, et `part_ugf` dit
précisément quelle fraction il détient.

Retirés avec lui : `onf_projet_from_parcelles()` et ses tests, six clés
i18n devenues orphelines. Un test verrouille l’absence du bouton pour
qu’il ne revienne pas par mégarde.

## nemetonshiny 0.130.0 (2026-08-19)

Recette de la spec 046 exécutée contre le **vrai** service WFS ONF
(forêt domaniale de Chaux), et correction du défaut qu’elle a révélé.

#### Fixed — L’import du parcellaire ONF échouait sur données réelles

`onf_projet_from_parcelles()` plantait dès que le parcellaire forestier
n’avait pas exactement le même nombre de lignes que les parcelles du
projet :

    Error in `[[<-.data.frame` : replacement has 427 rows, data has 1

La cause est l’idiome
`utils::modifyList(projet, list(parcels = parcelles))`, repris de
l’esquisse du brief.
[`modifyList()`](https://rdrr.io/r/utils/modifyList.html) **récurse dans
les listes** — et un `data.frame` en est une : au lieu de remplacer
l’objet `parcels`, il fusionne les colonnes de l’ancien parcellaire avec
celles du nouveau. Erreur immédiate à tailles différentes, et fusion
**silencieuse** à tailles égales, ce qui est pire. Remplacé par une
affectation directe.

Les tests unitaires ne pouvaient pas le voir : leurs fixtures avaient 2
parcelles cadastrales et 2 parcelles forestières, donc la fusion «
marchait » par accident. C’est exactement la configuration qui ne se
produit jamais en vrai, où une poignée de parcelles cadastrales
rencontre des centaines de parcelles forestières. Un test de régression
fixe désormais des tailles volontairement différentes.

#### Changed — Recette §6 validée contre le service réel

| Cas | Attendu par le brief | Mesuré |
|----|----|----|
| Chaux, emprise 4×4 km | ~200 UGF ; 217 parcelles / 2 105 ha / 5,8 s | **213 parcelles / 2 114 ha / 1,1 s** — 189 « Forêt domaniale de Chaux » + 24 « Forêt communale de La-Vieille-Loye » |
| filtre `domaniale` | seules les domaniales subsistent | 394 / 394 ; `autre` → 33, aucune domaniale |
| plaine agricole | « aucune forêt publique » | `status = empty` en 0,4 s |
| service coupé | message d’indisponibilité, repli cadastral | `status = unavailable` en 0,1 s, `NULL` rendu |

Bout-en-bout sur les 427 parcelles réelles : import en 0,1 s (427 UGF,
invariants verts) ; croisement produisant 586 tènements pour 423 UGF,
avec identifiants uniques, invariants verts et surtout un **pavage exact
des parcelles cadastrales à 0,000000 %** — l’invariant qui se serait
cassé en silence.

#### Changed — Le calage cadastral, validé sur cadastre réel

La case « caler les UGF sur les limites cadastrales » reproduit
**exactement** les mesures du brief, sur le vrai cadastre de
La-Vieille-Loye (1 271 parcelles cadastrales × 94 parcelles forestières)
:

|                                  | Brief (cœur) | Mesuré (app) |
|----------------------------------|--------------|--------------|
| Tènements forestiers sans calage | 170          | **170**      |
| Tènements forestiers avec calage | 124          | **124**      |
| Bords cadastraux sans calage     | 13           | **13**       |
| Bords cadastraux avec calage     | 41           | **41**       |

Le réglage était resté non vérifiable tant qu’on l’éprouvait sur un
cadastre *synthétique* : une grille régulière est par construction
désalignée du parcellaire forestier, l’UGF dominante n’y détenait jamais
plus de **30,1 %** d’une maille, contre les **90 %** qu’exige le calage.
Sur cadastre réel la médiane monte à **0,95**, et 41 parcelles sur 64
franchissent le seuil.

Au niveau du projet : 1 422 → 1 365 tènements et 87 → 86 UGF — une UGF
forestière disparaît, ne tenant que des échardes absorbées par l’UGF
dominante.

#### Fixed — Le croisement était 95 fois plus lent que nécessaire

`tenement_import_replace()` pesait **628,9 s** sur ce même croisement,
contre 24,9 s pour tout le calcul du cœur : 96 % du coût. Deux causes,
toutes deux dans l’app :

- pour **chacun** des 1 422 fragments, la fonction intersectait la
  totalité des 1 271 parcelles — et n’utilisait jamais le résultat. Du
  calcul intégralement mort. Le `st_intersects()` qui suivait était lui
  aussi refait par fragment, au lieu d’être calculé une fois via l’index
  spatial ;
- surtout, les comparaisons d’aires appelaient `st_area()` sur des
  géométries portant un CRS. `sf` en relit alors les paramètres à chaque
  appel pour attacher une unité : `CPL_crs_parameters` représentait
  **76,8 %** du temps, quand les intersections GEOS elles-mêmes n’en
  prenaient que 1,14 s. Ces aires ne servent qu’à départager des
  candidates — l’unité n’y joue aucun rôle.

Corrigé : index calculé une fois, calcul mort supprimé, court-circuit
quand un fragment ne touche qu’une parcelle, et comparaisons d’aires sur
des copies sans CRS (les géométries stockées gardent le leur).

**628,9 s → 6,6 s**, à résultat *strictement identique* — mêmes
tènements, mêmes parents, mêmes UGF, même répartition, surfaces au
centième près, invariants verts. Un croisement complet passe de 654 s à
31,5 s. Le gain profite aussi à l’import de découpage QGIS, qui emprunte
la même fonction.

Les versions antérieures à 0.130.0 sont dans
[NEWS-archive.md](https://github.com/pobsteta/nemetonshiny/blob/main/NEWS-archive.md).

# Sources météo horaires pour microclimf sans la file CDS

- Propriétaire : Pascal
- Date de consultation : 2026-11-23 (toutes les pages ci-dessous, sauf mention contraire)
- Décision éclairée : quelle source horaire alimente `microclimf` (via `.rsen_forcage_era5`, `R/regen_engines.R`) maintenant que la file CDS atteint ~4 h/mois demandé.
- Liste de contrôle : non jointe à la consigne ; non passée (dit dans le rendu).
- Statut : rapport d'acquisition, aucun référentiel de codes créé. Rien écrit dans `nemeton` ni `nemetonshiny` hors ce fichier.

## 0. Contrainte d'entrée à retenir (vérifiée)

Le code actuel passe par `mcera5::extract_clim(format = "microclimf")` et obtient les colonnes `obs_time, temp, relhum, pres, swdown, difrad, lwdown, windspeed, winddir, precip` (`R/regen_engines.R` l. 960-970).
`microclimf::checkinputs()` exige `lwdown`, `difrad` et `precip` dans `weather`, et des jours entiers de 24 h (source : code `dataprep.R`, lu sur [raw.githubusercontent.com/ilyamaclean/microclimf](https://raw.githubusercontent.com/ilyamaclean/microclimf/master/R/dataprep.R)). Le rayonnement thermique descendant et le diffus sont donc obligatoires. C'est ce critère qui départage les pistes.

## 1. Constat : la file CDS

- Fils du forum ECMWF : files longues, 4 à 15 h par requête fractionnée (7 avr. 2026), ~6 h (27 mai), « 6 h et plus, un job de 10 h pour 2,24 Mo » (5 août), puis « de 9 h à quelques minutes » le 5 août à 15 h 30. Aucune réponse du personnel ECMWF dans le fil. Source : [forum.ecmwf.int, fil 14836](https://forum.ecmwf.int/t/severe-and-persistent-queue-delays-for-era5-hourly-data-pressure-levels-via-cds-api/14836). Ce fil porte sur les niveaux de pression, pas sur les niveaux simples. Le chiffre « ~4 h/mois demandé depuis le 22/11 » vient de la consigne, je ne l'ai pas retrouvé dans une source publique.
- Les délais sont donc intermittents et non garantis. Une source qui n'en dépend pas vaut mieux qu'un contournement.

## 2. Tableau comparatif

| Critère | A. CDS « time-series » (même clé CDS) | B. ARCO-ERA5 Google (Zarr) | C. Earthmover Icechunk (AWS) | D. ECMWF ARCO « data lake » (beta) | E. Open-Meteo Historical | F. SAFRAN journalier (GéoSAS) + désagrégation |
|---|---|---|---|---|---|---|
| Variables horaires utiles | Liste non confirmée (page sans liste) | `2t, 2d, sp, 10u, 10v, ssrd, strd, tcc, tp` (0,25°) | 35 variables de surface (liste non vérifiée) | « sous-ensemble » seulement | T2m, HR, point de rosée, pression, nuages, précip, rayonnement direct/diffus/total ; PAS de rayonnement thermique descendant ni de u/v listés | Journalier seulement : T, Tmin, Tmax, HU, FF, DLI_Q (rayonnement atmosphérique, J/cm²), SSI_Q (visible), précip |
| Résolution | 0,25° (aligné ERA5) | 0,25° | 0,25° | ERA5 0,25° et ERA5-Land | ERA5 0,25°, ERA5-Land 0,1° | Maille SAFRAN 8 km |
| Couverture / fraîcheur | 1940 à aujourd'hui, màj quotidienne, latence ~5 j | Annoncé 3 mois (stable) / ~1 sem (ERA5T) ; MAIS stores `co/single-level` en retard (voir 3.B) | 1940-2025, màj trimestrielle (version gratuite) ; version quotidienne payante | 1940 à aujourd'hui | 1940 à aujourd'hui, màj quotidienne, délai 5 j | 1958-08-01 au 2026-11-19 |
| Licence | CC-BY (page CDS) | Non établie sur la page Google ; données Copernicus (licence Copernicus/CC-BY à confirmer) | CC-BY 4.0 | Non indiquée | Données CC-BY 4.0 ; API gratuite NON commerciale ; payant pour usage commercial ; code AGPL-3 | Licence Ouverte 2.0 (Etalab) |
| Accès | Clé CDS, mais jeu dédié séries ponctuelles | Anonyme, bucket public | Anonyme, S3 us-east-1, sans compte | Jeton, beta, accès révocable « à tout moment » | HTTP sans clé (gratuit), < 10 000 appels/j, 5 000/h, 600/min | Sans clé (OGC-EDR) |
| Lecture depuis R | Fichier CSV/NetCDF, pas de Zarr ; adapter `mcera5::extract_clim` | `stars::read_mdim` (GDAL Zarr), `Rarr`, `pizzarr`, ou xarray via reticulate (voir 3.B) | Icechunk/Zarr v3 : pas de lecteur R vérifié ; Python recommandé | xarray (exemple officiel) | JSON/CSV simple | Déjà en place (BILJOU) |
| Pérennité | Service Copernicus, mais files | Dépôt GitHub : 24 tickets ouverts, certains sans réponse | Dépôt privé Earthmover, trimestriel | « Beta », annulable | Société privée, code ouvert | Service INRAE/GéoSAS + Météo-France |
| Souveraineté (ADR-004) | Européenne (ECMWF/Copernicus) | Hébergé Google (hors UE) | Hébergé AWS us-east-1 | Européenne | Suisse/France-Allemagne (non vérifié) | Française |

## 3. CQQCOQP par piste

### 3.A ERA5 par la porte « time-series » du CDS (piste que tu n'avais pas listée)

- Pourquoi : éviter la file en changeant de jeu, sans changer de source scientifique.
- Qui / Quoi : ECMWF/Copernicus. Jeu `reanalysis-era5-single-levels-timeseries`, « regridded subset » d'ERA5, conçu pour récupérer de longues séries en un point ; le point demandé est ramené au nœud de grille le plus proche. Sources : [CDS](https://cds.climate.copernicus.eu/datasets/reanalysis-era5-single-levels-timeseries), [ECMWF](https://www.ecmwf.int/en/forecasts/datasets/era5-hourly-time-series-data-single-levels-1940-present).
- Comment / Où / Quand : 1940 à aujourd'hui, 0,25°, mise à jour quotidienne, latence ~5 jours (les deux pages concordent). Formats NetCDF et CSV. Licence « CC-BY » (CDS).
- Combien : non vérifié (pas de limite affichée). Surtout non vérifié : la liste des variables (page CDS vide sous « Main variables »), le mode d'accumulation des flux (`ssrd`, `strd`, `tp`) et si cette porte passe par la même file que le jeu gridé.
- Un fil du forum mentionne qu'on peut éviter d'attendre en queue si les données viennent d'un stockage cloud (sous-ensemble ARCO pour séries ponctuelles). Source : résultat de recherche sur [forum.ecmwf.int](https://forum.ecmwf.int/t/access-to-our-arco-era5-data-lake-zarr-store/15123).
- Écart avec l'existant : `mcera5` télécharge le jeu gridé ; il faudrait écrire un lecteur du CSV/NetCDF ponctuel et reconstruire le format `microclimf`. Travail côté cœur, à confier à une instance dédiée.

### 3.B ARCO-ERA5 sur Google Cloud (Zarr)

- Qui : Google Research, projet [google-research/arco-era5](https://github.com/google-research/arco-era5) (code Apache-2.0).
- Quoi : le jeu « analysis-ready » 0,25° contient `2t, 2d, sp, 10u, 10v, ssrd, strd, tcc, tp`, toutes les variables dont `microclimf` a besoin (sauf `difrad`, voir 5). Le jeu « cloud-optimized » (`co/single-level-surface.zarr-v2`, `co/single-level-reanalysis.zarr-v2`) est sur grille native.
- Où : `gs://gcp-public-data-arco-era5/` (chemins `ar/...` et `co/...`). La page [Google Cloud](https://docs.cloud.google.com/storage/docs/public-datasets/era5) dit « from 1979 to the present » et ne donne ni licence ni fréquence de mise à jour. Les versions japonaise, allemande et chinoise de cette même page disent que le programme public ne contient les données que jusqu'en mai 2023 et n'est pas rafraîchi (lu via résultats de recherche, version anglaise non retrouvée). Contradiction non tranchée.
- Quand : le README annonce « updated … on roughly the 9th of each month with a 3 month delay » (ERA5) et ERA5T « approximately 1 week delay ». Mais le ticket [#155](https://github.com/google-research/arco-era5/issues/155) (ouvert, sans réponse des mainteneurs) rapporte que `co/single-level-*` n'était plus à jour depuis le 17 sept., ERA5T s'arrêtant au 2026-07-31 ; au 29 oct., août 2026 manque (404) alors que les métadonnées annoncent ERA5T jusqu'au 30 sept. Le store `ar/full_37-1h-0p25deg-chunk-1.zarr-v3` est dit à jour. Tickets associés #161 (« Latest data not synced », 11 nov. 2026) et #163 (« Broken data », 16 nov. 2026).
  Pour des « années récentes jusqu'à l'an dernier » (donc jusqu'à fin 2025), le retard n'est pas bloquant ; il le serait pour une année en cours.
- Combien : accès anonyme ; les frais d'egress vers l'extérieur de Google Cloud pour un point ou une petite emprise sont négligeables avec des chunks temporels, mais je n'ai pas trouvé le tarif exact (supposé).
- Licence : à ne pas supposer. La page Google ne la donne pas, le README montre seulement l'Apache-2.0 du code. Les données ERA5 restent sous licence Copernicus ; à vérifier sur la page Copernicus de la licence avant de livrer.
- Lisibilité R : `stars::read_mdim()` via le pilote GDAL Zarr est documenté sur un jeu de précipitation, pas sur ERA5 ([r-spatial](https://r-spatial.org/r/2022/09/13/zarr.html)) ; `Rarr` (Bioconductor) et `pizzarr` (CRAN) existent ([Rarr](https://bioconductor.statistik.tu-dortmund.de/packages/3.24/bioc/manuals/Rarr/man/Rarr.pdf), [pizzarr](https://cran.r-project.org/web/packages/pizzarr/readme/README.html)). Aucun exemple vérifié sur ARCO-ERA5 en R : à tester. Le cœur utilise déjà `reticulate` pour FORDEAD, donc xarray est un repli possible.
- Mise en forme : les cumuls (`ssrd`, `strd`, `tp`) sont à dé-accumuler comme aujourd'hui (voir `.rsen_clamp_flux`). L'unité de ces stores n'a pas été vérifiée.

### 3.C ERA5 AWS (Earthmover Icechunk)

- Qui : Earthmover, source NCAR/CDS. [registry.opendata.aws/earthmover-era5](https://registry.opendata.aws/earthmover-era5/).
- Quoi : ERA5 0,25°, 1940-2025, 35 variables de surface, Icechunk/Zarr v3, CC-BY 4.0, accès anonyme.
- Quand : version gratuite mise à jour tous les trois mois ; version « mise à jour quotidienne » payante. Cela suffit pour 2025 mais couvre mal l'année en cours.
- Lisibilité R : Icechunk n'est pas lisible par GDAL (non vérifié) ; il faudrait Python.
- Pérennité : un seul mainteneur commercial ; page sans engagement de durée.
- Autres miroirs AWS (NSF NCAR) : non explorés.

### 3.D ECMWF « Data Stores » ARCO (CDS-Beta)

- Annonce du 30 juin 2026 : accès « tokenized » à des cubes ERA5 et ERA5-Land, formats geo-chunked (longues périodes, zone limitée, soit notre cas) et time-chunked, « only a subset » des variables, service en beta que les administrateurs peuvent fermer « à tout moment ». Source : [forum ECMWF 15123](https://forum.ecmwf.int/t/access-to-our-arco-era5-data-lake-zarr-store/15123).
- Inconnues : liste des variables (si `strd`, `ssrd` y sont), obtention du jeton, quotas, licence, lecteur R. C'est potentiellement la meilleure option européenne mais la moins documentée.

### 3.E Open-Meteo Historical Weather

- Quoi : ERA5 0,25° et ERA5-Land 0,1°, horaires, délai « 5 days ». Variables listées : T2m, HR, point de rosée, pression, nébulosité, précipitation, vent à 10 m (vitesse et direction), rayonnement global, direct et diffus. Les composantes u/v et le rayonnement thermique descendant ne sont pas listés. Source : [documentation officielle](https://open-meteo.com/en/docs/historical-weather-api). Vent vitesse/direction suffit à reconstruire `windspeed`/`winddir` (le code actuel utilise déjà ces colonnes). Le manque de `lwdown` est bloquant tel quel : il faudrait l'estimer (formule Brutsaert/Prata avec T, HR, nuages), ce qui change le moteur et la validation.
- Licence : l'API gratuite est réservée aux usages non commerciaux (« You may only use the free API services for non-commercial purposes »), limites 10 000 appels/jour, 5 000/h, 600/min, 300 000/mois ; payant pour commercial. Données CC-BY 4.0. Source : [terms](https://open-meteo.com/en/terms). Un SaaS forestier facturé est a priori « commercial » : à demander à Open-Meteo.
- Code : AGPL-3 ou ultérieure, auto-hébergement Docker/Ubuntu documenté. Mais le dépôt ne dit pas si ERA5 historique peut être servi par une instance auto-hébergée (inconnu). Source : [dépôt GitHub](https://github.com/open-meteo/open-meteo).
- Ce n'est pas une source indépendante : ERA5 reste en dessous (même erreur que ERA5 direct).

### 3.F SAFRAN journalier découpé en heures

- Ce qui est vérifié : l'API EDR GéoSAS `safran-isba` ne sert que des paramètres journaliers (PT24H) : `T_Q`, `TINF_H_Q`, `TSUP_H_Q`, `HU_Q`, `FF_Q`, `DLI_Q`, `SSI_Q`, précipitations liquide et solide ; période 1958-08-01 à 2026-11-19 ; Licence Ouverte. Source : [api.geosas.fr/edr/collections/safran-isba](https://api.geosas.fr/edr/collections/safran-isba).
- Pas de SAFRAN horaire ouvert trouvé : les jeux SIM de [data.gouv.fr](https://www.data.gouv.fr/datasets/meteo-france-donnees-sim) sont quotidiens (Licence Ouverte 2.0), le seul jeu « horaire » est celui des stations (non grillé). Météo-France a ouvert ses données publiques le 1er janvier 2024 ([ministère](https://www.ecologie.gouv.fr/ouverture-des-donnees-publiques-du-ministere-au-1er-janvier-2024-toutes-donnees-publiques-meteo)), mais je n'ai pas trouvé de produit SAFRAN horaire dans cette ouverture (absence de preuve, pas preuve d'absence : à demander à Météo-France). AROME : modèle opérationnel, je n'ai pas vérifié d'archive historique ni de réanalyse couvrant plusieurs années.
- microclimf et le journalier : `checkinputs()` exige des jours de 24 h horaires et `lwdown`/`difrad` ; je n'ai trouvé dans le code lu aucune fonction `hourlyfromdaily` ni de pas de temps journalier utilisable. La documentation d'une autre bibliothèque de Maclean (`microclimc`) et le résultat de recherche parlent de modèles « horaire ou journalier » et d'une interpolation journalier vers horaire, mais je n'ai pas pu rattacher ce texte à `microclimf` (404 sur les pages rdrr). Les fonctions `microclima::hourlyNCEP`, `ropenmeteo::daily_to_hourly` et `meteor::day2hour` existent, sans lien établi avec microclimf. Statut : non vérifié.
- Conséquence : il faudrait désagréger soi-même (cycle diurne de T entre Tmin et Tmax, profil solaire, `lwdown` depuis `DLI_Q` répartie sur 24 h, précipitation /24 comme le conseille la documentation microclimf pour la seule précipitation). Perte attendue : le résultat visé (T max estivale sous couvert, VPD) dépend de l'amplitude diurne et des heures ensoleillées ; un cycle synthétique les lisse. Perte non quantifiée : à mesurer en comparant, sur une année déjà calculée en ERA5 horaire, T max sous couvert et VPD obtenus avec le forçage désagrégé.
- Avantage : même source que BILJOU, cohérence hydro-microclimat, aucune clé, maille 8 km (plus fine que 25 km), souveraineté.

## 4. Vérifié / supposé

Vérifié (page lue, 2026-11-23) :
- CDS time-series : 1940-présent, màj quotidienne, latence ~5 j, NetCDF/CSV, CC-BY (CDS ; ECMWF).
- ARCO Google : variables `2t,2d,sp,10u,10v,ssrd,strd,tcc,tp`, annonce 3 mois / 1 semaine, mais retards et trous signalés (#155).
- Earthmover : 1940-2025, trimestriel, CC-BY 4.0, anonyme.
- Open-Meteo : délai 5 j, gratuit non commercial, AGPL-3 (code), pas de `lwdown` listé.
- SAFRAN GéoSAS : journalier seulement, 1958-2026-11-19, Licence Ouverte.
- microclimf : `lwdown`, `difrad`, `precip` obligatoires dans `checkinputs`.

Supposé ou non confirmé :
- Variables exactes du jeu time-series CDS ; son accumulation ; sa file.
- Licence formelle des données ARCO Google ; date effective de dernière mise à jour du bucket aujourd'hui.
- Variables et quota du « data lake » ECMWF.
- Open-Meteo : usage commercial d'une instance auto-hébergée ; ERA5 historique auto-hébergeable.
- Lecture d'ERA5 Zarr depuis R (`stars`/`Rarr`) : aucune preuve sur ERA5.
- Existence d'une désagrégation journalier vers horaire dans microclimf.
- Coût d'egress (probablement nul ou négligeable, non chiffré).

## 5. Écart relevé entre sources

- Google Cloud : page principale dit « 1979 to the present » ; versions localisées de la même page disent jusqu'en mai 2023, non rafraîchi ; README et tickets disent mise à jour mensuelle avec retards. À vérifier directement : lire l'attribut `last_updated` et `valid_time_stop_era5t` du store.
- `difrad` : ERA5 ne fournit pas de rayonnement diffus ; `mcera5::extract_clim` le calcule (non relu ici). Vérifier que le chemin ARCO/time-series reproduit la même estimation, sinon les valeurs `difrad` divergeront.

## 6. Recommandation

1. Court terme (aucune ingénierie lourde) : essayer la porte A (CDS time-series, même clé, même licence, même source scientifique) : une seule requête CSV par point et par année au lieu de grilles mensuelles. Si le délai tient, c'est le plus petit changement. Inconnues à lever par un test de 10 minutes : variables présentes, accumulation, durée réelle en file.
2. Moyen terme, pour sortir de la dépendance à la file : ARCO-ERA5 Google (B) pour les années closes (jusqu'à 2025), avec `ar/full_37-1h-0p25deg-chunk-1.zarr-v3` comme store de référence puisque c'est celui signalé à jour. Même ERA5, donc aucune validation scientifique à refaire. À tester en R (`stars::read_mdim` ou `Rarr`), repli reticulate/xarray. Mettre en cache local par point (comme aujourd'hui) pour ne pas dépendre de la disponibilité du bucket. À la fois source de repli et option A pour l'année en cours via CDS. Éviter C (trimestriel, Icechunk non lisible en R) et D tant que la bêta n'est pas documentée (la suivre, elle est européenne et geo-chunked).
3. Écarter Open-Meteo comme source principale : pas de `lwdown`, API gratuite non commerciale, non indépendante d'ERA5. Possible comme secours pour des usages non commerciaux.
4. SAFRAN désagrégé : ne pas en faire la source de `microclimf` sans mesure. À instruire comme piste de validation indépendante (deuxième source qui se trompe pour une autre raison, maille 8 km) : comparer T max journalière SAFRAN `TSUP_H_Q` à celle d'ERA5 sur 2-3 projets. Sert aussi de contrôle de la source retenue.
5. Souveraineté : la chaîne A/B/D transporte des données Copernicus ; B passe par l'hébergement Google, mais en simple lecture publique d'un jeu européen. À noter dans l'ADR si B devient la source.

## 7. Trois points au sol (pour Pascal)

1. Une année close (2024) sur un projet déjà calculé : comparer T max estivale sous couvert et VPD entre forçage CDS actuel et forçage ARCO (store `ar/…zarr-v3`), même point. Écart attendu ~0 (même ERA5, regrillé différemment).
2. Un projet en forêt de montagne (Mouthe) : comparer T max journalière ERA5 0,25° et `TSUP_H_Q` SAFRAN 8 km. Écart à noter (relief, une maille ERA5 couvre ~600 km²).
3. Une année récente (2025 et début 2026) : lire `last_updated` et `valid_time_stop_era5t` du store ARCO et vérifier les 12 mois présents (le ticket #155 signale août 2026 absent).

## 8. Question de contrôle

Quelle décision cette donnée va-t-elle éclairer ? Quelle source horaire (ERA5 par quelle porte, ou SAFRAN désagrégé) alimente `microclimf` pour le moteur d'exposition, sans la file CDS ?

## 9. Ligne de journal proposée

2026-11-23 | acquéreur | Sources météo horaires sans file CDS : piste prioritaire = jeu CDS `reanalysis-era5-single-levels-timeseries` (test à faire), repli ARCO-ERA5 Google (retards signalés, ticket #155) ; Open-Meteo écarté (pas de lwdown, API gratuite non commerciale) ; SAFRAN journalier non retenu sans mesure. Rapport : pilotage/rapports/2026-11-23-sources-meteo-horaire.md.

## 10. Points de la liste de contrôle non cochés

La liste de contrôle n'a pas été jointe à la consigne : non passée. Décomptes complets non applicables (pas de référentiel de codes produit).

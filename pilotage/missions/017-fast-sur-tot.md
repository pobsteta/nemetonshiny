# Mission 017 — FAST calculé sur `_tot`, découpé par strate à l'affichage (parité FORDEAD / RECONFORT)

> Créée par l'orchestrateur le 2026-11-13 sur décision de Pascal (« oui, aligne FAST sur _tot »).
> Hors du chantier MCP du brief : changement de cap inscrit au journal.

## Mission

**En une phrase :** Faire calculer la carte FAST (masque d'alerte 0-4, préchauffage trend,
ingestion Sentinel-2) toujours sur la zone `_tot` du projet (union des UGF), et ne faire de la
strate choisie dans le menu (`_res`, `_feu`, `_mix`) qu'une **découpe d'affichage** du raster
`_tot`, exactement comme FORDEAD et RECONFORT.

## Constat (orchestrateur, 2026-11-13, lu sur `main` 351bdd76)

- Le menu zone présélectionne `_tot` (`R/mod_monitoring.R:1121-1138`).
- FORDEAD lit toujours `_tot` et découpe par strate avec `terra::mask` sur
  `get_monitoring_zone_aoi()` (`R/mod_monitoring_fordead_map.R:208-221`, `254-258`, `283-305`).
- RECONFORT calcule et met en cache sur `_tot` (`run_zone_id_r = fordead_zone_id()`,
  `R/mod_monitoring.R:3515`) et masque l'affichage par strate
  (`R/mod_monitoring_reconfort_map.R:104-107`, `179-192`, `425-456`).
- **FAST** reçoit `zone_id_r = input$zone_id` (`R/mod_monitoring.R:3398`) et le passe tel quel à
  `nemeton::compute_fast_alert_mask(zone_id = …)` (`R/mod_monitoring_fast_alerts.R:72-96`,
  appels `:456` (affichage) et `:624` (préchauffage trend)). L'ingestion FAST part aussi sur la
  strate choisie (`start_fast_ingest(as.integer(zone_id %||% input$zone_id), …)`,
  `R/mod_monitoring.R:2544`). Choisir `_res` relance donc un calcul complet sur la strate, avec
  des quartiles propres à la strate : les trois cartes ne sont plus comparables.
- Piège à ne pas prendre : `compute_fast_alert_mask(mask_polygon = …)` découpe **avant** le
  calcul des quartiles (cœur `R/fast_alert_mask.R:168-184`, lecture seule) ; l'utiliser pour la
  strate referait des quartiles par strate. La découpe se fait donc **après**, sur le masque 0-4
  `_tot`, comme FORDEAD (présentation pure).

## Décisions de Pascal applicables

- 2026-11-13 : « oui, aligne FAST sur _tot » (journal, ligne `cap` du 2026-11-13).
- Règles strictes CLAUDE.md n° 1-3 : aucune logique métier ; la découpe d'affichage par
  `terra::mask` est déjà admise comme présentation pure pour FORDEAD
  (`R/mod_monitoring_fordead_map.R:283-287`).
- Versionnage : cycle dev, `2026.10.18.9009` → `2026.10.18.9010`, entrée NEWS ; pas de release
  (v2026.11.1 reportée, journal 2026-11-11).

## Ce qu'il faut faire

1. `mod_monitoring_fast_alerts_server` gagne `run_zone_id_r` (zone du calcul et du cache), par
   défaut `zone_id_r` (rétro-compatibilité), comme `mod_monitoring_reconfort_map_server`.
   `R/mod_monitoring.R` lui passe `shiny::reactive(as.character(fordead_zone_id()))`, avec repli
   sur `input$zone_id` quand le projet n'a pas de `_tot` (même repli que FORDEAD,
   `fallback_zone`).
2. Le calcul du masque (`compute_fast_raster`, `:384-…`) et le préchauffage trend (`:599-624`)
   utilisent `run_zone_id_r()`. La signature de cache (`.fast_raster_signature`) porte la zone du
   calcul, pas la strate : changer de strate **ne recalcule pas** et n'affiche pas le toast de
   calcul.
3. Si la strate affichée diffère de la zone du calcul : découper le masque 0-4 avec
   `terra::mask` sur `get_monitoring_zone_aoi(con, strate)` transformé au CRS du raster, en
   réactif séparé (la découpe ne relance pas le calcul). Échec de découpe → avertissement
   `cli` et raster non découpé, comme FORDEAD.
4. Bandeau « zone saine » et décomptes d'alerte : calculés sur le raster **découpé** (ce qu'on
   voit), pas sur `_tot`.
5. Ingestion FAST (`.lancer_fast`, `R/mod_monitoring.R:~2500-2546`) sur la zone `_tot`, repli
   sur la strate si pas de `_tot`.
6. NEWS (cycle dev `.9010`) : le changement de comportement en une phrase (« choisir une strate
   ne recalcule plus FAST ; les quartiles sont ceux de l'union des UGF »).

**Hors périmètre** (à signaler dans le rendu, pas à modifier) : `mod_monitoring_pixel_map.R`
(onglet pixel), `list_fast_alerts_for_zone()` (placettes), validation-sampling (déjà sur `_tot`),
le cœur `nemeton`.

## L'Homme

- **Agent :** `dev` (sonnet, main : appuyer), tests compris selon la liste « Test discriminant ».
- **Relecteur :** relit cette fiche avant le dev (liste « Fiche prête pour le dev »), puis la PR.
- **Peut :** modifier `R/mod_monitoring_fast_alerts.R`, `R/mod_monitoring.R` (câblage FAST et
  `.lancer_fast` seulement), tests `tests/testthat/test-mod_monitoring_fast_alerts*.R` et
  nouveaux tests, NEWS, DESCRIPTION.
- **Ne doit pas :** toucher FORDEAD, RECONFORT, le pixel map, le MCP ; écrire dans `nemeton` ;
  passer `mask_polygon` au cœur pour la strate.

## Tests (liste « Test discriminant », `pilotage/listes-de-controle.md`)

En `testServer` avec `nemeton::compute_fast_alert_mask` moqué
(`local_mocked_bindings(..., .package = "nemeton")`) et une connexion/zone factices :

- Strate `_res` sélectionnée, `_tot` existante → le cœur est appelé avec `zone_id = id_tot`
  (valeur exacte), jamais avec l'id de la strate. Mutation : repasser `zone_id_r()` → rouge.
- Changer la strate `_tot` → `_res` → **aucun nouvel appel** au cœur (compteur d'appels).
  Mutation : signature portant la strate → rouge.
- Raster affiché : un pixel hors AOI de la strate vaut `NA`, un pixel dedans garde sa classe.
  Mutation : supprimer le `terra::mask` → rouge.
- `_tot` sélectionnée → raster non découpé (identique au masque calculé).
- Projet sans `_tot` → calcul sur la strate (repli), pas d'erreur.
- Ingestion : `start_fast_ingest` reçoit l'id `_tot`.
- Pas de test vacant : l'`observeEvent` est déclenché (`session$setInputs()` d'abord, mémoire
  `reference_testserver_external_reactivevalues`) ; aucun `tryCatch` autour des `expect_*`.
- Rendu : décompte pass/fail/skip de `devtools::test(filter = "monitoring")`, mutations citées.

## Critère d'arrêt

- Tests du périmètre verts, mutations prouvées ; suite `monitoring` sans régression.
- Si un test existant fige le calcul par strate : le corriger en le signalant (pas en silence).
- Si l'alignement exige de toucher le cœur ou le pixel map : s'arrêter et remonter à
  l'orchestrateur.
- PR ouverte par l'orchestrateur après le « prêt » du relecteur.

## Terrain

Essai par Pascal sur un projet avec strates (Couchey ou Dabo) : la carte FAST `_res` est la
carte `_tot` découpée, mêmes couleurs aux mêmes pixels que `_tot`, sans toast de calcul au
changement de strate.

## Redite par l'agent : _à compléter_

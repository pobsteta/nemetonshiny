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
- 2026-11-13 : « oui, inclus le Plan de validation FAST » (journal, même date).
- Règles strictes CLAUDE.md n° 1-3 : aucune logique métier ; la découpe d'affichage par
  `terra::mask` est déjà admise comme présentation pure pour FORDEAD
  (`R/mod_monitoring_fordead_map.R:283-287`).
- Versionnage : cycle dev, `2026.10.18.9009` → `2026.10.18.9010`, entrée NEWS ; pas de release
  (v2026.11.1 reportée, journal 2026-11-11).

## Ce qu'il faut faire

1. `mod_monitoring_fast_alerts_server` gagne `run_zone_id_r` (zone du calcul et du cache), par
   défaut `zone_id_r` (rétro-compatibilité), comme `mod_monitoring_reconfort_map_server`.
   `R/mod_monitoring.R` lui passe un réactif à **repli explicite** :
   `{ id <- fordead_zone_id(); if (is.na(id)) input$zone_id else as.character(id) }`.
   Attention : `fordead_zone_id()` (`:1092-1098`) rend `NA_integer_` sans `_tot`, et
   `as.character(NA)` passe la garde `isTRUE(nzchar(zone))` (`fast_alerts.R:388`, `:542`, `:600`)
   car `nzchar(NA)` vaut `TRUE` : le cœur s'arrêterait sur un NA. (Le `fallback_zone` de FORDEAD
   est interne à son module, `fordead_map.R:215`, pas un réactif parent.)
2. Passent à `run_zone_id_r()` dans `R/mod_monitoring_fast_alerts.R` : le calcul du masque
   (`compute_fast_raster`, `:385-…`), les dépendances de l'observer (`:530`), la garde « zone
   vide → `raster_rv(NULL)` » (`:541-547`), la signature (`:553-557`), le déclencheur du
   préchauffage trend (`:592`) et le préchauffage lui-même (`:599-624`). La signature de cache (`.fast_raster_signature`) porte la zone du
   calcul, pas la strate : changer de strate **ne recalcule pas** et n'affiche pas le toast de
   calcul.
3. `raster_rv` garde le masque `_tot` non découpé. `raster_r` devient le raster **découpé** :
   si la strate affichée (`zone_id_r()`) diffère de la zone du calcul, `terra::mask` sur
   `get_monitoring_zone_aoi(con, strate)` transformé au CRS du raster. L'AOI vit dans un
   réactif à connexion propre, comme `aoi_r` de RECONFORT (`reconfort_map.R:183-192`) ; la
   découpe ne relance pas le calcul. Échec de découpe → avertissement `cli` et raster non
   découpé, comme FORDEAD.
4. Tous les lecteurs de `raster_r` voient donc la strate : bandeau « zone saine » (`:868-896`),
   peinture de la carte (`:974-992`), sévérité au clic trend (`:795`), export `raster`
   (`:1069`).
5. Ingestion FAST : `.lancer_fast` (`R/mod_monitoring.R:2462-2547`). Seul l'appel manuel
   `shiny::observeEvent(input$run, .lancer_fast())` (`:2549`) est à changer : il passe la zone
   `_tot` (même repli explicite qu'au point 1). Hors changement : le pipeline passe déjà
   `fordead_zone_id()` (`:2567-2577`), « Reprendre » rejoue la zone de la sentinelle (`:2617`).
6. **Plan de validation FAST** (même principe : calcul sur `_tot`, la strate ne fait que
   borner) :
   - `R/mod_monitoring.R:3947-3950` passe en plus à `mod_validation_sampling_server` la zone du
     calcul (même réactif à repli explicite qu'au point 1) ; `zone_id_r` reste la strate.
   - **Chemin trend** (celui de l'interface pour FAST, `mod_validation_sampling.R:481-523` →
     `generate_trend_sanitary_plan`, `service_validation_sampling.R:178-…`) : appeler
     `nemeton::create_trend_sanitary_plan(zone_id = id_tot, mask_polygon = AOI de la strate)`.
     Le paramètre existe (cœur `R/validation_sampling.R`, lecture seule) : la pente est calculée
     pixel par pixel, sans quartiles, donc les valeurs dans la strate ne changent pas ; on
     partage le cache avec Alertes FAST et le tirage reste dans la strate. `plan$zone_id` reste la
     strate.
   - **Chemin classes** (`generate_validation_plan(source = "FAST")` →
     `.resolve_alert_raster`, `service_validation_sampling.R:267-…`, `:~347`) :
     `compute_fast_alert_mask` et `read_fast_alert_mask` sur l'id `_tot`
     (`.fordead_tot_zone_id()`, `:379`, déjà là) ; le polygone `zone` passé à
     `create_validation_sampling_plan` reste celui de la strate (il borne le tirage).
     `.extract_mask_run_id` (`:451`) lit le dossier `zone_<id_tot>` pour FAST. Le dev dit si ce
     chemin est encore atteignable depuis l'interface.
   - Lectures du masque FAST côté module : histogramme `alert_mask_r` (`:372-392`), surcouche du
     plan `alert_raster_r` (`:634-657`) et aperçu `preview_raster_r` (`:276-311`) lisent `_tot`
     puis découpent à l'AOI de la strate (`terra::mask`, comme au point 3). Si l'aperçu n'a plus
     de lecteur (l'observer `:313-318` sort pour FAST), le dire plutôt que le câbler.
7. NEWS (cycle dev `.9010`) : le changement de comportement en une phrase (« choisir une strate
   ne recalcule plus FAST ; les quartiles sont ceux de l'union des UGF »), en reprenant la
   mention « Non publié… v2026.11.1 » que porte la tête `.9009`.

**Hors périmètre** (à signaler dans le rendu, pas à modifier) : `mod_monitoring_pixel_map.R`
(onglet pixel), `list_fast_alerts_for_zone()` (placettes), le cœur `nemeton`.
**Plan de validation FAST** : **inclus** (décision de Pascal, 2026-11-13), voir point 7.
Constat hors périmètre à signaler seulement : FORDEAD dans le Plan de validation lit
`read_fordead_dieback_mask(con, zone_id)` sur la strate (`service_validation_sampling.R:~277`,
`mod_validation_sampling.R:~390`, `~651`) alors que son cache est sous `zone_<id_tot>` ;
le dev vérifie si une strate ≠ `_tot` y trouve son masque et le dit dans son rendu, sans corriger.

## L'Homme

- **Agent :** `dev` (sonnet, main : appuyer), tests compris selon la liste « Test discriminant ».
- **Relecteur :** relit cette fiche avant le dev (liste « Fiche prête pour le dev »), puis la PR.
- **Peut :** modifier `R/mod_monitoring_fast_alerts.R`, `R/mod_monitoring.R` (câblage FAST, du
  Plan de validation FAST et `.lancer_fast` seulement), `R/mod_validation_sampling.R` et
  `R/service_validation_sampling.R` (chemins FAST seulement), tests
  `tests/testthat/test-mod_monitoring_fast_alerts*.R`, `test-*validation_sampling*.R` et
  nouveaux tests, NEWS, DESCRIPTION.
- **Ne doit pas :** toucher FORDEAD, RECONFORT, le pixel map, le MCP ; écrire dans `nemeton` ;
  passer `mask_polygon` au cœur pour la strate.

## Dépend de / rouge attendu jusqu'à

Aucun.

## Tests (liste « Test discriminant », `pilotage/listes-de-controle.md`)

Réutiliser le harnais `.fast_raster_tab_harness` (`tests/testthat/test-mod_monitoring_fast_alerts.R:307`),
qui moque `.compute_fast_mask` (pas le cœur) : lui faire capturer `zone`. Moquer
`get_monitoring_zone_aoi` dans l'app (raster du harnais : EPSG:2154, 2×2, valeurs 0/1/2/4).
« Pas de toast » = `session$getReturned()$computing()` resté `FALSE`. Ingestion : avec
`make_fake_fast_task` et `find_zones_by_project` moqué (`tests/testthat/test-mod_monitoring.R:230`, `:317`).

- Strate `_res` sélectionnée, `_tot` existante → le cœur est appelé avec `zone_id = id_tot`
  (valeur exacte), jamais avec l'id de la strate. Mutation : repasser `zone_id_r()` → rouge.
- Changer la strate `_tot` → `_res` → **aucun nouvel appel** au cœur (compteur d'appels).
  Mutation : signature portant la strate → rouge.
- Raster affiché : un pixel hors AOI de la strate vaut `NA`, un pixel dedans garde sa classe.
  Mutation : supprimer le `terra::mask` → rouge.
- `_tot` sélectionnée → raster non découpé (identique au masque calculé).
- Projet sans `_tot` → calcul sur la strate (repli), pas d'erreur.
- Ingestion : `start_fast_ingest` reçoit l'id `_tot`.
- Plan trend : `create_trend_sanitary_plan` moqué reçoit `zone_id = id_tot` et un `mask_polygon`
  égal à l'AOI de la strate ; `plan$zone_id` vaut la strate. Mutation : repasser la strate → rouge.
- Plan classes : `compute_fast_alert_mask` / `read_fast_alert_mask` moqués reçoivent l'id `_tot`,
  `create_validation_sampling_plan` reçoit le polygone de la strate.
- Histogramme FAST : la distribution ne compte que les pixels de la strate (raster 2×2, un pixel
  hors AOI exclu). Mutation : supprimer la découpe → rouge.
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
changement de strate. Contrôler aussi la couverture : un projet dont le cache S2 n'aurait été
ingéré que sur une strate donnerait un calcul `_tot` partiel (cas probablement rare, FORDEAD et le
pipeline ingérant déjà sur `_tot`).

## Redite par l'agent : _à compléter_

## Statut

Terminée le 2026-11-13 : PR #266 fusionnée sur `main` (`61a682c4`, commit de mission `7267c4d2`),
cycle dev `2026.10.18.9010`, non publié. Relecture : prêt. Suites à trancher par Pascal : aperçu
`preview_raster_r`, FORDEAD du Plan de validation sur strate, bandeau « zone saine » sur strate.

# Mission 021 — Plan de validation RECONFORT sur une strate : lire `_tot`, découper à la strate (parité FAST / FORDEAD)

> Créée par l'orchestrateur le 2026-11-14 sur décision de Pascal (« oui fais de la même façon sur
> FAST, FORDEAD et RECONFORT »). FAST est aligné depuis la mission 017, FORDEAD depuis la 019 :
> reste RECONFORT, plus un garde-fou commun aux trois sources.

## Mission

**En une phrase :** Faire lire au Plan de validation RECONFORT sa classification 1/2/3 et son
`score` sur la zone `_tot` (là où RECONFORT calcule et met en cache), puis les restreindre à la
strate choisie, exactement comme FORDEAD (019), et verrouiller par un test commun que les trois
sources suivent la même règle.

## Constat (orchestrateur, 2026-11-14, lu sur `main` 33a0fc42)

- RECONFORT tourne sur `_tot` (runs : `R/mod_monitoring.R:3809` lancement manuel, refusé sans
  `_tot`, et `:3878` chaîne ; `:3527` est le câblage de la carte) ; le lecteur cœur cherche `zone_<id>` sous le cache
  (`nemeton/R/reconfort_pixel_series.R:208-209`, constat du relecteur de 019).
- Le Plan de validation RECONFORT lit sur la **strate** à cinq endroits :
  1. génération, masque : `.resolve_alert_raster()` branche RECONFORT
     (`R/service_validation_sampling.R:386-396`) ;
  2. génération, pondération continue `score` : `.resolve_weight_raster()`
     (`:505-529`), `reconfort_cache_manifest(cd, zone_id = <strate>)` ; le polygone
     `aoi` (`:521`) est déjà la strate et `read_reconfort_layer(mask_polygon = aoi,
     apply_zone_mask = TRUE)` découpe déjà ;
  3. provenance : `.extract_mask_run_id(project, "RECONFORT", mask_zone_id)` (`:541-563`),
     appelée avec la strate ;
  4. histogramme / auto-relâchement des témoins : `alert_mask_r`
     (`R/mod_validation_sampling.R:336`) ;
  5. surcouche carte : `R/mod_validation_sampling.R:604`.
- Modèle à suivre (019) : `.read_fordead_mask_chain()` (`R/service_validation_sampling.R:201-220`,
  `_tot` résolu par `.fordead_tot_zone_id()` `:469`, puis repli sur la strate) et
  `.read_fordead_mask_stratum()` (`:227-232`, découpe par `.clip_fast_to_stratum` /
  `.fast_stratum_aoi`). La génération ne découpe pas côté app : le cœur restreint le masque au
  polygone de la strate (`nemeton/R/validation_sampling.R:170`).
- Classe saine RECONFORT = 1, 0 / NA = pas de donnée (`R/mod_validation_sampling.R:943-950`) :
  la découpe à la strate met NA hors strate, ce qui ne doit pas fausser la distribution.

## Travail attendu

1. **Généraliser la chaîne de 019** aux deux moteurs catégoriels plutôt que la dupliquer : par
   exemple `.read_categorical_mask_chain(con, project, zone_id, source)` et
   `.read_categorical_mask_stratum(...)` pour FORDEAD et RECONFORT, avec la même résolution
   `_tot` (renommer `.fordead_tot_zone_id` en un nom neutre si le dev le juge utile) et le même
   **repli double** : (a) pas de zone `_tot` en base → strate ; (b) masque `_tot` absent sur
   disque → strate. FORDEAD doit rester strictement identique (ses 16 tests de
   `test-service_validation_sampling_fordead_tot.R` restent verts sans retouche ; toute retouche
   est signalée). Précisions de relecture :
   - garder `.read_fordead_mask_stratum(con, project, zone_id)` avec ce nom et cette signature
     (appelée par nom dans `test-service_validation_sampling_fordead_tot.R:132,149,165,173`), au
     besoin comme simple enveloppe de la fonction généralisée ;
   - appeler les lecteurs cœur uniquement en `(con, id, cache_dir = cd)` : les mocks ont la
     signature `function(con, zone_id, run_id = NULL, cache_dir)` (`:47-52`) ;
   - « 16 tests » = 16 `expect_*` dans 8 `test_that` ;
   - les tests RECONFORT existants (`test-service_validation_sampling.R:302`, `:356`, `:517`,
     `test-mod_validation_sampling.R:19`) ne simulent pas `find_zones_by_project` et passent
     `con = "fake-con"` : ils doivent rester verts sans retouche (le `tryCatch` de la résolution
     `_tot` retombe sur la strate) ; tout mock ajouté est une retouche à signaler.
2. Génération RECONFORT : masque lu par la chaîne ; `source_run_id` suit la zone réellement lue ;
   polygone de tirage = strate ; ne pas passer `mask_polygon` au lecteur de masque.
3. Pondération continue RECONFORT : manifeste lu sur le `zone_id` **rendu par la chaîne** (celui
   que le masque a réellement lu), jamais sur une zone `_tot` résolue à nouveau — sinon, en repli
   (b), le poids viendrait de `_tot` et le masque de la strate. `aoi` reste la strate (le cœur
   découpe déjà, `read_reconfort_layer` → `.apply_zone_mask` → `terra::mask`).
4. Histogramme et surcouche RECONFORT : helper `..._stratum` partagé.
5. Strate = `_tot` : aucun changement de comportement.
6. **Garde-fou commun aux trois sources** : un test paramétré (FAST, FORDEAD, RECONFORT) qui, pour
   une strate ≠ `_tot`, vérifie que le lecteur de masque est appelé avec l'id `_tot` et que le
   raster rendu est découpé à la strate (NA hors strate). Il doit rougir si l'une des trois
   sources revient à lire la strate. Conditions pour qu'il ne soit pas vacant :
   - **câblage réel** de `R/mod_monitoring.R:3960-4017` : `run_zone_id_r` est passé à FAST
     **seulement** ; FORDEAD et RECONFORT ne le reçoivent pas → simuler `find_zones_by_project`
     pour les trois, ne jamais passer `run_zone_id_r` à FORDEAD / RECONFORT ;
   - nommer le « lecteur de masque » de chaque source : `read_fast_alert_mask`,
     `read_fordead_dieback_mask`, `read_reconfort_alert_mask` ;
   - FAST : créer le dossier `cache/layers/fast_sampling` (sinon `alert_mask_r` rend NULL et le
     test est vacant) ; le chemin FAST qui compte en production (génération trend) reste couvert
     par `test-service_validation_sampling_tot.R:36` et `:177`, le citer ;
   - au moins **une mutation rouge par source**.
7. Tests RECONFORT dédiés (modèle `test-service_validation_sampling_fordead_tot.R`) : génération
   sur strate, repli (a), repli (b), provenance, pondération `score` lue sur `_tot`, découpe
   réelle par `terra::mask`, strate = `_tot`. Pièges : `tryCatch(error=)` autour des lecteurs →
   capturer les ids dans un environnement, aucun `expect_*` dans un mock ; `alert_mask_r` exige
   `app_state$active_main_tab = "monitoring"`. Au moins 4 mutations rouges, rendues.
8. Commentaires devenus faux corrigés (`.fast_run_zone` `:184-186` « seul RECONFORT garde la
   strate », en-tête `:360-385`, doc `@param run_zone_id` `:44-48`, en-tête de
   `.fordead_tot_zone_id` `:463-468` s'il sert aussi à RECONFORT, doc `run_zone_id_r`
   `R/mod_validation_sampling.R:217-220` si son sens change).
9. `grep -nP "[^\x00-\x7F]"` vide sur chaque fichier R touché.

## Hors périmètre

- La carte RECONFORT (déjà sur `_tot` + découpe), FAST et FORDEAD hors du garde-fou commun.
- Le bandeau « zone saine » sur strate (question encore ouverte à Pascal).
- Palette de la surcouche (`R/mod_validation_sampling.R:736-740`, 0:4 FAST/FORDEAD) : pour
  RECONFORT la classe saine 1 s'affiche en jaune pâle comme une alerte faible. Défaut existant,
  signalé à Pascal, non corrigé ici.

## Versionnage

Cycle dev `2026.10.18.9013` → `2026.10.18.9014`, entrée NEWS « Non publié », section
correctifs. Pas de release.

## Tests

La machine est chargée : lancer au premier plan d'abord
`Rscript -e 'devtools::test(filter = "validation_sampling")'`, puis
`Rscript -e 'devtools::test(filter = "validation|monitoring")'` (sortie dans un fichier, `tail`) ;
rendre pass / fail / skip.

## Moyens

- Homme : `dev` (sonnet), worktree depuis `main`.
- Relecteur sur la fiche avant dev, puis sur la PR.
- Temps de la main : *appuyer*.

## Redite par l'agent : _à compléter_

# Mission 019 — Plan de validation FORDEAD sur une strate : lire `_tot`, découper à la strate

> Créée par l'orchestrateur le 2026-11-14 sur décision de Pascal (« Oui fais le »), suite du
> constat du dev de la mission 017.

## Mission

**En une phrase :** Faire lire au Plan de validation FORDEAD le masque de dépérissement de la zone
`_tot` (là où FORDEAD le calcule et le met en cache), puis le restreindre à la strate choisie,
comme le fait désormais le Plan de validation FAST.

## Constat (orchestrateur, 2026-11-14, lu sur `main` b050e977)

- Constat du dev 017 (journal du 2026-11-13) : sur une strate ≠ `_tot`, le masque FORDEAD du Plan
  de validation est introuvable (zone 9 rend un raster, zone 6 rend `NULL`).
- FORDEAD calcule et met en cache sur `_tot` : la carte le lit sur `_tot` puis découpe à la strate
  par `terra::mask` (`R/mod_monitoring_fordead_map.R:215`, `:257`, `:288-297`).
- Le Plan de validation FORDEAD lit le masque sur la **strate** :
  - génération : `.resolve_alert_raster()` → `nemeton::read_fordead_dieback_mask(con, zone_id, …)`
    (`R/service_validation_sampling.R:328-338`), appelé avec `mask_zone_id`, qui pour FORDEAD
    vaut la strate (`.fast_run_zone()`, `:107`, `:177-181`) ;
  - histogramme / auto-relâchement des témoins : `alert_mask_r`
    (`R/mod_validation_sampling.R:394-395`) ;
  - surcouche carte : `R/mod_validation_sampling.R:662-663` ;
  - provenance : `.extract_mask_run_id(project, source, mask_zone_id)`
    (`R/service_validation_sampling.R:168`, recherche dans `fordead/zone_<id>`, `:503-505`) ;
    sur une strate, `source_run_id` vaut aujourd'hui NA.
- Mécanisme du NULL : le cœur cherche `zone_<id>` sous le cache et rend NULL s'il n'existe pas
  (`nemeton/R/fordead_mask.R:85-86`) ; le run FORDEAD tourne sur `_tot`
  (`R/mod_monitoring.R:3189`, `fordead_zone_id()`).
- Le cœur restreint déjà le masque au polygone de la strate avant tirage
  (`nemeton/R/validation_sampling.R:170`, `.mask_raster_to_zone`, puis `:177` ; présent depuis
  v0.212.0, sous le plancher `Imports`) : aucun point hors strate possible, **la génération n'a
  pas besoin de découpe côté app**.
- La pondération continue FORDEAD lit **déjà** `_tot` via `.fordead_tot_zone_id()`
  (`R/service_validation_sampling.R:429-438`, `:451-456`) : le mécanisme existe, il manque au
  masque 0-4. (Vrai pour FORDEAD seulement : voir RECONFORT au point 5.)
- Le module FORDEAD ne reçoit pas de `run_zone_id_r` (`R/mod_monitoring.R:3982-3999`) ; il prend
  le défaut `run_zone_id_r = zone_id_r` (`R/mod_validation_sampling.R:241`).
- Le tirage reste borné par le polygone de la **strate** (`zone`, `:99`) : le cœur restreint le
  raster à ce polygone (même logique que le chemin FAST, commentaire `:105-106`), donc les classes
  restent celles de `_tot` (présentation pure, parité FAST 017).

## Décisions de Pascal applicables

- 2026-11-13 : FAST aligné sur `_tot` (mission 017) ; 2026-11-14 : « Oui fais le » pour FORDEAD.
- Règles strictes CLAUDE.md n° 1-3 : aucune logique métier, découpe d'affichage seule.

## Travail attendu

1. Génération : pour FORDEAD, lire le masque sur la zone `_tot` (résolue par
   `.fordead_tot_zone_id()` ou un `run_zone_id` passé par le module, au choix du dev, en le
   justifiant). Le polygone de tirage reste la strate. **Ne pas** passer `mask_polygon` au lecteur
   cœur (le cœur découpe déjà, et les mocks existants ont une signature fixe
   `function(con, zone_id, run_id = NULL, cache_dir)`, `test-service_validation_sampling.R:84`,
   `:404`, `:450`, `:491`).
   - **Repli double (décision de l'orchestrateur, jamais pire qu'aujourd'hui)** : (a) aucune
     zone `_tot` en base → strate ; (b) zone `_tot` présente mais masque absent sur disque
     (ancien run fait sur la strate) → relire sur la strate. Le cas (b) protège les projets où la
     strate rend aujourd'hui un raster (zone 9).
   - La provenance (`source_run_id`, `:168`) suit la zone réellement lue (`_tot`, ou la strate
     en repli).
   - Si le dev passe par le module (`run_zone_id_r`, en réutilisant `fast_run_zone_id`,
     `R/mod_monitoring.R:1105`) : corriger la doc `R/service_validation_sampling.R:44-47`
     (« Ignored for FORDEAD / RECONFORT ») et `R/mod_validation_sampling.R:217`, et renommer
     `.fast_run_zone` (`:177`) s'il devient général.
2. Histogramme (`alert_mask_r`) et surcouche (`:662`) : masque `_tot` découpé à la strate, par un
   helper unique `.read_fordead_mask_stratum` (modèle `.read_fast_mask_stratum()`, avec le même
   repli double), testé directement ; réutiliser ou généraliser `.clip_fast_to_stratum` /
   `.fast_stratum_aoi` plutôt que dupliquer (si renommage, garder FAST vert).
3. Si la strate choisie **est** `_tot`, aucun changement de comportement.
4. Ne pas toucher FAST ni la pondération continue (déjà sur `_tot`).
5. **RECONFORT** : vérifier (lecture du code + test si simple) si le Plan de validation RECONFORT a
   le même défaut. Relecteur : oui, RECONFORT tourne sur `_tot` (`R/mod_monitoring.R:3527`,
   `:3809`, `:3878`) et le Plan le lit sur la strate à quatre endroits
   (`R/service_validation_sampling.R:350` et `:469` pondération continue,
   `R/mod_validation_sampling.R:397` et `:665`). **Ne pas corriger** : le signaler dans le
   rendu, c'est une question de périmètre pour Pascal.
6. Tests (`testServer` / tests de service avec mocks cœur `.package = "nemeton"`) :
   - génération FORDEAD sur une strate ≠ `_tot` : `read_fordead_dieback_mask` est appelé avec
     l'id `_tot`, le plan est produit et `source_run_id` vient de `_tot` ;
   - repli (a) sans zone `_tot` en base, et repli (b) masque `_tot` absent → lecture sur la
     strate ;
   - histogramme : le raster rendu est découpé à la strate (valeurs hors strate = NA) ;
   - strate = `_tot` : pas de découpe.
   Pièges : `.resolve_alert_raster` enveloppe le lecteur dans `tryCatch(error=)` (`:331-338`) :
   ne jamais mettre d'`expect_*` dans un mock, capturer `zone_id` dans un environnement et
   asserter dehors. Le test « hors strate = NA » demande un vrai `terra::mask` sur un raster réel
   (`get_monitoring_zone_aoi` mocké, modèle `test-service_validation_sampling_tot.R:149-172`).
   `alert_mask_r` exige `app_state$active_main_tab = "monitoring"` (`:372`).
   Chaque test prouvé par mutation (au moins 3 mutations rouges, rendues dans le rapport).
7. `grep -nP "[^\x00-\x7F]"` vide sur chaque fichier R touché.

## Versionnage

Cycle dev `2026.10.18.9011` → `2026.10.18.9012` si la PR #268 (018) est fusionnée avant ;
sinon partir de `main` et le signaler (l'orchestrateur rebasera le numéro). Entrée NEWS « Non
publié », section correctifs. Pas de release.

## Tests

Commande : `Rscript -e 'devtools::test(filter = "validation|monitoring")'` ; rendre pass / fail /
skip.

## Moyens

- Homme : `dev` (sonnet), worktree depuis `main`.
- Relecteur sur la fiche avant dev, puis sur la PR.
- Temps de la main : *appuyer*.

## Redite par l'agent : _à compléter_

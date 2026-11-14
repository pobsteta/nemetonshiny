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
  - surcouche carte : `R/mod_validation_sampling.R:662-663`.
- La pondération continue FORDEAD lit **déjà** `_tot` via `.fordead_tot_zone_id()`
  (`R/service_validation_sampling.R:429-438`, `:451-456`) : le mécanisme existe, il manque au
  masque 0-4.
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
   justifiant), avec repli explicite sur la strate quand `_tot` est introuvable. Le polygone de
   tirage reste la strate.
2. Histogramme (`alert_mask_r`) et surcouche (`:662`) : masque `_tot` découpé à la strate, sur le
   modèle de `.read_fast_mask_stratum()` (réutiliser ou généraliser `.clip_fast_to_stratum` /
   `.fast_stratum_aoi` plutôt que dupliquer ; si renommage, garder FAST vert).
3. Si la strate choisie **est** `_tot`, aucun changement de comportement.
4. Ne pas toucher FAST ni la pondération continue (déjà sur `_tot`).
5. **RECONFORT** : vérifier (lecture du code + test si simple) si le Plan de validation RECONFORT a
   le même défaut (`read_reconfort_alert_mask(con, zone_id)` sur la strate, alors que RECONFORT
   met en cache sur `_tot`, `R/mod_monitoring.R:3515`). **Ne pas corriger** : le signaler dans le
   rendu, c'est une question de périmètre pour Pascal.
6. Tests (`testServer` / tests de service avec mocks cœur `.package = "nemeton"`) :
   - génération FORDEAD sur une strate ≠ `_tot` : `read_fordead_dieback_mask` est appelé avec
     l'id `_tot` et le plan est produit ;
   - repli : sans zone `_tot`, lecture sur la strate ;
   - histogramme : le raster rendu est découpé à la strate (valeurs hors strate = NA) ;
   - strate = `_tot` : pas de découpe.
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

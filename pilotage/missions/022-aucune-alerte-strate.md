# Mission 022 — « Aucune alerte dans cette strate » au lieu de « zone saine » (FAST, FORDEAD, RECONFORT)

> Créée par l'orchestrateur le 2026-11-14 sur décision de Pascal (« oui "aucune alerte dans cette
> strate" »), suite de la relecture de 017. Parité des trois moteurs voulue par Pascal (« de la
> même façon sur FAST, FORDEAD et RECONFORT »).

## Mission

**En une phrase :** Quand la strate choisie (≠ `_tot`) n'a aucune alerte, les cartes de santé
disent « Aucune alerte dans cette strate » et rappellent combien la zone `_tot` en compte, au lieu
d'un « zone saine » vert qui laisse croire que toute la forêt est saine.

## Constat (orchestrateur, 2026-11-14, lu sur `main` 33a0fc42)

- **FAST, Alertes FAST** : bandeau vert `monitoring_fast_alerts_empty_title` / `_body`
  (`R/mod_monitoring_fast_alerts.R:965-975`) dès que `.fast_raster_is_empty(r)` sur le raster
  **découpé** à la strate (`raster_r`, `:574-575`). Le masque `_tot` non découpé est dans
  `raster_rv()` (`:552-555`, mission 017) : le compte `_tot` est disponible sans recalcul.
- **FORDEAD, carte** : carte « Zone saine — aucune anomalie détectée »
  (`monitoring_fordead_no_alerts_title` / `_body`, `R/mod_monitoring_fordead_map.R:414-432`)
  quand `.fordead_raster_max(r) < 1` sur le raster **découpé** à la strate (`:283-305`). Le
  masque `_tot` est lu avant la découpe.
- **RECONFORT, carte** : pas d'état « aucune alerte » ; la classe 1 (sain) est rendue
  transparente (`R/mod_monitoring_reconfort_map.R:338-345`) : une strate sans pixel 2/3 donne une
  carte vide sans message. Le masquage est fait par le lecteur cœur (`masked_rasters_r`).
- Clés i18n : `R/utils_i18n.R:4514-4529` (FAST), `:5051-5058` (FORDEAD).

## Règle

- Strate = `_tot` (ou pas de `_tot`) : **comportement actuel inchangé** (« zone saine » / « aucune
  alerte FAST sur la fenêtre »).
- Strate ≠ `_tot`, aucune alerte dans la strate :
  - titre : « Aucune alerte dans cette strate » ;
  - si `_tot` a des alertes : rappel « La zone entière (_tot) compte N pixels en alerte » (N
    formaté, unité pixels) ; ton **informatif** (pas vert « succès » : classe `alert-info` ou
    équivalent du thème, icône neutre), pour ne pas laisser croire que la forêt est saine ;
  - si `_tot` n'a pas d'alerte non plus : garder le vert actuel.
- RECONFORT : ajouter le même état (strate ≠ `_tot` sans pixel 2/3), présenté comme FORDEAD
  (surcouche), avec le même rappel ; ne rien changer quand des pixels 2/3 existent.

## Travail attendu

1. Un helper unique, testable hors Shiny, qui rend l'état (`aucun`, `strate_vide_tot_alerte`,
   `tout_sain`) et le compte `_tot` à partir du raster découpé, du raster `_tot` et de la règle
   d'alerte propre à chaque moteur (FAST : classe ≥ 1 du masque 0-4 ; FORDEAD : classe ≥ 1 ;
   RECONFORT : classes 2-3). Pas de logique métier : on compte des pixels déjà classés par le
   cœur.
2. Brancher FAST (`:965`), FORDEAD (`:414`) et RECONFORT (overlay) sur ce helper.
3. Nouvelles clés i18n FR/EN (convention NMT, ≤ 30 caractères, `\uXXXX`), par ex.
   `sante_strate_sans_alerte_titre`, `sante_strate_rappel_tot` (avec `%s`).
4. Le raster `_tot` non découpé doit être accessible dans chaque module sans relecture coûteuse :
   FAST `raster_rv()` ; FORDEAD et RECONFORT : réutiliser ce qui est déjà lu avant la découpe, ou
   le dire si ce n'est pas possible (pas de nouvelle lecture disque à chaque rendu).
5. Tests : helper (les 3 états × 3 moteurs) ; un `testServer` par module montrant le nouveau texte
   sur strate ≠ `_tot` et l'ancien sur `_tot`. Au moins 3 mutations rouges (une par moteur).
6. `grep -nP "[^\x00-\x7F]"` vide sur chaque fichier R touché.

## Hors périmètre

- Le Plan de validation (mission 021 en cours, fichiers `service_validation_sampling.R` et
  `mod_validation_sampling.R` : ne pas les toucher).
- La palette de la surcouche RECONFORT du Plan de validation (question à Pascal).

## Versionnage

Cycle dev : prochain numéro libre (`2026.10.18.9015` si 021 fusionne avant, sinon `.9014` et
l'orchestrateur renumérote). Entrée NEWS « Non publié », section changements visibles.

## Tests

Machine chargée : premier plan, `timeout 540`, filtres ciblés
(`mod_monitoring_fast_alerts|mod_monitoring_fordead_map|mod_monitoring_reconfort_map|i18n`) ;
rendre pass / fail / skip.

## Moyens

- Homme : `dev` (sonnet), worktree depuis `main`.
- Relecteur sur la fiche avant dev, puis sur la PR.
- Temps de la main : *appuyer*.

## Redite par l'agent : _à compléter_

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
  masque `_tot` n'est qu'une **variable locale** de `mask_r` (`:254-305`), découpée dans la même
  réactive ; le booléen « strate ≠ `_tot` » (`!is.na(id_tot) && sel != id_tot`, `:294`) y reste
  aussi. L'overlay lit `mask_r`, pas `display_r` (filtré par date).
- **RECONFORT, carte** : pas d'état « aucune alerte » ; la classe 1 (sain) est rendue
  transparente (`R/mod_monitoring_reconfort_map.R:338-345`) : une strate sans pixel 2/3 donne une
  carte vide sans message. Le cœur découpe à la lecture :
  `read_reconfort_layer(mask_polygon = aoi_r(), apply_zone_mask = TRUE)` (`:452-457`), `aoi_r`
  (`:184-194`) = polygone de la strate : **aucun raster `_tot` en mémoire**.
  `apply_zone_mask = FALSE` ne rend pas `_tot` (bbox + emprise feuillus OSO, cœur
  `reconfort_manifest.R`).
- Clés i18n : `R/utils_i18n.R:4514-4529` (FAST), `:5051-5058` (FORDEAD).

## Règle

Détection « strate ≠ `_tot` » par module (à tester aussi sans zone `_tot`) :
- FAST : `aoi_r()` non NULL (`R/mod_monitoring_fast_alerts.R:560-572`) ; sans `_tot`,
  `fast_run_zone_id` retombe sur `input$zone_id` (`R/mod_monitoring.R:1105-1107`) → inchangé.
- FORDEAD : `id_tot` contre `sel` (`R/mod_monitoring_fordead_map.R:215-222`, `:294`), à sortir de
  `mask_r`.
- RECONFORT : `zone_id_r()` contre `run_zone_id_r()`. **Piège** : `run_zone_id_r =
  reactive(as.character(fordead_zone_id()))` (`R/mod_monitoring.R:3527`) vaut la chaîne `"NA"`
  sans `_tot` (`nzchar("NA")` est vrai) → convertir par `as.integer()` puis `is.na()`.

- Strate = `_tot` (ou pas de `_tot`) : **comportement actuel inchangé** (« zone saine » / « aucune
  alerte FAST sur la fenêtre »).
- Strate ≠ `_tot`, aucune alerte dans la strate :
  - titre : « Aucune alerte dans cette strate » ;
  - si `_tot` a des alertes : rappel « La zone entière (_tot) compte N pixels en alerte » (N
    formaté, unité pixels) ; ton **informatif** (pas vert « succès » : classe `alert-info` ou
    équivalent du thème, icône neutre), pour ne pas laisser croire que la forêt est saine ;
  - si `_tot` n'a pas d'alerte non plus : garder le vert actuel.
- RECONFORT : ajouter le même état (strate ≠ `_tot` sans pixel 2/3), présenté comme FORDEAD
  (surcouche), avec le même rappel ; ne rien changer quand des pixels 2/3 existent. **Seulement
  si la couche affichée est `classification`** (comme FORDEAD se limite à `severity`) ; sur
  `score` / `probability`, rien ne change.
- Rendu : FAST bandeau `alert-info` + icône neutre (`info-circle`) ; FORDEAD et RECONFORT carte
  `border-info`, icône en `text-info` (pas `border-success` / `check-circle-fill`). Retirer le
  `text-white` du corps FAST si le texte change. `info` = `#2B5B8B` (`R/utils_theme.R:47`).

Arbitrages de l'orchestrateur (questions soulevées par la relecture, tranchées au plus prudent,
à revoir par Pascal sur le terrain) :
- **Sol nu FORDEAD (classe 4)** : compte comme alerte, comme aujourd'hui (`.fordead_raster_max
  < 1` = sain) — aucune règle métier nouvelle.
- **Strate entièrement NA** (aucune donnée, ex. hors emprise feuillus OSO) : pas de « aucune
  alerte dans cette strate » ; on garde le comportement actuel. Le nouvel état exige au moins un
  pixel non NA dans la strate.
- **Unité du rappel** : nombre de pixels **et** surface approximative en hectares (pixels ×
  surface d'une cellule, présentation pure), par ex. « La zone entière compte 1 234 pixels en
  alerte (≈ 12,3 ha) ».

## Travail attendu

1. Un helper unique, testable hors Shiny, dans un fichier neuf (`R/utils_sante_strate.R`), qui
   rend l'état (`alertes_strate` = rien de nouveau à afficher, `strate_vide_tot_alerte`,
   `tout_sain`) et le compte `_tot` à partir du raster découpé, du raster `_tot` et de la règle
   d'alerte propre à chaque moteur (FAST : classe ≥ 1 du masque 0-4 ; FORDEAD : classe ≥ 1 ;
   RECONFORT : classes 2-3). Strate = `_tot` revient à l'appeler sans raster `_tot`
   (comportement inchangé). Pas de logique métier : on compte des pixels déjà classés par le
   cœur. Ne pas modifier `.clip_fast_to_stratum` ni `.fast_stratum_aoi` (réutilisés par 021).
2. Brancher FAST (`:965`), FORDEAD (`:414`) et RECONFORT (overlay) sur ce helper.
3. Nouvelles clés i18n FR/EN (convention NMT, ≤ 30 caractères, `\uXXXX`), par ex.
   `sante_strate_sans_alerte_titre`, `sante_strate_rappel_tot` (avec `%s`).
4. Raster `_tot` non découpé : FAST `raster_rv()` (même signature fenêtre/seuils/mode que
   l'affichage) ; FORDEAD : scinder `mask_r` en `mask_tot_r` (lecture) et `mask_r` (découpe),
   sans nouvelle lecture, compte sur le masque complet (pas `display_r`) ; RECONFORT : **une**
   relecture de la seule ligne `classification` avec `mask_polygon` = AOI de `run_zone_id_r()`,
   dans une réactive mise en cache sur le manifeste et la zone de calcul (une fois par run, jamais
   à chaque rendu), jamais `apply_zone_mask = FALSE`.
5. Tests : helper (les 3 états × 3 moteurs) ; un `testServer` par module montrant le nouveau texte
   sur strate ≠ `_tot` et l'ancien sur `_tot`. Au moins 3 mutations rouges (une par moteur).
6. `grep -nP "[^\x00-\x7F]"` vide sur chaque fichier R touché.

## Hors périmètre

- Le Plan de validation (mission 021 en cours, fichiers `service_validation_sampling.R` et
  `mod_validation_sampling.R` : ne pas les toucher).
- La palette de la surcouche du Plan de validation : tranchée par Pascal le 2026-11-14, traitée
  par la mission 023 (lancée après 022).

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

## Statut

Terminée le 2026-11-14 : PR #274 fusionnée sur `main` (`7d11cdc4`), cycle dev `2026.10.18.9015`, non publié. Relecture : prêt.

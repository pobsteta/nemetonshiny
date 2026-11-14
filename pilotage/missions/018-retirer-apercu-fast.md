# Mission 018 — Retirer l'aperçu FAST mort du Plan de validation (`preview_raster_r`)

> Créée par l'orchestrateur le 2026-11-14 sur décision de Pascal (« Retire le »), suite de la
> relecture de la mission 017.

## Mission

**En une phrase :** Supprimer la réactive `preview_raster_r` de `R/mod_validation_sampling.R`,
qui ne sert plus à rien, sans changer ce que voit l'utilisateur.

## Constat (orchestrateur, 2026-11-14, lu sur `main` b050e977)

- `preview_raster_r` (`R/mod_validation_sampling.R:279-331`, en-tête v0.45.0 `:273-278`) ne rend un raster que pour la source
  FAST (`if (!identical(current_source(), "FAST")) return(NULL)`). Elle lit la zone choisie
  (`zone_id_r()`), pas `_tot` : elle n'a pas été alignée par la mission 017.
- Son **seul** lecteur est l'observateur des libellés de classes (`:333-356`, sortie FAST/RECONFORT `:342`), qui **sort tout de
  suite** pour FAST et RECONFORT (`if (identical(src, "RECONFORT") || identical(src, "FAST"))
  return()`). Il n'appelle donc `preview_raster_r()` que pour FORDEAD, où elle vaut toujours
  `NULL`.
- `.fast_class_labels()` (`R/mod_monitoring_fast_alerts.R:1244`) rend pour FORDEAD des libellés
  statiques **sans lire `r`**. Supprimer la réactive ne change donc aucun libellé.
- Effet de bord actuel : pour FORDEAD, l'appel ouvre une dépendance réactive inutile (et pour
  FAST, rien n'est lu puisque l'observateur sort avant). Aucun accès base ni lecture raster n'a
  lieu aujourd'hui depuis l'UI : la suppression ne coûte rien en comportement.
- L'observateur reste utile : l'UI pose les libellés FORDEAD en dur en français
  (`R/mod_validation_sampling.R:29`, `:130-139`, `get_i18n("fr")`), c'est lui qui les traduit en
  anglais. On le garde.
- Déjà sans lecteur dans l'app avant 018 (l'observateur sort pour FAST), et toujours après :
  branche FAST de `.fast_class_labels` (`R/mod_monitoring_fast_alerts.R:1257-1285`),
  `.classify_alert_count` (`:1193`), `.alert_count_palette` (`:1293`), clés i18n
  `validation_class_fast_1..4` (`R/utils_i18n.R:5580-5595`) et
  `validation_class_unit_days|deficit|trend` (`:5565-5576`). **Hors périmètre** : on les signale,
  on ne les retire pas (question de périmètre à Pascal).
- Aucun test ne cite `preview_raster` (`grep -rn preview tests/testthat/test-*validation*`).

## Travail attendu

1. Supprimer `preview_raster_r` et son commentaire d'en-tête (v0.45.0) devenu faux.
2. Dans l'observateur des libellés, remplacer `preview_raster_r()` par `NULL` passé à
   `.fast_class_labels()` ; corriger le commentaire qui dit que FAST charge le raster d'alerte.
3. Corriger aussi les commentaires devenus faux hors du module (commentaires seuls, aucun code) :
   `R/mod_monitoring_fast_alerts.R:1132-1136` et `:1232-1237`, `R/mod_monitoring.R:3966-3969`,
   `R/utils_i18n.R:5560-5564`.
4. Ne **pas** toucher au chemin « classes » FAST, à `alert_mask_r`, à l'histogramme ni à la
   surcouche (avis du relecteur de 017 : garder l'API alignée).
5. Vérifier qu'aucun autre fichier (R/, tests/, inst/) ne cite `preview_raster_r`, et qu'aucune
   fonction ou clé i18n ne devient orpheline ; si une le devient, la signaler sans la retirer.
6. Test (n'existe pas : `test-mod_monitoring_fast_alerts.R:125` teste la fonction seule) : un
   `testServer` sur `mod_validation_sampling_server`, **en anglais** (`language = "en"`), source
   FORDEAD, qui intercepte `updateCheckboxGroupInput` par
   `testthat::local_mocked_bindings(..., .package = "shiny")` (modèle
   `tests/testthat/test-mod_pipeline.R:97-117`, `MockShinySession` n'expose pas
   `sendInputMessage`), avec un premier `session$setInputs()` pour déclencher l'observateur, et
   vérifie que les choix portent les libellés anglais `validation_class_fordead_*` dans l'ordre
   4, 3, 2, 1. Prouvé par mutation (inverser l'ordre, changer une clé → rouge).
7. `grep -nP "[^\x00-\x7F]" R/mod_validation_sampling.R` vide (pas d'accents bruts).

## Versionnage

Cycle dev `2026.10.18.9010` → `2026.10.18.9011` (DESCRIPTION + entrée NEWS « Non publié »,
section nettoyage). Pas de release (v2026.11.1 reportée).

## Tests

Commande : `Rscript -e 'devtools::test(filter = "validation|monitoring")'` (`.claude/test-command` ne filtre que `mcp`) ; rendre les
chiffres pass / fail / skip.

## Moyens

- Homme : `dev` (sonnet), worktree depuis `main`.
- Relecteur sur la fiche avant dev, puis sur la PR.
- Temps de la main : *appuyer* (petite suppression, risque faible).

## Redite par l'agent : _à compléter_

## Statut

Terminée le 2026-11-14 : PR #268 fusionnée sur `main` (`99959307`, commit de mission `e4d672fc`),
cycle dev `2026.10.18.9011`, non publié. Relecture : prêt. Reste à Pascal : retrait ou non des
éléments FAST sans lecteur (branche FAST de `.fast_class_labels`, `.classify_alert_count`,
`.alert_count_palette`, clés `validation_class_fast_*` / `validation_class_unit_*`).

# Mission 041 — Intégrité et correction LiDAR : le pipeline ne dit plus « ok » sur un échec

> Créée par l'orchestrateur le 2026-11-21 (Pascal : « fais ce qu'il faut »). Issue de la relecture
> de la PR #311 (mission 040) : défaut confirmé par lecture, antérieur aux missions 037-040.

## Mission

**En une phrase :** quand `run_desserte_integrite()` ou `run_desserte_lidar_correction()` rendent
`list(status = "error", reason = …)` dans une tâche qui « réussit », le rapport du pipeline affiche
l'échec et son motif précis, comme l'onglet.

## Constat (orchestrateur, `main` au 2026-11-21, `1e5956fb`)

- `R/mod_desserte.R:1150-1159` : `shiny::observe` sur `integ_task$status()` → `"ok"` dès que la
  tâche est `success`, sans lire `integ_task$result()$status`. Or `run_desserte_integrite()` rend
  des listes d'erreur (`R/service_desserte.R` ~325-350, ex. `desserte_no_foretaccess`).
  L'onglet (l. ~1170-1182) affiche bien l'erreur.
- `R/mod_accessibility.R:~886-898` : même schéma pour `correct_task` (correction LiDAR) ; seul le
  cas succès porte les alertes de tuiles (mission 038). `run_desserte_lidar_correction()` rend des
  listes d'erreur (`R/service_accessibility.R` ~533-666).
- `R/service_accessibility.R:641` : `reason = "acc_correct_invariant_broken"` sans clé dans
  `R/utils_i18n.R` (le rapport et la notification afficheraient la clé brute).
- Outils existants (`R/service_pipeline.R`) : `pipeline_engine_outcome(res, i18n, defaut, …)`
  (statut + message + alertes, mission 037/038), `pipeline_reason_message()` (mission 040, PR #311
  pas encore sur `main` au moment de la fiche), `pipeline_task_error()`.

## Travail attendu

1. Les deux observateurs de fin de tâche (pipeline) passent par `pipeline_engine_outcome()` (ou
   équivalent pur) appliqué à `task$result()` : `success` + `res$status == "error"` → `"error"` +
   `i18n$t(reason)` + `detail` ; `success` + alertes de tuiles → `"ok"` + alerte (garder le
   comportement 038) ; tâche en `error` → `pipeline_task_error()` comme aujourd'hui.
2. Remplacer les `shiny::observe` orphelins par `observeEvent(task$status(), …)` si cela ne change
   pas le comportement (règle du CLAUDE.md) ; sinon le dire dans la PR.
3. Clé i18n `acc_correct_invariant_broken` FR/EN (`\uXXXX`, ≤ 30 car. ; le sens : la correction a
   cassé un invariant du réseau — lire le code autour de la l. 641 pour le libellé exact). Balayer
   les `reason = "…"` de `service_desserte.R` et `service_accessibility.R` : toute clé absente de
   `TRANSLATIONS` est ajoutée ; test qui le verrouille (liste extraite des sources ou liste
   explicite).
4. Ne pas toucher à `run_desserte_typage()` ni à l'étape `desserte_typage` (PR #311).

## Tests

Test direct de la décision (helper pur) pour les quatre issues : succès, succès avec alerte, succès
rendant `status = "error"` (motif + detail), tâche en erreur. Test de présence des clés i18n.
Mutations rouges (ignorer `res$status` → rouge ; retirer la clé → rouge). Fichier par fichier,
`timeout 540` : `test-service_pipeline.R`, `test-mod_desserte.R`, `test-mod_accessibility.R`,
`test-service_tuiles_en_echec*.R`, `test-utils_i18n*.R`.

## Versionnage

`2026.11.2.9011` (la PR #311 en `.9010` est en CI), entrée NEWS (`fix`).

## Moyens

- Homme : `dev` (sonnet), worktree depuis `origin/main`, branche `claude/041-integrite-correction-statut`.
- Relecteur sur la PR. Temps de la main : *appuyer*.
- Terrain (Pascal) : un pipeline où l'intégrité échoue doit montrer l'étape en erreur avec son motif.

## Statut

Terminée le 2026-11-22 : PR #312 fusionnée (`accd2789`, `.9011`). Relecture : reprises faites (clés `acc_profil_*` mortes retirées, NEWS corrigée).

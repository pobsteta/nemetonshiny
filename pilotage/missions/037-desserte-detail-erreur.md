# Mission 037 — Pipeline : la vraie erreur de la desserte (et de l'accessibilité) dans le rapport

> Créée par l'orchestrateur le 2026-11-21 (« lis et code le brief »). Solde
> `~/dev/briefs/vers-nemetonshiny/2026-11-21-desserte-detail-erreur.md` — **le lire**.
> Cause racine hors app : foretaccess 2.6.0, `.rbind_features` plante sur `as.Date("14791")`
> dès que l'emprise dépasse une tuile WFS de 2 km (brief parallèle vers foretaccess).

## Mission

**En une phrase :** quand une étape desserte ou accessibilité échoue dans le lancement enchaîné,
le rapport du pipeline montre la cause précise (le `detail`), plus seulement « vérifiez la
connexion réseau ».

## Constat (orchestrateur, lu sur `main` le 2026-11-21)

- `R/service_desserte.R:1161` rend `list(status = "error", reason = "desserte_desserte_failed",
  detail = desserte$msg)` ; idem `desserte_typage_failed` (l. 1475, 1501) et
  `R/service_accessibility.R:395` (`accessibility_desserte_failed`, `detail`).
- `R/mod_desserte.R:752-757` : `pipeline_answer()` reçoit **seulement** `i18n$t(res$reason)`. Le
  `detail` n'est ajouté qu'à la `showNotification` (l. 759-768), invisible pendant un run.
- Même schéma dans `R/mod_accessibility.R:1140-1156`.
- `R/mod_desserte.R:1484-1502` (étape `desserte_typage`) : `.lancer_typage()` rend
  TRUE/NA/NULL/FALSE, le pipeline reçoit `i18n$t("desserte_typage_failed")` nu, le `detail` (dans
  `rv_typage()`) est perdu aussi.
- `pipeline_task_error()` (`R/service_pipeline.R:397`) tronque déjà à ~300 caractères ; s'en
  inspirer (ou factoriser un petit helper commun, par exemple dans `service_pipeline.R`).
- Clés : `R/utils_i18n.R:198` (`accessibility_desserte_failed`, accuse OpenStreetMap + réseau),
  `:359` (`desserte_desserte_failed`, accuse le réseau).

## Travail attendu

1. Un helper (ex. `pipeline_error_message(i18n_msg, detail, max = 300)`) : `msg` + `" — "` +
   `.strip_ansi(detail)` tronqué quand `detail` est non vide. Utilisé à la fois par
   `pipeline_answer()` et par la `showNotification` (un seul calcul de `msg`).
2. L'appliquer dans `mod_desserte.R` (fin de tâche moteur ET étape `desserte_typage`, en lisant le
   `detail` de `rv_typage()`) et dans `mod_accessibility.R`. Balayer les autres
   `pipeline_answer(..., i18n$t(res$reason))` : tout `detail` disponible doit remonter.
3. Reformuler `desserte_desserte_failed` et `accessibility_desserte_failed` (FR/EN) sans accuser le
   réseau d'office (« Échec de l'acquisition de la desserte existante (IGN BD TOPO). »). Encodage
   `\uXXXX` ; vérifier `grep -nP "[^\x00-\x7F]" R/*.R` après édition (l'outil Edit décode les
   échappements).
4. Facultatif du brief, à faire si simple : quand l'étape `desserte` du même run est en `error`,
   `desserte_typage` répond `skipped` avec un motif nouveau (clé i18n, ex.
   `pipeline_skip_desserte_ko`, ≤ 30 caractères) au lieu d'un `error` en cascade. Regarder d'abord
   si `mod_pipeline.R` garde l'état des étapes déjà jouées ; si ce n'est pas simple, ne pas le faire
   et le dire dans la PR.
5. **Hors mission** : la montée du plancher foretaccess attend sa release corrigée.

## Tests

`testServer()` (ou test direct du helper) : un `res` en erreur avec `detail` → le message passé à
`pipeline_answer` contient le `detail` (sans codes ANSI, tronqué au-delà de 300 caractères) ; sans
`detail` → la seule traduction. Pour l'étape `desserte_typage`, même vérification. Mutation rouge
(retirer le `detail` du message pipeline → le test échoue). Filtre
`desserte|accessibility|pipeline|i18n`, `timeout 540`.

## Versionnage

`2026.11.2.9007` (main était déjà en `.9006`), entrée NEWS (`fix`).

## Moyens

- Homme : `dev` (sonnet), worktree depuis `main`, branche `claude/037-desserte-detail-erreur`.
- Relecteur sur la PR. Temps de la main : *appuyer*.
- Terrain (Pascal) : avec foretaccess 2.6.0, relancer le pipeline sur `20261121_083914_gwsa` ; le
  rapport de l'étape `desserte` doit contenir `character string is not in a standard unambiguous
  format`.

## Statut

Terminée le 2026-11-21 : PR #308 fusionnée (`b518ee1c`, `.9007`, plancher `foretaccess (>= 2.6.1)`). Relecture : à reprendre (léger), reprises faites ; `desserte_integrite` aussi ignorée en cascade (décision de Pascal). Reste : essai sur `20261121_083914_gwsa` (desserte en `ok` avec foretaccess 2.6.1).

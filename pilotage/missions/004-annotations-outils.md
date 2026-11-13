# Mission 004 — Annotations des dix outils MCP

> Gabarit (fiches n° 9 et 15 de la méthode). Rempli par l'orchestrateur le 2026-11-09,
> **proposé à Pascal (porte n° 2)**. Une mission = une phrase = une branche = une PR.

## Mission

**En une phrase (quoi, et pourquoi) :** Poser sur chaque outil de `mcp_tools()` les annotations `read_only_hint`, `destructive_hint` et `idempotent_hint` justes, pour que les clients sachent ce qui lit et ce qui écrit (critère 5).

**Redite par l'agent :** non rédigée (fiche close au bilan du 2026-11-13)

## Les trois questions (règle de staffing)

| Question | Réponse | Conséquence |
|---|---|---|
| Peut-il redire la mission en une phrase, avec son but ? | oui | |
| Sait-il comment faire ? (sources, conventions nommées ci-dessous) | oui | |
| A-t-il les moyens ? (outils, données d'essai, tests) | oui | |

**Temps de la main :** appuyer — dix appels `ellmer::tool_annotations()`, sans effet de bord.

## Ma réponse d'abord

_Rempli par Pascal seul, avant de lire le travail de l'agent._

## L'Homme

- **Agent :** `dev`
- **Modèle :** sonnet
- **Ce qui ne lui est pas demandé :** la confirmation (010).

## Harnais

- **Peut :** modifier `mcp_tools()` dans `R/service_mcp.R`, NEWS.
- **Ne doit pas :** sortir du périmètre, toucher à `main`, écrire dans `nemeton`, `nemetonmcp`, `aigora` ou `victor` (lecture seule), mettre un littéral FR/EN hors i18n, ajouter de la logique métier.
- **Sources autorisées :** spec critère 5 ; https://ellmer.tidyverse.org/reference/tool_annotations.html ; tests de 002.

## Valeur provisoire (chiffre, réglage)

- aucune.

## Dépend de / rouge attendu jusqu'à

- 002 ; peut fusionner avant ou après 003 (pas de conflit attendu hors `service_mcp.R` : se rebaser).

## Tests

- **Comment saura-t-on que c'est juste :** tests de 002 (annotations) verts. Commande : `.claude/test-command` (`devtools::test()`).

## Terrain

- **Échantillon que Pascal vérifiera :** —

## Qui veille, qui tranche

- **Relecteur :** `relecteur`, puis Pascal.
- **Critère d'arrêt de la relecture :** 3 allers-retours ; au-delà, la décision remonte à Pascal.
- **Point de suivi :** à la remise de la PR (ou du compte rendu), et au plan d'abord si la main est tenue.
- **Qui tranche :** Pascal.

## Comment saura-t-on que ça marche ?

`tools/list` montre `readOnlyHint: true` sur `resume_projet`.

## Moyens et arbitrages — 2026-11-10

- **Premier commit de la branche** : `git fetch origin claude/mission-002-tests-acceptation && git checkout 37cbbf92 -- tests/testthat/test-service_mcp_annotations.R` (tests d'acceptation de 002, à ne pas retoucher ; ils doivent passer au vert sans retouche).
- Annotations arbitrées par Pascal (journal du 2026-11-10) :
  - `read_only_hint` TRUE : `lister_projets`, `resume_projet`, `etat_calcul`, `url_app` ; FALSE pour les 6 autres (dont `generer_rapport`, `exporter_gpkg`).
  - `destructive_hint` TRUE : `appliquer_ugf`, `croiser_onf` seulement.
  - `idempotent_hint` FALSE : `lancer_calcul` seul ; TRUE pour les 9 autres.
- Branche à partir de `origin/main` (contient #253 : le contrat ; ne change aucune signature listée dans `CONTRAT.md` § 6 bis). Indépendante de 003 (#256) : attention aux conflits sur `R/service_mcp.R` (`mcp_tools()`), à signaler au rendu.
- Cycle dev : 003 est en 2026.10.18.9002 ; prends 2026.10.18.9003 + entrée NEWS.

## Mise à jour sur main — 2026-11-10

#256 (003) est fusionnée sur `main` (86138435). Point de départ : `origin/claude/mission-004-annotations-outils` (1b6930c3). `git merge origin/main` (pas de rebase : push forcé refusé), puis résoudre :
- `R/service_mcp.R`, `mcp_tools()` : chaque outil garde **à la fois** `.mcp_marquer(...)` (de 003) et ses `annotations = ellmer::tool_annotations(...)` (de 004) : `ellmer::tool(.mcp_marquer(mcp_x), name = "x", annotations = ..., description = ...)`.
- `DESCRIPTION` : **une seule** ligne `Version: 2026.10.18.9003`.
- `NEWS.md` : entrée 9003 au-dessus de 9002, une ligne vide entre elles.
Puis `devtools::test(filter = "mcp|contrat")` vert (annotations ET erreurs ; 2 skips attendus de 010) ; `git diff 37cbbf92 -- tests/testthat/test-service_mcp_annotations.R` vide. Pousser en fast-forward sur la branche de #257.

## Statut

fusionnée #257 (9cdd1f3c), 2026-11-10.

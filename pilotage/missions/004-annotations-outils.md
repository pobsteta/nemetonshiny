# Mission 004 — Annotations des dix outils MCP

> Gabarit (fiches n° 9 et 15 de la méthode). Rempli par l'orchestrateur le 2026-11-09,
> **proposé à Pascal (porte n° 2)**. Une mission = une phrase = une branche = une PR.

## Mission

**En une phrase (quoi, et pourquoi) :** Poser sur chaque outil de `mcp_tools()` les annotations `read_only_hint`, `destructive_hint` et `idempotent_hint` justes, pour que les clients sachent ce qui lit et ce qui écrit (critère 5).

**Redite par l'agent :** _à compléter par l'agent avant d'agir — s'il n'y arrive pas, il s'arrête._

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

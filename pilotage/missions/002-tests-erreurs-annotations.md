# Mission 002 — Tests d'acceptation : erreurs classées et annotations

> Gabarit (fiches n° 9 et 15 de la méthode). Rempli par l'orchestrateur le 2026-11-09,
> **proposé à Pascal (porte n° 2)**. Une mission = une phrase = une branche = une PR.

## Mission

**En une phrase (quoi, et pourquoi) :** Écrire, sans lire l'implémentation à venir, les tests d'acceptation des critères 3 (chaque outil mis en échec rend une erreur MCP `isError`, classée, FR/EN, avec action proposée) et 5 (annotations `read_only_hint`/`destructive_hint`/`idempotent_hint` justes dans `tools/list`), pour que les missions 003 et 004 ne s'auto-évaluent pas.

**Redite par l'agent :** _à compléter par l'agent avant d'agir — s'il n'y arrive pas, il s'arrête._

## Les trois questions (règle de staffing)

| Question | Réponse | Conséquence |
|---|---|---|
| Peut-il redire la mission en une phrase, avec son but ? | oui | |
| Sait-il comment faire ? (sources, conventions nommées ci-dessous) | oui | |
| A-t-il les moyens ? (outils, données d'essai, tests) | oui | |

**Temps de la main :** appuyer — la forme attendue est fixée par la spec et par le comportement mesuré de mcptools 1.0.3.

## Ma réponse d'abord

_Rempli par Pascal seul, avant de lire le travail de l'agent._

## L'Homme

- **Agent :** `testeur`
- **Modèle :** sonnet
- **Ce qui ne lui est pas demandé :** implémenter quoi que ce soit dans `R/`.

## Harnais

- **Peut :** écrire `tests/testthat/test-mcp_erreurs.R` et `test-mcp_annotations.R`, marqués `skip()` tant que 003/004 ne sont pas fusionnées, ou rouges annoncés.
- **Ne doit pas :** sortir du périmètre, toucher à `main`, écrire dans `nemeton`, `nemetonmcp`, `aigora` ou `victor` (lecture seule), mettre un littéral FR/EN hors i18n, ajouter de la logique métier.
- **Sources autorisées :** `pilotage/spec.md` § 1 (P6) et § 5 (critères 3, 5), journal du 2026-11-09 (comportement mcptools : `isError` seulement via `ellmer::ContentToolResult(error=)`, annotations transmises par `tool_annotations_as_json`), `R/service_mcp.R` pour les noms d'outils et de classes existants seulement.

## Valeur provisoire (chiffre, réglage)

- aucune.

## Dépend de / rouge attendu jusqu'à

- aucune ; précède 003 et 004.

## Tests

- **Comment saura-t-on que c'est juste :** un test par classe d'erreur (`projet_introuvable`, `projet_ambigu`, `projet_verrouille`, `calcul_en_cours`, `confirmation_requise`, `jeton_invalide`) et un test par outil pour les annotations. Commande : `.claude/test-command` (`devtools::test()`).

## Terrain

- **Échantillon que Pascal vérifiera :** —

## Qui veille, qui tranche

- **Relecteur :** `relecteur`, puis Pascal.
- **Critère d'arrêt de la relecture :** 3 allers-retours ; au-delà, la décision remonte à Pascal.
- **Point de suivi :** à la remise de la PR (ou du compte rendu), et au plan d'abord si la main est tenue.
- **Qui tranche :** Pascal.

## Comment saura-t-on que ça marche ?

les tests sont rouges sur `main` et verts après 003/004, sans retouche.

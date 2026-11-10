# Mission 009 — Tests d'acceptation : confirmation par jeton

> Gabarit (fiches n° 9 et 15 de la méthode). Rempli par l'orchestrateur le 2026-11-09,
> **proposé à Pascal (porte n° 2)**. Une mission = une phrase = une branche = une PR.

## Mission

**En une phrase (quoi, et pourquoi) :** Écrire, sans lire l'implémentation, les tests du critère 4 : chaque outil qui modifie un projet existant, appelé sans jeton, n'écrit rien (disque et base de test) et rend l'aperçu ; avec un jeton valide il écrit ; un jeton périmé, rejoué ou d'un autre projet est refusé.

**Redite par l'agent :** _à compléter par l'agent avant d'agir — s'il n'y arrive pas, il s'arrête._

## Les trois questions (règle de staffing)

| Question | Réponse | Conséquence |
|---|---|---|
| Peut-il redire la mission en une phrase, avec son but ? | oui | |
| Sait-il comment faire ? (sources, conventions nommées ci-dessous) | oui | |
| A-t-il les moyens ? (outils, données d'essai, tests) | oui | |

**Temps de la main :** appuyer — critère précis, ADR 0002 tranchée.

## Ma réponse d'abord

_Rempli par Pascal seul, avant de lire le travail de l'agent._

## L'Homme

- **Agent :** `testeur`
- **Modèle :** sonnet
- **Ce qui ne lui est pas demandé :** implémenter.

## Harnais

- **Peut :** écrire `tests/testthat/test-mcp_confirmation.R` ; base de test via `NEMETON_DB_URL_TEST` si présente, sinon `skip`.
- **Ne doit pas :** sortir du périmètre, toucher à `main`, écrire dans `nemeton`, `nemetonmcp`, `aigora` ou `victor` (lecture seule), mettre un littéral FR/EN hors i18n, ajouter de la logique métier.
- **Sources autorisées :** ADR 0002 (périmètre : toute modification d'un projet existant ; lecture, calcul, exports libres), spec P4, critère 4.

## Valeur provisoire (chiffre, réglage)

- durée du jeton 10 min : provisoire (ADR 0002), à confirmer par Pascal à l'usage vocal, gel avant la release.

## Dépend de / rouge attendu jusqu'à

- 003 (classes `confirmation_requise`, `jeton_invalide`) ; précède 010.

## Tests

- **Comment saura-t-on que c'est juste :** tests rouges jusqu'à 010. Commande : `.claude/test-command` (`devtools::test()`).

## Terrain

- **Échantillon que Pascal vérifiera :** —

## Qui veille, qui tranche

- **Relecteur :** `relecteur`, puis Pascal.
- **Critère d'arrêt de la relecture :** 3 allers-retours ; au-delà, la décision remonte à Pascal.
- **Point de suivi :** à la remise de la PR (ou du compte rendu), et au plan d'abord si la main est tenue.
- **Qui tranche :** Pascal.

## Comment saura-t-on que ça marche ?

—

# Mission 006 — Tests d'acceptation : noms dictés et résumé complet

> Gabarit (fiches n° 9 et 15 de la méthode). Rempli par l'orchestrateur le 2026-11-09,
> **proposé à Pascal (porte n° 2)**. Une mission = une phrase = une branche = une PR.

## Mission

**En une phrase (quoi, et pourquoi) :** Écrire, sans lire l'implémentation, les tests des critères 1 (la fixture 005 donne le bon projet ou une liste de candidats à 100 %, aucun « introuvable » à distance ≤ 2) et 2 (`resume_projet` rend tout ce qu'affiche la Synthèse, plus les heures ISO 8601 avec fuseau).

**Redite par l'agent :** _à compléter par l'agent avant d'agir — s'il n'y arrive pas, il s'arrête._

## Les trois questions (règle de staffing)

| Question | Réponse | Conséquence |
|---|---|---|
| Peut-il redire la mission en une phrase, avec son but ? | oui | |
| Sait-il comment faire ? (sources, conventions nommées ci-dessous) | oui | |
| A-t-il les moyens ? (outils, données d'essai, tests) | oui (après 005) | |

**Temps de la main :** appuyer — critères chiffrés dans la spec.

## Ma réponse d'abord

_Rempli par Pascal seul, avant de lire le travail de l'agent._

## L'Homme

- **Agent :** `testeur`
- **Modèle :** sonnet
- **Ce qui ne lui est pas demandé :** implémenter.

## Harnais

- **Peut :** écrire `tests/testthat/test-mcp_noms_dictes.R` et `test-mcp_resume.R` sur projet fixture temporaire (`withr::with_tempdir`).
- **Ne doit pas :** sortir du périmètre, toucher à `main`, écrire dans `nemeton`, `nemetonmcp`, `aigora` ou `victor` (lecture seule), mettre un littéral FR/EN hors i18n, ajouter de la logique métier.
- **Sources autorisées :** spec P1, critères 1-2 ; fixture 005 ; `R/mod_synthesis.R` pour savoir ce qu'affiche la Synthèse.

## Valeur provisoire (chiffre, réglage)

- distance ≤ 2 : provisoire (spec), mesurée sur la fixture 005, gel avant fusion de 007.

## Dépend de / rouge attendu jusqu'à

- 005 ; précède 007 et 008.

## Tests

- **Comment saura-t-on que c'est juste :** tests rouges sur `main`, verts après 007/008. Commande : `.claude/test-command` (`devtools::test()`).

## Terrain

- **Échantillon que Pascal vérifiera :** —

## Qui veille, qui tranche

- **Relecteur :** `relecteur`, puis Pascal.
- **Critère d'arrêt de la relecture :** 3 allers-retours ; au-delà, la décision remonte à Pascal.
- **Point de suivi :** à la remise de la PR (ou du compte rendu), et au plan d'abord si la main est tenue.
- **Qui tranche :** Pascal.

## Comment saura-t-on que ça marche ?

—

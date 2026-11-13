# Mission 011 — Mesure de durée des outils longs

> Gabarit (fiches n° 9 et 15 de la méthode). Rempli par l'orchestrateur le 2026-11-09,
> **proposé à Pascal (porte n° 2)**. Une mission = une phrase = une branche = une PR.

## Mission

**En une phrase (quoi, et pourquoi) :** Mesurer la durée de `generer_rapport` et de `croiser_onf` (premier appel, cache DGFiP vide, puis plein) sur le plus gros projet disponible, pour décider sur pièces ce qui doit être détaché (ADR 0004, critère 6).

**Redite par l'agent :** non rédigée (fiche close au bilan du 2026-11-13)

## Les trois questions (règle de staffing)

| Question | Réponse | Conséquence |
|---|---|---|
| Peut-il redire la mission en une phrase, avec son but ? | oui | |
| Sait-il comment faire ? (sources, conventions nommées ci-dessous) | oui | |
| A-t-il les moyens ? (outils, données d'essai, tests) | oui | |

**Temps de la main :** appuyer — mesure, pas de code ; Pareto des temps.

## Ma réponse d'abord

_Rempli par Pascal seul, avant de lire le travail de l'agent._

## L'Homme

- **Agent :** `analyste`
- **Modèle :** sonnet
- **Ce qui ne lui est pas demandé :** détacher quoi que ce soit.

## Harnais

- **Peut :** exécuter les deux outils dans un process plafonné (cf. mémoire OOM), écrire le compte rendu dans `pilotage/journal.md` (par l'orchestrateur).
- **Ne doit pas :** sortir du périmètre, toucher à `main`, écrire dans `nemeton`, `nemetonmcp`, `aigora` ou `victor` (lecture seule), mettre un littéral FR/EN hors i18n, ajouter de la logique métier.
- **Sources autorisées :** ADR 0004 ; `R/service_mcp.R` ; projets réels de `project_dir` (en copie, jamais sur l'original).

## Valeur provisoire (chiffre, réglage)

- seuil de 60 s : provisoire (ADR 0004), c'est ce que la mission éclaire ; 600 s : plafond VICTOR mesuré.

## Dépend de / rouge attendu jusqu'à

- aucune.

## Tests

- **Comment saura-t-on que c'est juste :** trois mesures par cas, durée médiane et maximum. Commande : `.claude/test-command` (`devtools::test()`).

## Terrain

- **Échantillon que Pascal vérifiera :** —

## Qui veille, qui tranche

- **Relecteur :** `relecteur`, puis Pascal.
- **Critère d'arrêt de la relecture :** 3 allers-retours ; au-delà, la décision remonte à Pascal.
- **Point de suivi :** à la remise de la PR (ou du compte rendu), et au plan d'abord si la main est tenue.
- **Qui tranche :** Pascal.

## Comment saura-t-on que ça marche ?

un tableau de durées qui tranche 012.

## Statut

mesure faite, a tranché 012 (pas de PR propre).

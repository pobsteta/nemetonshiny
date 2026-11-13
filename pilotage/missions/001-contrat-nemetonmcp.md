# Mission 001 — Contrat déclaré nemetonshiny → nemetonmcp

> Gabarit (fiches n° 9 et 15 de la méthode). Rempli par l'orchestrateur le 2026-11-09,
> **proposé à Pascal (porte n° 2)**. Une mission = une phrase = une branche = une PR.

## Mission

**En une phrase (quoi, et pourquoi) :** Déclarer dans `CONTRAT.md` (section « API pour nemetonmcp ») les objets internes que `nemetonmcp` lit par `getFromNamespace`, et les verrouiller par un test de présence et de signature, pour qu'un renommage casse la CI d'ici et pas VICTOR en production (ADR 0001).

**Redite par l'agent :** non rédigée (fiche close au bilan du 2026-11-13)

## Les trois questions (règle de staffing)

| Question | Réponse | Conséquence |
|---|---|---|
| Peut-il redire la mission en une phrase, avec son but ? | oui | |
| Sait-il comment faire ? (sources, conventions nommées ci-dessous) | oui | |
| A-t-il les moyens ? (outils, données d'essai, tests) | oui (dépôt consommateur lisible, `formals()` testable) | |

**Temps de la main :** appuyer — pas de code déplacé, l'ADR est tranchée ; seule la liste exacte demande du soin.

## Ma réponse d'abord

_Rempli par Pascal seul, avant de lire le travail de l'agent._

## L'Homme

- **Agent :** `dev`
- **Modèle :** sonnet
- **Ce qui ne lui est pas demandé :** exporter quoi que ce soit ; modifier `nemetonmcp` ; changer une signature existante.

## Harnais

- **Peut :** lire `nemetonmcp/r/R/*.R` (lecture seule) pour dresser la liste exacte ; écrire `CONTRAT.md`, `inst/mcp/README.md` (10 outils, purge de `croiser_onf` déclarée) et `tests/testthat/test-contrat_nemetonmcp.R`.
- **Ne doit pas :** sortir du périmètre, toucher à `main`, écrire dans `nemeton`, `nemetonmcp`, `aigora` ou `victor` (lecture seule), mettre un littéral FR/EN hors i18n, ajouter de la logique métier.
- **Sources autorisées :** `pilotage/adr/0001-contrat-nemetonmcp.md`, `pilotage/spec.md` § 3 et § 5 critère 7, `CONTRAT.md`, `inst/mcp/README.md`, `/home/pascal/dev/nemetonmcp/r/R/` (grep `.ns(`/`getFromNamespace`).

## Valeur provisoire (chiffre, réglage)

- aucune (la liste des 31 objets de la spec est à recompter, pas à reprendre).

## Dépend de / rouge attendu jusqu'à

- aucune.

## Tests

- **Comment saura-t-on que c'est juste :** le test liste chaque objet et ses `formals()` ; une mutation (renommer un objet en local) le fait échouer — à montrer dans la PR. Le test de mutation remplace ici le `testeur` : le test est le livrable. Commande : `.claude/test-command` (`devtools::test()`).

## Terrain

- **Échantillon que Pascal vérifiera :** Pascal lit la section de `CONTRAT.md` et y reconnaît ce qu'appelle VICTOR.

## Qui veille, qui tranche

- **Relecteur :** `relecteur`, puis Pascal.
- **Critère d'arrêt de la relecture :** 3 allers-retours ; au-delà, la décision remonte à Pascal.
- **Point de suivi :** à la remise de la PR (ou du compte rendu), et au plan d'abord si la main est tenue.
- **Qui tranche :** Pascal.

## Comment saura-t-on que ça marche ?

la CI rouge le jour où un objet du contrat est renommé.

## Statut

fusionnée #253 (46eee94b), 2026-11-10.

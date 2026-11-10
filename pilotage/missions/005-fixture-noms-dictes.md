# Mission 005 — Corpus de noms de forêts dictés de travers

> Gabarit (fiches n° 9 et 15 de la méthode). Rempli par l'orchestrateur le 2026-11-09,
> **proposé à Pascal (porte n° 2)**. Une mission = une phrase = une branche = une PR.

## Mission

**En une phrase (quoi, et pourquoi) :** Constituer une fixture de transcriptions approximatives (vocales réelles de VICTOR si on en trouve, sinon simulées et marquées comme telles) des noms de projets et de communes, avec le projet attendu, pour donner au critère 1 un moyen de mesure.

**Redite par l'agent :** _à compléter par l'agent avant d'agir — s'il n'y arrive pas, il s'arrête._

## Les trois questions (règle de staffing)

| Question | Réponse | Conséquence |
|---|---|---|
| Peut-il redire la mission en une phrase, avec son but ? | oui | |
| Sait-il comment faire ? (sources, conventions nommées ci-dessous) | oui | |
| A-t-il les moyens ? (outils, données d'essai, tests) | non → partiellement : transcriptions réelles incertaines ; d'où cette mission préalable à 006/007 | |

**Temps de la main :** appuyer — référentiel ; deux sources comparées (journaux VICTOR, noms réels des projets).

## Ma réponse d'abord

_Rempli par Pascal seul, avant de lire le travail de l'agent._

## L'Homme

- **Agent :** `acquereur`
- **Modèle :** sonnet
- **Ce qui ne lui est pas demandé :** écrire du code dans `R/`.

## Harnais

- **Peut :** lire `/home/pascal/dev/victor` (journaux, transcriptions), `projets_lister()` ; écrire `tests/testthat/fixtures/noms_dictes.csv` (colonnes `dit`, `attendu`, `source`).
- **Ne doit pas :** sortir du périmètre, toucher à `main`, écrire dans `nemeton`, `nemetonmcp`, `aigora` ou `victor` (lecture seule), mettre un littéral FR/EN hors i18n, ajouter de la logique métier.
- **Sources autorisées :** spec critère 1 et § 6 ; dépôt `victor` en lecture seule.

## Valeur provisoire (chiffre, réglage)

- la part de transcriptions simulées est provisoire, mesurée par les journaux VICTOR à venir.

## Dépend de / rouge attendu jusqu'à

- aucune.

## Tests

- **Comment saura-t-on que c'est juste :** le CSV est lu par 006 ; chaque ligne a une source. Commande : `.claude/test-command` (`devtools::test()`).

## Terrain

- **Échantillon que Pascal vérifiera :** Pascal relit la liste et y reconnaît ses erreurs de dictée habituelles.

## Qui veille, qui tranche

- **Relecteur :** `relecteur`, puis Pascal.
- **Critère d'arrêt de la relecture :** 3 allers-retours ; au-delà, la décision remonte à Pascal.
- **Point de suivi :** à la remise de la PR (ou du compte rendu), et au plan d'abord si la main est tenue.
- **Qui tranche :** Pascal.

## Comment saura-t-on que ça marche ?

006 peut écrire ses tests sur cette fixture.

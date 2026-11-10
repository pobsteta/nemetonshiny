# Mission 008 — Résumé de projet complet en un appel

> Gabarit (fiches n° 9 et 15 de la méthode). Rempli par l'orchestrateur le 2026-11-09,
> **proposé à Pascal (porte n° 2)**. Une mission = une phrase = une branche = une PR.

## Mission

**En une phrase (quoi, et pourquoi) :** Compléter `mcp_resume_projet` (UGF, date de calcul, heures ISO 8601 avec fuseau) pour qu'il rende tout ce qu'affiche l'onglet Synthèse, à écart 0, et qu'un seul appel suffise à « où en est Dabo ? » (P1, critère 2).

**Redite par l'agent :** _à compléter par l'agent avant d'agir — s'il n'y arrive pas, il s'arrête._

## Les trois questions (règle de staffing)

| Question | Réponse | Conséquence |
|---|---|---|
| Peut-il redire la mission en une phrase, avec son but ? | oui | |
| Sait-il comment faire ? (sources, conventions nommées ci-dessous) | oui | |
| A-t-il les moyens ? (outils, données d'essai, tests) | oui (après 006) | |

**Temps de la main :** appuyer — lecture seule, appel à `nemeton::compute_general_index()` déjà en place.

## Ma réponse d'abord

_Rempli par Pascal seul, avant de lire le travail de l'agent._

## L'Homme

- **Agent :** `dev`
- **Modèle :** sonnet
- **Ce qui ne lui est pas demandé :** aucune logique métier nouvelle.

## Harnais

- **Peut :** modifier `R/service_mcp.R`, `R/api.R`, NEWS.
- **Ne doit pas :** sortir du périmètre, toucher à `main`, écrire dans `nemeton`, `nemetonmcp`, `aigora` ou `victor` (lecture seule), mettre un littéral FR/EN hors i18n, ajouter de la logique métier.
- **Sources autorisées :** spec P1, critère 2 ; tests 006 ; `R/mod_synthesis.R`.

## Valeur provisoire (chiffre, réglage)

- aucune.

## Dépend de / rouge attendu jusqu'à

- 006.

## Tests

- **Comment saura-t-on que c'est juste :** tests de 006 (résumé) verts. Commande : `.claude/test-command` (`devtools::test()`).

## Terrain

- **Échantillon que Pascal vérifiera :** Pascal compare la réponse de VICTOR à l'écran Synthèse de Dabo.

## Qui veille, qui tranche

- **Relecteur :** `relecteur`, puis Pascal.
- **Critère d'arrêt de la relecture :** 3 allers-retours ; au-delà, la décision remonte à Pascal.
- **Point de suivi :** à la remise de la PR (ou du compte rendu), et au plan d'abord si la main est tenue.
- **Qui tranche :** Pascal.

## Comment saura-t-on que ça marche ?

écart 0 entre la voix et l'écran.

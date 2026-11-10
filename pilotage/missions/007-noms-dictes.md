# Mission 007 — Résolution tolérante des noms de projet

> Gabarit (fiches n° 9 et 15 de la méthode). Rempli par l'orchestrateur le 2026-11-09,
> **proposé à Pascal (porte n° 2)**. Une mission = une phrase = une branche = une PR.

## Mission

**En une phrase (quoi, et pourquoi) :** Étendre `.mcp_resolve_project` pour qu'un nom approximatif (accents, homophones, distance d'édition ≤ 2) donne le projet ou une seule liste de candidats, jamais un « introuvable » sec, pour que l'utilisateur ne redise pas le nom (P1, critère 1).

**Redite par l'agent :** _à compléter par l'agent avant d'agir — s'il n'y arrive pas, il s'arrête._

## Les trois questions (règle de staffing)

| Question | Réponse | Conséquence |
|---|---|---|
| Peut-il redire la mission en une phrase, avec son but ? | oui | |
| Sait-il comment faire ? (sources, conventions nommées ci-dessous) | oui | |
| A-t-il les moyens ? (outils, données d'essai, tests) | oui (après 005, 006) | |

**Temps de la main :** appuyer — fonction isolée, tests prêts (006).

## Ma réponse d'abord

_Rempli par Pascal seul, avant de lire le travail de l'agent._

## L'Homme

- **Agent :** `dev`
- **Modèle :** sonnet
- **Ce qui ne lui est pas demandé :** un choix silencieux parmi plusieurs candidats.

## Harnais

- **Peut :** modifier `R/service_mcp.R`, NEWS.
- **Ne doit pas :** sortir du périmètre, toucher à `main`, écrire dans `nemeton`, `nemetonmcp`, `aigora` ou `victor` (lecture seule), mettre un littéral FR/EN hors i18n, ajouter de la logique métier.
- **Sources autorisées :** spec P1 ; tests 006 ; fixture 005 ; `utils::adist`.

## Valeur provisoire (chiffre, réglage)

- seuil de distance 2 : provisoire (voir 006).

## Dépend de / rouge attendu jusqu'à

- 005, 006 ; ordre 005 → 006 → 007.

## Tests

- **Comment saura-t-on que c'est juste :** tests de 006 (noms) verts. Commande : `.claude/test-command` (`devtools::test()`).

## Terrain

- **Échantillon que Pascal vérifiera :** Pascal dit « Couchet » à VICTOR : Couchey est trouvé.

## Qui veille, qui tranche

- **Relecteur :** `relecteur`, puis Pascal.
- **Critère d'arrêt de la relecture :** 3 allers-retours ; au-delà, la décision remonte à Pascal.
- **Point de suivi :** à la remise de la PR (ou du compte rendu), et au plan d'abord si la main est tenue.
- **Qui tranche :** Pascal.

## Comment saura-t-on que ça marche ?

plus de « introuvable » sur un nom dicté.

# Mission 010 — Confirmation des écritures par jeton en deux temps

> Gabarit (fiches n° 9 et 15 de la méthode). Rempli par l'orchestrateur le 2026-11-09,
> **proposé à Pascal (porte n° 2)**. Une mission = une phrase = une branche = une PR.

## Mission

**En une phrase (quoi, et pourquoi) :** Faire que `appliquer_ugf`, `croiser_onf` (et toute écriture d'un projet existant) n'écrivent rien au premier appel, rendent un aperçu et un jeton à usage unique, et n'écrivent qu'au second appel portant ce jeton, avec un helper réutilisable par `nemetonmcp` pour ses propres écritures (plan d'actions), pour qu'aucune phrase mal transcrite ne modifie un projet (ADR 0002, P4).

**Redite par l'agent :** _à compléter par l'agent avant d'agir — s'il n'y arrive pas, il s'arrête._

## Les trois questions (règle de staffing)

| Question | Réponse | Conséquence |
|---|---|---|
| Peut-il redire la mission en une phrase, avec son but ? | oui | |
| Sait-il comment faire ? (sources, conventions nommées ci-dessous) | oui | |
| A-t-il les moyens ? (outils, données d'essai, tests) | oui (après 009) | |

**Temps de la main :** tenir — garde de sécurité qui engage le contrat avec les clients : plan d'abord (registre, portée du jeton, aperçu), validé avant code.

## Ma réponse d'abord

_Rempli par Pascal seul, avant de lire le travail de l'agent._

## L'Homme

- **Agent :** `dev`
- **Modèle :** opus
- **Ce qui ne lui est pas demandé :** le relais vocal (dépôt victor), les permissions AIGORA.

## Harnais

- **Peut :** modifier `R/service_mcp.R`, `CONTRAT.md` (helper ajouté au contrat 001), `R/utils_i18n.R`, NEWS (changement cassant pour les clients : à signaler).
- **Ne doit pas :** sortir du périmètre, toucher à `main`, écrire dans `nemeton`, `nemetonmcp`, `aigora` ou `victor` (lecture seule), mettre un littéral FR/EN hors i18n, ajouter de la logique métier.
- **Sources autorisées :** ADR 0002, spec P4 et critère 4, tests 009, `.mcp_garde_ecriture`, `nemetonmcp/r/R/plan.R` (lecture seule) pour le besoin du helper.

## Valeur provisoire (chiffre, réglage)

- jeton 10 min : provisoire (voir 009).

## Dépend de / rouge attendu jusqu'à

- 001, 003, 009 ; ordre 001 → 003 → 009 → 010.

## Tests

- **Comment saura-t-on que c'est juste :** tests de 009 verts ; tests existants de `appliquer_ugf`/`croiser_onf` adaptés et annoncés dans la PR (seule retouche permise, car le comportement change par décision). Commande : `.claude/test-command` (`devtools::test()`).

## Terrain

- **Échantillon que Pascal vérifiera :** Pascal dit « applique les UGF ONF » à VICTOR : il entend l'aperçu, confirme, et seulement alors le projet change.

## Qui veille, qui tranche

- **Relecteur :** `relecteur`, puis Pascal.
- **Critère d'arrêt de la relecture :** 3 allers-retours ; au-delà, la décision remonte à Pascal.
- **Point de suivi :** à la remise de la PR (ou du compte rendu), et au plan d'abord si la main est tenue.
- **Qui tranche :** Pascal.

## Comment saura-t-on que ça marche ?

un « applique » mal compris ne modifie rien.

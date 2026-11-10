# ADR 0004 — Outils longs : détacher comme le calcul

- **Date :** 2026-10-24
- **Statut :** acceptée le 2026-11-09 (option recommandée)
- **Proposée par :** Claude (orchestrateur)
- **Tranchée par :** Pascal

## La décision en une phrase

Tout outil MCP dont la durée mesurée peut dépasser 60 s (candidats : `generer_rapport`, `croiser_onf` au premier appel) est détaché sur le modèle de `lancer_calcul` / `etat_calcul` : il rend la main tout de suite, et un état se consulte ensuite.

## Contexte

Une tâche VICTOR est coupée à 600 s. `croiser_onf` prend « ~15 s par commune » selon nemetonshiny et « environ 35 s » selon AIGORA, avec un téléchargement DGFiP de 376 Mo au premier appel. `generer_rapport` produit un PDF Quarto en synchrone. Aucune durée n'a été mesurée sur un gros projet. Une tâche coupée oblige l'utilisateur à redemander : c'est exactement « se répéter ». Options :

- **A. Détacher au-delà de 60 s mesurés** (proposée) : réutilise le mécanisme de calcul (process enfant, journal, état JSON).
- **B. Tâches MCP natives** (`execution.taskSupport`) : standard récent, support mcptools inconnu.
- **C. Garder synchrone** et relever le plafond VICTOR : le brief VICTOR l'exclut.
- **D. Préchauffer** le téléchargement DGFiP (cache) : réduit `croiser_onf` sans détacher, complémentaire de A.

## Le crible FRANCE

| Lettre | Question | Réponse (une ligne) | Ce que ça coûte | Prêts à payer ? |
|---|---|---|---|---|
| **F**rugale | La plus simple qui tienne ? | mesurer d'abord ; ne détacher que ce qui dépasse | une mesure sur gros projet | oui (Pascal, 2026-11-09) |
| **R**esponsable | Qui fait quoi ? | nemetonshiny détache ; nemetonmcp/VICTOR suivent l'état | consignes clientes | oui (Pascal, 2026-11-09) |
| **A**nticiper | Et si l'imprévu arrive ? | process enfant mort : état `echec` + fin du log, comme le calcul | — | oui (Pascal, 2026-11-09) |
| **N**ovateur | Toutes les idées ? | A à D | — | — |
| **C**oût | Dépense inutile ? | ne rien détacher sans mesure | — | oui (Pascal, 2026-11-09) |
| **É**cologique | Qui réutilise ? | un seul mécanisme de tâche pour tous les outils longs | — | oui (Pascal, 2026-11-09) |

## Valeurs sacrifiées

**Simplicité d'usage** : un outil détaché demande un second geste (« où en est le rapport ? ») au lieu d'une réponse directe.

## Sources

`R/service_mcp.R:466-469,508-516,584` ; `aigora/aigora-nemeton/CLAUDE.md:88` ; `specs/BRIEF-pilotage-victor-aigora.md`, § 5.4 ; spécification MCP, `execution.taskSupport` (https://modelcontextprotocol.io/specification/2025-11-25/server/tools).

## Question de contrôle

Cette décision tient-elle nos six valeurs, et que sommes-nous prêts à payer pour cela ?

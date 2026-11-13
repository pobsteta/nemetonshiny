# ADR 0003 — Ouvrir le pilotage par IA à tous les utilisateurs : souveraineté des données

- **Date :** 2026-10-24
- **Statut :** proposée
- **Proposée par :** Claude (orchestrateur)
- **Tranchée par :** Pascal

## La décision en une phrase

Le pilotage par IA est ouvert à tous les utilisateurs par le connecteur distant de `nemetonmcp`, derrière Keycloak (ADR-005). On accepte que des données de projet soient lues par le fournisseur du client IA de l'utilisateur, et c'est dit à l'utilisateur ; nemetonshiny reste neutre vis-à-vis du fournisseur.

## Contexte

Arbitrage 3 : « pour tous les utilisateurs ». Aujourd'hui, le serveur est local (stdio) sur le poste de Pascal. Le connecteur distant existe (`nemetonmcp/r/R/distant.R` : JWT Keycloak, `127.0.0.1:8765`) mais n'est pas déployé : le dernier tunnel a renvoyé une 502, le 2026-10-12. L'ADR-004 privilégie Mistral par souveraineté ; or Claude (Anthropic) est le client d'AIGORA et de VICTOR. Le brief VICTOR déclarait les données de projet confidentielles. Options :

- **A. Distant, tout client MCP, avertissement à l'utilisateur** (proposée).
- **B. Distant, réservé à un client souverain** (Mistral Le Chat, s'il parle MCP, ou client auto-hébergé). Plus conforme à l'ADR-004 ; dépend d'un client qui reste à trouver.
- **C. Local seulement** : contredit l'arbitrage 3.

Cette ADR touche la plateforme (déploiement, ADR-003/004/005) : elle devra être reportée ou remplacée dans `platform_nemeton/docs/`.

## Le crible FRANCE

| Lettre | Question | Réponse (une ligne) | Ce que ça coûte | Prêts à payer ? |
|---|---|---|---|---|
| **F**rugale | La plus simple qui tienne ? | A réutilise distant.R et Keycloak existants | un déploiement OVH à faire | à trancher |
| **R**esponsable | Qui fait quoi ? | nemetonmcp sert ; plateforme déploie ; nemetonshiny garantit l'API et les gardes | trois dépôts | à trancher |
| **A**nticiper | Et si l'imprévu arrive ? | fuite de données de projet vers un tiers ; fournisseur qui change ses conditions | réputation, RGPD | à trancher |
| **N**ovateur | Toutes les idées ? | A, B, C ; B mérite une veille | — | — |
| **C**oût | Dépense inutile ? | hébergement et maintenance du distant | — | à trancher |
| **É**cologique | Qui réutilise ? | standard MCP : tout client compatible | — | à trancher |

## Valeurs sacrifiées

**Souveraineté** (ADR-004) et **confidentialité des projets**, au profit de l'accès pour tous par les interfaces IA du marché. Sacrifice à rendre visible à l'utilisateur, et à reconsidérer si une option B mûrit.

## Sources

Arbitrage 3 (`pilotage/brief.md`) ; `nemetonmcp/r/R/distant.R:99-128,405-424` ; `briefs/traites/2026-10-12-traite-reponse-connecteur-nemetonclaude-pour-victor.md:39-42` ; `specs/BRIEF-pilotage-victor-aigora.md:222-223,255` ; `CLAUDE.md`, ADR-003, ADR-004, ADR-005.

## Question de contrôle

Cette décision tient-elle nos six valeurs, et que sommes-nous prêts à payer pour cela ?

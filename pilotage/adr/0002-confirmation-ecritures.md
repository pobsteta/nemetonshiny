# ADR 0002 — Confirmation des écritures demandées par un agent

- **Date :** 2026-10-24
- **Statut :** acceptée le 2026-11-09 (option recommandée)
- **Proposée par :** Claude (orchestrateur)
- **Tranchée par :** Pascal

## La décision en une phrase

Tout outil MCP qui écrit fonctionne en **deux temps**. Le premier appel n'écrit rien : il rend un aperçu et un jeton à usage unique, valable 10 minutes. Seul un second appel portant ce jeton écrit. La garde vit dans le serveur (nemetonshiny), pas seulement dans le client.

## Contexte

Arbitrage 4 de Pascal : « il faut confirmer dès qu'on touche à la base de données postgres/postgis ». Aujourd'hui, VICTOR tourne en `bypassPermissions` et `aigora-nemeton/.claude/settings.json:16-37` autorise les 22 outils : rien n'arrête une phrase mal transcrite. Options :

- **A. Jeton en deux temps côté serveur** (proposée) : fonctionne quel que soit le client (stdio, distant, bypass), et se teste en R.
- **B. Elicitation MCP** : le serveur demande la confirmation au client. C'est la voie standard, mais elle dépend du support de mcptools et du client ; VICTOR devrait relayer à la voix. À vérifier par l'`acquereur`.
- **C. Permissions client** : retirer les outils qui écrivent de `permissions.allow`. Inopérant en `bypassPermissions`, donc insuffisant seul.
- **D. Annotations `destructive_hint` seules** : la spécification les déclare « untrusted ». Indicatives, insuffisantes seules.

Les annotations (D) sont posées dans tous les cas.

Périmètre (tranché par Pascal le 2026-11-09, spec Q1) : toute modification d'un projet existant, sur disque ou en base ; lecture, lancement de calcul et exports restent libres.

## Le crible FRANCE

| Lettre | Question | Réponse (une ligne) | Ce que ça coûte | Prêts à payer ? |
|---|---|---|---|---|
| **F**rugale | La plus simple qui tienne ? | A : un paramètre `jeton` et un registre en mémoire | un aller-retour de plus | oui (Pascal, 2026-11-09) |
| **R**esponsable | Qui fait quoi ? | serveur = garde ; client = relaie l'aperçu à l'humain | consignes VICTOR et AIGORA à écrire | oui (Pascal, 2026-11-09) |
| **A**nticiper | Et si l'imprévu arrive ? | jeton périmé ou rejoué refusé ; un agent peut enchaîner seul les deux appels | la garde suppose que l'agent relaie | oui (Pascal, 2026-11-09) |
| **N**ovateur | Toutes les idées ? | A à D, plus B en évolution si mcptools le supporte | — | — |
| **C**oût | Dépense inutile ? | pas de dépendance nouvelle | — | oui (Pascal, 2026-11-09) |
| **É**cologique | Qui réutilise ? | le même jeton sert l'app distante et tout futur client | — | oui (Pascal, 2026-11-09) |

## Valeurs sacrifiées

- **Fluidité** : chaque écriture demande deux tours de parole.
- **Garantie absolue** : un agent qui enchaîne lui-même les deux appels sans demander contourne la garde. Seule l'elicitation (B) met vraiment un humain dans la boucle. A est une barrière, pas une preuve.

## Sources

Arbitrage 4 (`pilotage/brief.md`) ; `aigora/aigora-nemeton/.claude/settings.json:16-37` ; `R/service_mcp.R:477-503` (purge de `croiser_onf`) ; spécification MCP, Tools, « human in the loop » et « annotations untrusted » (https://modelcontextprotocol.io/specification/2025-11-25/server/tools).

## Question de contrôle

Cette décision tient-elle nos six valeurs, et que sommes-nous prêts à payer pour cela ?

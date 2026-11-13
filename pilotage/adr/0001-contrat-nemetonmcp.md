# ADR 0001 — Contrat entre nemetonshiny et nemetonmcp

- **Date :** 2026-10-24
- **Statut :** acceptée le 2026-11-09 (option recommandée)
- **Proposée par :** Claude (orchestrateur)
- **Tranchée par :** Pascal

## La décision en une phrase

Les 31 objets internes que `nemetonmcp` lit par `getFromNamespace` deviennent un **contrat déclaré** dans `CONTRAT.md` (section « API pour nemetonmcp »), verrouillé par un test de présence et de signature, sans être exportés.

## Contexte

`nemetonmcp` (paquet R `nemetonclaude`) appelle 41 fois `utils::getFromNamespace(nom, "nemetonshiny")` (`nemetonmcp/r/R/utils.R:40`). `CONTRAT.md` ne garantit que les 9 exports (`NAMESPACE:2-10`). Un renommage casse VICTOR sans que la CI d'ici le voie. Options :

- **A. Exporter** les 31 objets. Le contrat est visible par R et la documentation roxygen devient obligatoire. Mais l'export public grossit (de 9 à 40 objets), et chaque export exige un test (règle stricte 5) ; toute rupture devient un changement cassant de l'API publique.
- **B. Déclarer sans exporter** (proposée) : section de `CONTRAT.md` et test qui vérifie présence et `formals()`. Le contrat est visible, l'API publique ne change pas.
- **C. `nemetonmcp` s'en défait** : il ne passe plus que par les 9 exports. Il faudrait étendre `api.R` (plan d'actions, communes), donc des exports nouveaux, au cas par cas.
- **D. Fusionner** le serveur MCP dans nemetonshiny. Rejetée : Pascal a désigné `nemetonmcp` comme porteur (arbitrage 1).

## Le crible FRANCE

| Lettre | Question | Réponse (une ligne) | Ce que ça coûte | Prêts à payer ? |
|---|---|---|---|---|
| **F**rugale | La plus simple qui tienne ? | B : une section de doc et un test, aucun code déplacé | des noms internes figés | oui (Pascal, 2026-11-09) |
| **R**esponsable | Qui fait quoi ? | nemetonshiny garantit la liste, nemetonmcp n'appelle rien d'autre | un brief vers nemetonmcp | oui (Pascal, 2026-11-09) |
| **A**nticiper | Et si l'imprévu arrive ? | le test casse ici avant la prod ; un objet hors liste reste non garanti | discipline des deux côtés | oui (Pascal, 2026-11-09) |
| **N**ovateur | Toutes les idées ? | A, B, C, D examinées | — | — |
| **C**oût | Dépense inutile ? | B évite 31 pages roxygen ; C coûte le plus cher | — | oui (Pascal, 2026-11-09) |
| **É**cologique | Qui réutilise ? | B laisse l'API publique sobre ; A la rendrait réutilisable par d'autres clients | moins de réutilisation externe | oui (Pascal, 2026-11-09) |

## Valeurs sacrifiées

- **B** sacrifie la propreté d'un contrat uniquement par exports : on garantit des fonctions qui restent « internes » au sens R, et un appel `:::` reste toléré.
- **A** sacrifierait la sobriété de l'API publique.

## Sources

`nemetonmcp/r/R/utils.R:40` ; inventaire `grep -oh '\.ns("[^"]*")' nemetonmcp/r/R/*.R` (31 objets distincts) ; `NAMESPACE:2-10` ; `CONTRAT.md:27-74` ; `CLAUDE.md`, règle stricte 5.

## Question de contrôle

Cette décision tient-elle nos six valeurs, et que sommes-nous prêts à payer pour cela ?

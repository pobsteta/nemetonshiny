# Mission 003 — Erreurs MCP marquées, classées, bilingues

> Gabarit (fiches n° 9 et 15 de la méthode). Rempli par l'orchestrateur le 2026-11-09,
> **proposé à Pascal (porte n° 2)**. Une mission = une phrase = une branche = une PR.

## Mission

**En une phrase (quoi, et pourquoi) :** Faire rendre par `.mcp_call` un `ellmer::ContentToolResult` en erreur (donc `isError: true` côté mcptools) portant la classe, le message FR/EN et l'action suivante, pour qu'un agent reconnaisse une erreur comme telle et sache quoi proposer sans faire répéter l'utilisateur (critère 3, P6).

**Redite par l'agent :** _à compléter par l'agent avant d'agir — s'il n'y arrive pas, il s'arrête._

## Les trois questions (règle de staffing)

| Question | Réponse | Conséquence |
|---|---|---|
| Peut-il redire la mission en une phrase, avec son but ? | oui | |
| Sait-il comment faire ? (sources, conventions nommées ci-dessous) | oui | |
| A-t-il les moyens ? (outils, données d'essai, tests) | oui | |

**Temps de la main :** tenir — la forme de l'erreur est consommée par trois clients (nemetonmcp, AIGORA, VICTOR) : l'agent rend un plan d'abord (forme JSON exacte, clés i18n).

## Ma réponse d'abord

_Rempli par Pascal seul, avant de lire le travail de l'agent._

## L'Homme

- **Agent :** `dev`
- **Modèle :** sonnet
- **Ce qui ne lui est pas demandé :** les annotations (004), le jeton (010).

## Harnais

- **Peut :** modifier `R/service_mcp.R`, `R/api.R` (classes d'erreur), `R/utils_i18n.R` (clés NMT ≤ 30 car., `\uXXXX`), NEWS.
- **Ne doit pas :** sortir du périmètre, toucher à `main`, écrire dans `nemeton`, `nemetonmcp`, `aigora` ou `victor` (lecture seule), mettre un littéral FR/EN hors i18n, ajouter de la logique métier.
- **Sources autorisées :** `pilotage/spec.md` P6, critère 3 ; tests de la mission 002 ; `CLAUDE.md` (i18n, règles strictes) ; mcptools `as_tool_call_result`.

## Valeur provisoire (chiffre, réglage)

- aucune.

## Dépend de / rouge attendu jusqu'à

- 002 (tests) ; ordre de fusion 002 → 003.

## Tests

- **Comment saura-t-on que c'est juste :** les tests de 002 (erreurs) passent ; tests existants verts sans retouche (critère 8). Commande : `.claude/test-command` (`devtools::test()`).

## Terrain

- **Échantillon que Pascal vérifiera :** Pascal dit « où en est Doubs-qui-n'existe-pas » dans AIGORA : l'agent annonce une erreur et propose des candidats.

## Qui veille, qui tranche

- **Relecteur :** `relecteur`, puis Pascal.
- **Critère d'arrêt de la relecture :** 3 allers-retours ; au-delà, la décision remonte à Pascal.
- **Point de suivi :** à la remise de la PR (ou du compte rendu), et au plan d'abord si la main est tenue.
- **Qui tranche :** Pascal.

## Comment saura-t-on que ça marche ?

dans AIGORA, une erreur n'est plus lue comme un succès.

## Plan accepté et moyens — 2026-11-10

- Plan rendu par le `dev` (main tenue) et accepté par Pascal avec ses arbitrages (journal du 2026-11-10) : `classe` = nom R complet + `code` court ; forme JSON `{ok, classe, code, langue, erreur, action, candidats}` dans `ellmer::ContentToolResult(error=…)` ; langue FR par défaut, passée seulement par `resume_projet`/`generer_rapport` ; `confirmation_requise`/`jeton_invalide` reportées à 010 ; actions : introuvable → `lister_projets`, calcul_en_cours → `etat_calcul`, verrouillé → attendre ou prévenir le détenteur nommé.
- **Premier commit de la branche** : `git checkout 37cbbf92 -- tests/testthat/test-service_mcp_erreurs.R` (tests d'acceptation de 002, à ne pas retoucher). Ils doivent passer au vert sans retouche.
- `candidats` contient le **nom affiché** des projets (le test l'exige : « Dabo nord », « Dabo sud »), pas seulement leur identifiant.
- Le `.lit` de `test-service_mcp.R` peut casser (critère 8) : remonter plutôt que modifier ces tests.

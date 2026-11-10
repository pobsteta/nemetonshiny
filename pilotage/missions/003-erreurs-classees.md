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

## Remarques de relecture — aller-retour 1/3 (2026-11-10)

Point de départ : branche `origin/claude/mission-003-erreurs-classees` (commit `cf587187`) — `git fetch origin claude/mission-003-erreurs-classees && git reset --hard origin/claude/mission-003-erreurs-classees`.

Arbitrages de Pascal (2026-11-10) :
- **Mécanisme ratifié** : marquage dans `mcp_tools()` par `.mcp_marquer()` ; les `mcp_*` gardent leur chaîne JSON (attribut `mcp_erreur`) ; `test-service_mcp.R` intact.
- **nemetonmcp** court-circuite le marquage pour `etat_calcul` et `exporter_gpkg` : brief sortant (mission 013), et `.mcp_vers_outil` (ou l'objet que nemetonmcp devra appeler pour marquer ses remplaçants) entre au contrat de 001.
- **`_problems/`** : à nettoyer ici.
- Le détail technique en FR dans une réponse EN est **accepté pour l'instant** (dette notée).

À corriger :
1. Retirer les 11 fichiers `tests/testthat/_problems/*.R` ajoutés par `cf587187` ; supprimer aussi les 3 déjà présents sur `main` (`test-mod_family-450/453`, `test-service_synthesis-46`) ; ajouter `^tests/testthat/_problems$` à `.Rbuildignore` et `tests/testthat/_problems/` à `.gitignore`.
2. Mettre à jour `inst/mcp/README.md` (l. 33-34) : nouvelle forme d'erreur (`isError`, `{ok, classe, code, langue, erreur, action, candidats}`), langue FR par défaut, actions.
3. **Contrat** : la PR #253 (contrat, `CONTRAT.md` § 6 bis + `test-contrat_nemetonmcp.R`) n'est pas encore fusionnée. **Si elle l'est quand tu commences** (`git log origin/main --oneline | grep -i contrat`), rebase sur `origin/main` et ajoute `.mcp_vers_outil` (avec sa signature) à la § 6 bis et à la table du test. **Sinon**, ne touche pas au contrat et dis-le au rendu : l'orchestrateur le demandera après la fusion de #253.
4. Ajouter à NEWS la ligne de la dette : détail technique en FR dans les erreurs EN.

## Complément — après fusion de #253 (2026-11-10)

#253 (contrat) est fusionnée sur `main` (46eee94b), #254 aussi (54d65ec4). PR de cette mission : #256 (brouillon).

Point de départ : `origin/claude/mission-003-erreurs-classees` (210fbbbb). **Fusionner `origin/main` dans la branche** (`git merge origin/main`, pas de rebase : le hook refuse le push forcé), puis :
1. Ajouter `.mcp_vers_outil` (et tout autre objet que nemetonmcp devra appeler pour marquer ses remplaçants, avec signature) à `CONTRAT.md` § 6 bis et à la table de `tests/testthat/test-contrat_nemetonmcp.R` ; le test contrat doit passer.
2. `inst/mcp/README.md` : préciser que `candidats` n'est rempli que pour `projet_ambigu`, au format « nom (id) », et absent sinon ; que `classe` vaut `nemetonshiny_erreur`/`nemetonshiny_erreur_inconnue` pour une erreur non répertoriée ; et une phrase sur la limite actuelle via nemetonmcp (`etat_calcul`, `exporter_gpkg` pas encore marqués, brief en cours).
3. `devtools::test(filter = "mcp|contrat")` vert ; `git diff 37cbbf92 -- tests/testthat/test-service_mcp_erreurs.R` vide.

## Dernier point — 2026-11-10 (Pascal)

Point de départ : `origin/claude/mission-003-erreurs-classees` (7ff648aa) ; pousser en fast-forward.
- Rétablir l'**égalité stricte** à la fin de `tests/testthat/test-contrat_nemetonmcp.R`, avec une **exception nommée et datée** pour `.mcp_vers_outil` :
  ```r
  # Exception 2026-11-10 : .mcp_vers_outil est au contrat avant d'être appelé
  # par nemetonmcp (brief mission 013). Retirer l'exception à sa livraison.
  expect_setequal(appelles, setdiff(<noms du contrat>, ".mcp_vers_outil"))
  ```
- `devtools::test(filter = "mcp|contrat")` vert (le test contrat tourne sur ce poste : nemetonmcp local présent).

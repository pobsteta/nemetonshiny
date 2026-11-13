# Mission 002 — Tests d'acceptation : erreurs classées et annotations

> Gabarit (fiches n° 9 et 15 de la méthode). Rempli par l'orchestrateur le 2026-11-09,
> **proposé à Pascal (porte n° 2)**. Une mission = une phrase = une branche = une PR.

## Mission

**En une phrase (quoi, et pourquoi) :** Écrire, sans lire l'implémentation à venir, les tests d'acceptation des critères 3 (chaque outil mis en échec rend une erreur MCP `isError`, classée, FR/EN, avec action proposée) et 5 (annotations `read_only_hint`/`destructive_hint`/`idempotent_hint` justes dans `tools/list`), pour que les missions 003 et 004 ne s'auto-évaluent pas.

**Redite par l'agent :** non rédigée (fiche close au bilan du 2026-11-13)

## Les trois questions (règle de staffing)

| Question | Réponse | Conséquence |
|---|---|---|
| Peut-il redire la mission en une phrase, avec son but ? | oui | |
| Sait-il comment faire ? (sources, conventions nommées ci-dessous) | oui | |
| A-t-il les moyens ? (outils, données d'essai, tests) | oui | |

**Temps de la main :** appuyer — la forme attendue est fixée par la spec et par le comportement mesuré de mcptools 1.0.3.

## Ma réponse d'abord

_Rempli par Pascal seul, avant de lire le travail de l'agent._

## L'Homme

- **Agent :** `testeur`
- **Modèle :** sonnet
- **Ce qui ne lui est pas demandé :** implémenter quoi que ce soit dans `R/`.

## Harnais

- **Peut :** écrire `tests/testthat/test-mcp_erreurs.R` et `test-mcp_annotations.R`, marqués `skip()` tant que 003/004 ne sont pas fusionnées, ou rouges annoncés.
- **Ne doit pas :** sortir du périmètre, toucher à `main`, écrire dans `nemeton`, `nemetonmcp`, `aigora` ou `victor` (lecture seule), mettre un littéral FR/EN hors i18n, ajouter de la logique métier.
- **Sources autorisées :** `pilotage/spec.md` § 1 (P6) et § 5 (critères 3, 5), journal du 2026-11-09 (comportement mcptools : `isError` seulement via `ellmer::ContentToolResult(error=)`, annotations transmises par `tool_annotations_as_json`), `R/service_mcp.R` pour les noms d'outils et de classes existants seulement.

## Valeur provisoire (chiffre, réglage)

- aucune.

## Dépend de / rouge attendu jusqu'à

- aucune ; précède 003 et 004.

## Tests

- **Comment saura-t-on que c'est juste :** un test par classe d'erreur (`projet_introuvable`, `projet_ambigu`, `projet_verrouille`, `calcul_en_cours`, `confirmation_requise`, `jeton_invalide`) et un test par outil pour les annotations. Commande : `.claude/test-command` (`devtools::test()`).

## Terrain

- **Échantillon que Pascal vérifiera :** —

## Qui veille, qui tranche

- **Relecteur :** `relecteur`, puis Pascal.
- **Critère d'arrêt de la relecture :** 3 allers-retours ; au-delà, la décision remonte à Pascal.
- **Point de suivi :** à la remise de la PR (ou du compte rendu), et au plan d'abord si la main est tenue.
- **Qui tranche :** Pascal.

## Comment saura-t-on que ça marche ?

les tests sont rouges sur `main` et verts après 003/004, sans retouche.

## Remarques de relecture — aller-retour 1/3 (2026-11-10)

Point de départ : les deux fichiers rendus dans `/home/pascal/dev/nemetonshiny/.claude/worktrees/agent-ab9b942ef4ecc40d7/tests/testthat/` (`test-mcp_erreurs.R`, `test-mcp_annotations.R`), à recopier dans ton worktree, puis à corriger.

Arbitrages de Pascal (2026-11-10) :
- Forme de l'erreur (plan 003) : JSON dans `ellmer::ContentToolResult(error=…)`, texte client `"Tool calling failed with error <json>"` ; champs `classe` = nom R complet (`nemetonshiny_projet_introuvable`) ET `code` court (`projet_introuvable`) ; langue FR par défaut, passée seulement par `resume_projet`/`generer_rapport` ; `confirmation_requise`/`jeton_invalide` restent en `skip` (mission 010).
- Annotations, à asserter exactement pour les 10 outils :
  - `read_only_hint` TRUE : `lister_projets`, `resume_projet`, `etat_calcul`, `url_app` ; FALSE pour les 6 autres, y compris `generer_rapport` et `exporter_gpkg`.
  - `destructive_hint` TRUE : `appliquer_ugf`, `croiser_onf` seulement ; FALSE pour `lancer_calcul`, `annuler_calcul`, `generer_rapport`, `exporter_gpkg`.
  - `idempotent_hint` TRUE : les 4 outils de lecture + `appliquer_ugf`, `croiser_onf`, `generer_rapport`, `exporter_gpkg`, `annuler_calcul` ; FALSE : `lancer_calcul`.
- Actions proposées, à asserter : `projet_introuvable` → `lister_projets` ; `calcul_en_cours` → `etat_calcul` ; `projet_verrouille` → attendre la fin de l'édition ou prévenir le détenteur nommé (asserter le nom du détenteur ET la présence d'un champ action non vide).

À corriger (relecteur) :
1. La classification est verte aujourd'hui par sous-chaîne (`"projet_introuvable"` ⊂ `"nemetonshiny_projet_introuvable"`). Retirer le préfixe `"Tool calling failed with error "`, parser le reste par `jsonlite::fromJSON`, puis `expect_identical(j$code, …)` et `expect_identical(j$classe, …)` pour chaque classe. Le test doit être rouge aujourd'hui pour cette raison-là aussi.
2. Asserter `idempotent_hint` et `destructive_hint` des 6 outils qui écrivent, selon les arbitrages ci-dessus.
3. Ajouter l'action pour `projet_verrouille`.
4. FR/EN : au lieu de `!identical(fr, en)`, vérifier que chaque texte contient le message de sa langue (comparer à `get_i18n("fr"/"en")$t(<clé>)` n'est pas possible tant que les clés n'existent pas : à défaut, vérifier `j$langue` et que le message EN ne contient pas le message FR).
5. Mineurs : renommer en `test-service_mcp_erreurs.R` / `test-service_mcp_annotations.R` (convention CLAUDE.md, et le filtre `mcp` du test-command les attrape toujours) ; retirer le champ `brut` inutilisé de `.er_appel`.

## Remarques de relecture — aller-retour 2/3 (2026-11-10)

Point de départ : commit `6e5ca546` (branche `worktree-agent-a1b17ee4e89d86489`) — le reprendre dans ta branche par `git cherry-pick 6e5ca546`.

Un seul point bloquant :
1. La forme JSON est **fixée** par le plan 003 accepté par Pascal : `{ok, classe, code, langue, erreur, action, candidats}`. Le message s'appelle `erreur`, pas `message`. Dans `test-service_mcp_erreurs.R`, l. 187-195, remplacer `$message` par `$erreur` partout ; corriger l'en-tête (l. 14-16) pour citer la forme fixée au lieu d'une hypothèse.

Facultatif : vérifier les candidats de `projet_ambigu` dans le champ `candidats` (plus seulement dans le texte brut).

## Statut et fusion — décision de Pascal (2026-11-10)

- Relu **prêt** à l'aller-retour 2/3. Livrable de référence : commit `37cbbf92` (branche `worktree-agent-a823d18c1a278977d`), fichiers `tests/testthat/test-service_mcp_erreurs.R` et `test-service_mcp_annotations.R` (la rubrique Harnais disait `test-mcp_*.R` : renommés à l'aller-retour 1/3, convention CLAUDE.md).
- **Rouge attendu jusqu'à** 003 (fichier erreurs) et 004 (fichier annotations) — corrige la rubrique « Dépend de / rouge attendu » qui disait « aucune ».
- **Pas de PR 002 seule.** Chaque fichier entre en tête de la branche de sa mission : `git checkout 37cbbf92 -- tests/testthat/test-service_mcp_erreurs.R` en premier commit de 003, `… test-service_mcp_annotations.R` en premier commit de 004. Le relecteur de 003/004 vérifie `git diff 37cbbf92 -- <fichier>` vide.
- **Statut** (bilan du 2026-11-13) : tests intégrés à 003/004 (pas de PR propre), 2026-11-10.

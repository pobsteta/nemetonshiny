# Mission 009 — Tests d'acceptation : confirmation par jeton

> Gabarit (fiches n° 9 et 15 de la méthode). Rempli par l'orchestrateur le 2026-11-09,
> **proposé à Pascal (porte n° 2)**. Une mission = une phrase = une branche = une PR.

## Mission

**En une phrase (quoi, et pourquoi) :** Écrire, sans lire l'implémentation, les tests du critère 4 : chaque outil qui modifie un projet existant, appelé sans jeton, n'écrit rien (disque et base de test) et rend l'aperçu ; avec un jeton valide il écrit ; un jeton périmé, rejoué ou d'un autre projet est refusé.

**Redite par l'agent :** non rédigée (fiche close au bilan du 2026-11-13)

## Les trois questions (règle de staffing)

| Question | Réponse | Conséquence |
|---|---|---|
| Peut-il redire la mission en une phrase, avec son but ? | oui | |
| Sait-il comment faire ? (sources, conventions nommées ci-dessous) | oui | |
| A-t-il les moyens ? (outils, données d'essai, tests) | oui | |

**Temps de la main :** appuyer — critère précis, ADR 0002 tranchée.

## Ma réponse d'abord

_Rempli par Pascal seul, avant de lire le travail de l'agent._

## L'Homme

- **Agent :** `testeur`
- **Modèle :** sonnet
- **Ce qui ne lui est pas demandé :** implémenter.

## Harnais

- **Peut :** écrire `tests/testthat/test-mcp_confirmation.R` ; base de test via `NEMETON_DB_URL_TEST` si présente, sinon `skip`.
- **Ne doit pas :** sortir du périmètre, toucher à `main`, écrire dans `nemeton`, `nemetonmcp`, `aigora` ou `victor` (lecture seule), mettre un littéral FR/EN hors i18n, ajouter de la logique métier.
- **Sources autorisées :** ADR 0002 (périmètre : toute modification d'un projet existant ; lecture, calcul, exports libres), spec P4, critère 4.

## Valeur provisoire (chiffre, réglage)

- durée du jeton 10 min : provisoire (ADR 0002), à confirmer par Pascal à l'usage vocal, gel avant la release.

## Dépend de / rouge attendu jusqu'à

- 003 (classes `confirmation_requise`, `jeton_invalide`) ; précède 010.

## Tests

- **Comment saura-t-on que c'est juste :** tests rouges jusqu'à 010. Commande : `.claude/test-command` (`devtools::test()`).

## Terrain

- **Échantillon que Pascal vérifiera :** —

## Qui veille, qui tranche

- **Relecteur :** `relecteur`, puis Pascal.
- **Critère d'arrêt de la relecture :** 3 allers-retours ; au-delà, la décision remonte à Pascal.
- **Point de suivi :** à la remise de la PR (ou du compte rendu), et au plan d'abord si la main est tenue.
- **Qui tranche :** Pascal.

## Comment saura-t-on que ça marche ?

—

## Remarques de relecture — aller-retour 1/3 (2026-11-10)

Point de départ : branche `origin/claude/mission-009-tests-confirmation` (commit `a04bebf0`) — `git fetch origin claude/mission-009-tests-confirmation && git reset --hard origin/claude/mission-009-tests-confirmation` ; pousser en fast-forward (le hook refuse le push forcé).

Arbitrages de Pascal (2026-11-10) :
- **Premier appel** = erreur `confirmation_requise` (`isError`), forme commune `{ok, classe, code, langue, erreur, action}` **plus** `jeton`, `apercu`, `expire_a`. N'accepter plus que cette forme.
- **Nom** : `jeton` (paramètre et champ) — recommandation du relecteur retenue par l'orchestrateur, conforme à l'ADR 0002.
- **Lien** : un jeton n'écrit que pour le **même outil, le même projet et les mêmes paramètres** (fichier + son md5, `remplacer`, `purger` résolu) ; sinon `jeton_invalide`.
- **Périmètre** : `appliquer_ugf` et `croiser_onf` quelle que soit la valeur de `purger` (TRUE, FALSE, NULL) ; `annuler_calcul`, `lancer_calcul`, lectures et exports restent **libres**.
- **Test base** : le **retirer**, avec une note en tête de fichier (aucun outil MCP n'écrit en base aujourd'hui ; à réécrire le jour où l'un le fera).
- **Ordre des erreurs** (recommandation du relecteur, retenue par l'orchestrateur — Pascal peut la renverser) : les erreurs métier et les gardes (`fichier_invalide`, `idu_inconnu`, `pavage_invalide`, `onf_echec`, `calcul_en_cours`, `projet_verrouille`) passent **avant** l'aperçu ; les gardes sont rejouées à la confirmation.
- **Langue** : arbitrage de 003 — seuls `resume_projet` et `generer_rapport` acceptent `langue` ; les erreurs de jeton sont en FR par défaut.

À corriger :
1. Test 13 : retirer `langue =` sur `appliquer_ugf` ; vérifier seulement `langue == "fr"` par défaut.
2. Retirer le test base PostGIS (note en tête).
3. Couvrir `croiser_onf` avec `purger = FALSE` et `NULL` (rien n'est écrit sans jeton ; écriture avec jeton).
4. Resserrer le premier appel à la seule forme `confirmation_requise`, avec `jeton`, `apercu`, `expire_a` présents ; vérifier l'aperçu de P4 : pour `appliquer_ugf`, le nombre d'UGF remplacées ; pour `croiser_onf`, le nombre de parcelles écartées et leurs IDU (assertion qui ne soit pas déjà verte aujourd'hui).
5. Lien jeton ↔ outil/paramètres : jeton d'`appliquer_ugf` rejoué sur `croiser_onf` ; `purger` changé ; `remplacer` changé ; fichier changé (même nom, autre contenu) → `jeton_invalide`, rien d'écrit.
6. Ordre : fichier absent au premier appel → `fichier_invalide` (pas d'aperçu, pas de jeton).
7. Test 14 (libres) : ajouter `annuler_calcul` et `generer_rapport`.
8. Renommer en `test-service_mcp_confirmation.R`.
9. Note pour 010 (pas à toi de la corriger) : les deux tests `skip` de `test-service_mcp_erreurs.R` (002, l. ~213) supposent `confirmation_requise` sur un fichier absent, ce qui contredit le point 6 ; 010 les activera et devra corriger ce cas — l'orchestrateur le consigne.

## Remarques de relecture — aller-retour 2/3 (2026-11-10)

Point de départ : `origin/claude/mission-009-tests-confirmation` (`9ab2931c`) ; pousser en fast-forward.

Arbitrages de Pascal (2026-11-10) : clés de l'aperçu fixées (`appliquer_ugf` : `n_ugf_remplacees`, `n_ugf_nouvelles` ; `croiser_onf` : `ecartees` liste d'IDU, `n_ecartees`, `n_retenues`) ; tout jeton connu présenté est **consommé**, même refusé ; durée 10 min confirmée provisoirement.

À corriger :
1. Aperçu : asserter les clés exactes ci-dessus avec leurs valeurs (`expect_identical`), plus de recherche dans l'aplati.
2. `purger = NULL` avec jeton : fixer le réglage `purger` du projet dans la fixture et vérifier les IDU restants.
3. Consommation : après un refus pour lien (autre outil, `purger`, `remplacer`, fichier) et après une garde rejouée (verrou, calcul en cours), le même jeton présenté avec les bons paramètres est refusé (`jeton_invalide`), rien d'écrit.
4. « purger résolu » : jeton obtenu avec `purger = NULL` puis présenté avec `purger = TRUE` explicite (le réglage du projet valant TRUE) — accepté ; réglage du projet modifié entre les deux appels — refusé.
5. Une seule constante pour la durée (`.cf_duree <- 10 * 60`) dans le fichier de test.
6. Tests 7 (`remplacer = FALSE`) et 13 (rejeu sur `croiser_onf`) : vérifier l'état du disque.
7. Dire au rendu le rouge réel sur main ET sur 003 (`origin/claude/mission-003-erreurs-classees`) : 6 garde-fous verts sur 003.

## Remarques de relecture — aller-retour 3/3 (2026-11-10, dernier)

Point de départ : `origin/claude/mission-009-tests-confirmation` (`4106d30c`) ; pousser en fast-forward. Ne corriger QUE ce qui suit.

Arbitrages de Pascal (2026-11-10) :
- `croiser_onf` avec `purger` résolu FALSE : l'aperçu dit ce qui va changer — `ecartees` vide, `n_ecartees` = 0, `n_retenues` = toutes les parcelles ; règle `n_ecartees + n_retenues` = nombre de parcelles du projet.
- Le jeton est lié à **tous les réglages ONF résolus** du projet (`purger`, `seuil_couverture`, `domanialite`, `clip_cadastre`, `tol`, `larg_hors`, `seuil`, `seuil_hors`) : un réglage changé entre aperçu et confirmation → `jeton_invalide`, jeton consommé.
- Consommation après refus « autre projet » : gardée.

À corriger :
1. L. 513-514 : ne plus réécrire le GeoPackage pour le « restituer » (un GPKG réécrit change de md5) ; sauvegarder les octets juste après l'obtention du jeton (`file.copy(f, orig)`) et les remettre (`file.copy(orig, f, overwrite = TRUE)`).
2. L. 255 : `n_retenues`/`ecartees`/`n_ecartees` conditionnels au `purger` résolu selon l'arbitrage ci-dessus, avec la règle de somme.
3. Ajouter un test : `seuil_couverture` (ou `domanialite`) modifié par `set_project_onf_params` entre l'aperçu et la confirmation → `jeton_invalide`, rien d'écrit, jeton consommé.

## Statut et fusion — 2026-11-10

- Relu **prêt** à l'aller-retour 3/3. Livrable de référence : commit `93291ec8` (`origin/claude/mission-009-tests-confirmation`), `tests/testthat/test-service_mcp_confirmation.R` (36 tests). Rouge réel : main 35/36, 003 30/36 (6 garde-fous verts) ; 36/36 face à un prototype 010 conforme.
- Fusion : règle de 002 — pas de PR seule ; le fichier entre en tête de la branche 010 (`git checkout 93291ec8 -- tests/testthat/test-service_mcp_confirmation.R`), contrôle `git diff 93291ec8` vide par le relecteur.
- **Statut** (bilan du 2026-11-13) : tests 93291ec8 intégrés à 010 (pas de PR propre), 2026-11-10.

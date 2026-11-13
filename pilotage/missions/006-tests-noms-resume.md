# Mission 006 — Tests d'acceptation : noms dictés et résumé complet

> Gabarit (fiches n° 9 et 15 de la méthode). Rempli par l'orchestrateur le 2026-11-09,
> **proposé à Pascal (porte n° 2)**. Une mission = une phrase = une branche = une PR.

## Mission

**En une phrase (quoi, et pourquoi) :** Écrire, sans lire l'implémentation, les tests des critères 1 (la fixture 005 donne le bon projet ou une liste de candidats à 100 %, aucun « introuvable » à distance ≤ 2) et 2 (`resume_projet` rend tout ce qu'affiche la Synthèse, plus les heures ISO 8601 avec fuseau).

**Redite par l'agent :** non rédigée (fiche close au bilan du 2026-11-13)

## Les trois questions (règle de staffing)

| Question | Réponse | Conséquence |
|---|---|---|
| Peut-il redire la mission en une phrase, avec son but ? | oui | |
| Sait-il comment faire ? (sources, conventions nommées ci-dessous) | oui | |
| A-t-il les moyens ? (outils, données d'essai, tests) | oui (après 005) | |

**Temps de la main :** appuyer — critères chiffrés dans la spec.

## Ma réponse d'abord

_Rempli par Pascal seul, avant de lire le travail de l'agent._

## L'Homme

- **Agent :** `testeur`
- **Modèle :** sonnet
- **Ce qui ne lui est pas demandé :** implémenter.

## Harnais

- **Peut :** écrire `tests/testthat/test-mcp_noms_dictes.R` et `test-mcp_resume.R` sur projet fixture temporaire (`withr::with_tempdir`).
- **Ne doit pas :** sortir du périmètre, toucher à `main`, écrire dans `nemeton`, `nemetonmcp`, `aigora` ou `victor` (lecture seule), mettre un littéral FR/EN hors i18n, ajouter de la logique métier.
- **Sources autorisées :** spec P1, critères 1-2 ; fixture 005 ; `R/mod_synthesis.R` pour savoir ce qu'affiche la Synthèse.

## Valeur provisoire (chiffre, réglage)

- distance ≤ 2 : provisoire (spec), mesurée sur la fixture 005, gel avant fusion de 007.

## Dépend de / rouge attendu jusqu'à

- 005 ; précède 007 et 008.

## Tests

- **Comment saura-t-on que c'est juste :** tests rouges sur `main`, verts après 007/008. Commande : `.claude/test-command` (`devtools::test()`).

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

Point de départ : branche `origin/claude/mission-006-tests-noms-resume` (commit `e635111a`) — `git fetch origin claude/mission-006-tests-noms-resume && git reset --hard origin/claude/mission-006-tests-noms-resume` dans ton worktree.

Arbitrages de Pascal (2026-11-10) :
- **Mot-clé** tiré du **nom** du projet : « Forêt communale de X (ONF) » → X ; sinon le nom lui-même (Dabo, Reconfort…).
- **Rapprochement** : distance ≤ 2 au mot-clé **ou** le dicté est un mot entier / un début du mot-clé (« Marsannay » et « Forêt communale de Marsannay » trouvent le projet Marsannay).
- **Deux projets proches** → erreur `projet_ambigu` avec les deux en candidats.
- **`calcule_le`** = vraie fin de calcul : `completed_at` de `progress_state.json` ou `fin_a` de `compute_job.json` quand présents, repli sur la date de `data/indicators.parquet` sinon ; NA si non calculé.
- **Heures** (`created_at`, `updated_at`, `calcule_le`) en ISO 8601 avec fuseau **Europe/Paris fixé** (même résultat sur un serveur en UTC).
- **Noms des champs** validés tels quels (`description`, `created_at`, `updated_at`, `calcule_le`, `chm_suspect`, `chm_suspect_max`, `familles$n_indicateurs`). `updated_at` existe déjà : corriger l'en-tête qui le dit nouveau.

À corriger (relecteur) :
1. **Candidats** : la forme livrée par 003 est « nom (id) » (`R/service_mcp.R`, `.mcp_resolve_project`). Chercher le nom ou l'id **à l'intérieur** de chaque candidat (sous-chaîne `fixed = TRUE`), plus de comparaison à l'identique.
2. **Test « deux projets proches »** : utiliser deux projets dont les mots-clés sont tous deux à ≤ 2 du dicté selon la règle ci-dessus (ex. « Forêt communale de Couchey (ONF) » et « Forêt communale de Couchey-le-Haut » si « couchey » en est un mot entier, ou un autre couple cohérent) ; le test ne doit pas contredire la règle écrite dans le même fichier.
3. **`n_indicateurs`** : vérifier les **12 familles**, dont au moins une famille sans indicateur calculé (la Synthèse remplace alors le compte par le nombre prévu, `present_count` dans `R/mod_synthesis.R` ~l. 560) ; comparer les scores à la valeur de la Synthèse arrondie à 2 décimales, pas avec une tolérance de 0,05.
4. **Renommer** en `test-service_mcp_noms_dictes.R` et `test-service_mcp_resume.R`.
5. **L. 116** : `expect_type(r$created_at, "character")` avant `expect_match`, pour un rouge lisible.
6. **Normalisation** : remplacer `iconv(..., "ASCII//TRANSLIT")` (dépend de la locale : « côte » → « c?te » en locale C) par `chartr` sur les lettres accentuées utiles.
7. Facultatif, proposé : au lieu de « moins de 8 candidats », chaque candidat rendu doit satisfaire la règle de rapprochement.
8. Dire au rendu quels tests sont verts d'emblée et pourquoi (préconditions).

## Remarques de relecture — aller-retour 2/3 (2026-11-10)

Point de départ : `origin/claude/mission-006-tests-noms-resume` (commit `0af44d56`) ; pousser en fast-forward.

Arbitrages de Pascal (2026-11-10, suite) :
- « Début du mot-clé » se lit **par mots entiers** : « marsannay » et « marsannay la » sont des débuts de « marsannay la cote », « mars » n'en est pas un.
- Les heures naïves des fichiers projet (`created_at`, `updated_at`, `completed_at`, `fin_a`) se **lisent dans le fuseau du process lecteur**, puis sont rendues en **Europe/Paris**.
- **Priorité** de `calcule_le` : `completed_at` > `fin_a` **seulement si** `statut == "termine"` > date de `data/indicators.parquet` > NA.

À corriger (relecteur) :
1. **(bloquant)** Retirer la 3e clause de `.nd_rapproche` (« dicté contenu dans le nom complet ») ; règle = distance ≤ 2 au mot-clé OU dicté = suite de mots entiers en tête du mot-clé (préfixe « Forêt communale de » toléré devant). Ajouter des contrôles négatifs : « de », « onf », « foret », « communale », « mars » ne rapprochent aucun projet ; Mouthe reste introuvable.
2. **(bloquant)** Tests vacants : `calcule_le` et `global_score` du projet jamais calculé — vérifier la présence de la clé à `null` sur la chaîne JSON brute (`expect_match(json, '"calcule_le":null', fixed = TRUE)`), pas `is.null || is.na`.
3. CHM du second projet : `expect_identical(r2$chm_suspect, FALSE)`.
4. Tableau de la Synthèse : vérifier aussi, par famille, le **NDP** (`metadata$ndp_level %||% 0`, `R/mod_synthesis.R` l. ~577) et le **nom de famille** (`name_fr`).
5. `calcule_le` : un cas `fin_a` avec `statut = "echec"` (ignoré, repli sur le parquet) ; un cas où `completed_at` et `fin_a` coexistent (`completed_at` gagne).
6. Fuseau : un cas « écrit sous UTC, lu sous UTC » (`withr::local_timezone("UTC")` le temps de l'écriture et de la lecture) qui doit rendre l'heure convertie en Europe/Paris (+01:00 ou +02:00 selon la date).
7. Corriger le commentaire « indépendant de la locale » de la normalisation (`chartr` échoue sous `LC_ALL=C`) : dire qu'il suppose une locale UTF-8.

## Statut et fusion — 2026-11-10

- Relu **prêt** à l'aller-retour 2/3. Livrable de référence : commit `35cbf2be` (`origin/claude/mission-006-tests-noms-resume`). Rouge réel sur main : 16 tests rouges, 4 verts (garde-fous), selon le relecteur.
- Fusion : règle de 002 — pas de PR seule ; `test-service_mcp_noms_dictes.R` en tête de la branche 007, `test-service_mcp_resume.R` en tête de 008 (`git checkout 35cbf2be -- <fichier>`), contrôle `git diff 35cbf2be` vide par le relecteur.
- `familles$ndp` validé par Pascal (2026-11-10).
- **Statut** (bilan du 2026-11-13) : tests 35cbf2be intégrés à 007/008 (pas de PR propre), 2026-11-10.

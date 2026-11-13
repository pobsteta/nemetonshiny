# Mission 007 — Résolution tolérante des noms de projet

> Gabarit (fiches n° 9 et 15 de la méthode). Rempli par l'orchestrateur le 2026-11-09,
> **proposé à Pascal (porte n° 2)**. Une mission = une phrase = une branche = une PR.

## Mission

**En une phrase (quoi, et pourquoi) :** Étendre `.mcp_resolve_project` pour qu'un nom approximatif (accents, homophones, distance d'édition ≤ 2) donne le projet ou une seule liste de candidats, jamais un « introuvable » sec, pour que l'utilisateur ne redise pas le nom (P1, critère 1).

**Redite par l'agent :** non rédigée (fiche close au bilan du 2026-11-13)

## Les trois questions (règle de staffing)

| Question | Réponse | Conséquence |
|---|---|---|
| Peut-il redire la mission en une phrase, avec son but ? | oui | |
| Sait-il comment faire ? (sources, conventions nommées ci-dessous) | oui | |
| A-t-il les moyens ? (outils, données d'essai, tests) | oui (après 005, 006) | |

**Temps de la main :** appuyer — fonction isolée, tests prêts (006).

## Ma réponse d'abord

_Rempli par Pascal seul, avant de lire le travail de l'agent._

## L'Homme

- **Agent :** `dev`
- **Modèle :** sonnet
- **Ce qui ne lui est pas demandé :** un choix silencieux parmi plusieurs candidats.

## Harnais

- **Peut :** modifier `R/service_mcp.R`, NEWS.
- **Ne doit pas :** sortir du périmètre, toucher à `main`, écrire dans `nemeton`, `nemetonmcp`, `aigora` ou `victor` (lecture seule), mettre un littéral FR/EN hors i18n, ajouter de la logique métier.
- **Sources autorisées :** spec P1 ; tests 006 ; fixture 005 ; `utils::adist`.

## Valeur provisoire (chiffre, réglage)

- seuil de distance 2 : provisoire (voir 006).

## Dépend de / rouge attendu jusqu'à

- 005, 006 ; ordre 005 → 006 → 007.

## Tests

- **Comment saura-t-on que c'est juste :** tests de 006 (noms) verts. Commande : `.claude/test-command` (`devtools::test()`).

## Terrain

- **Échantillon que Pascal vérifiera :** Pascal dit « Couchet » à VICTOR : Couchey est trouvé.

## Qui veille, qui tranche

- **Relecteur :** `relecteur`, puis Pascal.
- **Critère d'arrêt de la relecture :** 3 allers-retours ; au-delà, la décision remonte à Pascal.
- **Point de suivi :** à la remise de la PR (ou du compte rendu), et au plan d'abord si la main est tenue.
- **Qui tranche :** Pascal.

## Comment saura-t-on que ça marche ?

plus de « introuvable » sur un nom dicté.

## Décision du 2026-11-10 (Pascal)

- Le rapprochement porte sur le **mot-clé de la commune** du projet (ex. « couchey » pour « Forêt communale de Couchey (ONF) »), pas sur le nom complet ; seuil du critère 1 (≤ 2). Projet sans commune dans son nom : le mot-clé est le nom lui-même.
- Règle complète (2026-11-10) : distance ≤ 2 au mot-clé OU suite de mots entiers en tête du mot-clé (« Forêt communale de » toléré devant) ; jamais de simple sous-chaîne (« de », « onf » ne rapprochent rien) ; plusieurs projets → `projet_ambigu` ; mot-clé tiré du nom du projet.

## Moyens — 2026-11-10

- Branche depuis `origin/main` (contient 003 : erreurs classées, `candidats` au format « nom (id) »).
- **Premier commit** : `git fetch origin claude/mission-006-tests-noms-resume && git checkout 35cbf2be -- tests/testthat/test-service_mcp_noms_dictes.R` (tests d'acceptation de 006, à ne pas retoucher ; la fixture `noms_dictes.csv` est déjà sur main via #254). Ils doivent passer au vert sans retouche.
- Règle : voir décision ci-dessus (mot-clé tiré du nom ; distance ≤ 2 OU suite de mots entiers en tête du mot-clé, « Forêt communale de » toléré ; plusieurs projets → `projet_ambigu` ; jamais de simple sous-chaîne).
- `candidats` garde le format « nom (id) » de 003.
- Cycle dev : 2026.10.18.9004 + NEWS (004 a 9003).
- Attention : 008 et 010 toucheront aussi `R/service_mcp.R` ; rester dans `.mcp_resolve_project` et ses helpers.

## Remarques de relecture — aller-retour 1/3 (2026-11-10)

Point de départ : `origin/claude/mission-007-noms-dictes` (b99be25e) ; pousser en fast-forward.

Arbitrages de Pascal (2026-11-10) :
- Extension « Forêt de / du / des » **gardée** (comme « Forêt communale de ») ; elle doit être **symétrique** : retirée aussi du côté dicté (« Forêt de Dabot » trouve « Forêt de Dabo »).
- **Plancher** : la tolérance de distance ≤ 2 ne s'applique qu'à un mot-clé d'**au moins 4 lettres** ; en dessous, seule l'égalité exacte ou la suite de mots entiers en tête compte (« de »/« du » ne rapprochent jamais « Forêt de Bé »).

À corriger :
1. **Locale C** : `chartr` plante sous `LC_ALL=C` (« invalid input multibyte string ») — régression du chemin approché. Remplacer par une table de correspondance en base R par points de code (`utf8ToInt`/`intToUtf8` sur `enc2utf8(x)`), sans dépendance ; ajouter un test sous `withr::local_locale(c(LC_CTYPE = "C"))` (ou `withr::local_envvar(LC_ALL = "C")`) qui résout « Couché » et « Forêt communale de Couché » sans erreur ; corriger le commentaire.
2. Symétrie du préfixe et plancher de 4 lettres (ci-dessus), avec tests : « Forêt de Dabot » → Dabo ; « de », « du » ne rapprochent pas un projet « Forêt de Bé ».
3. Roxygen de `.mcp_rapprocher` et NEWS : décrire la règle réelle (préfixes, symétrie, plancher).
4. Rendu : corriger le décompte des skips (2, ceux de 010).
5. Conflit connu avec #257 (DESCRIPTION/NEWS) : ne pas le résoudre ici ; l'orchestrateur fera fusionner main au moment voulu.

## Remarques de relecture — aller-retour 2/3 (2026-11-10)

Point de départ : `origin/claude/mission-007-noms-dictes` (0427664a) ; pousser en fast-forward.

Arbitrages de Pascal (2026-11-10) : plancher de 4 lettres **des deux côtés** (mot-clé ET dicté) ; « Forêt [communale] d' » retiré aussi (élision), des deux côtés ; le serveur MCP est **forcé en locale UTF-8** au lancement.

À corriger :
1. Chaînes non marquées en locale C : dans `.mcp_norm_dicte`, `if (Encoding(s) == "unknown" && validUTF8(s)) utf8ToInt(s) else utf8ToInt(enc2utf8(s))` ; test en **vrai sous-process** (`system2("Rscript", ...)` avec `LC_ALL=C`, `skip_on_cran()`), pas seulement `withr`.
2. Table d'accents : œ→oe, æ→ae, Œ→oe, Æ→ae (deux lettres) ; sortir ÿ (255) de la règle « majuscule = minuscule − 32 » (sinon ß→y) et ajouter Ÿ (376) à part ; test « Cœuvres » / « Coeuvres » / « Coeuvre ».
3. Élision : « Forêt [communale] d' » (apostrophe droite et typographique ’) retiré des deux côtés ; test : projets « Forêt communale d'Écouché » et « Forêt communale de Couchey (ONF) » — « Écouché » trouve Écouché, pas Couchey ; « Couchet » trouve Couchey.
4. Plancher côté dicté : corriger la justification (adist("de","dabo") = 3) ; ajouter un test qui isole le côté dicté (projet « Laix », dicté « la » → introuvable ; « les » face à « Lens » → introuvable).
5. Locale du serveur : `inst/mcp/server.R` pose une locale UTF-8 au démarrage (`Sys.setlocale("LC_CTYPE", "C.UTF-8")`, repli `en_US.UTF-8`, message `cli` si aucune n'est disponible) ; README : le dire.
6. Commentaire du test locale : « accents en clair », pas « en \u ».

## Dernier aller-retour — 3/3 (2026-11-10)

Relu **prêt** à l'aller-retour 2/3 (97bd8b6a). Point de départ : `origin/claude/mission-007-noms-dictes` (97bd8b6a) ; pousser en fast-forward. Ne faire QUE ce qui suit.

Arbitrage de Pascal (2026-11-10) : **l'égalité exacte au mot-clé ne l'emporte que si aucun autre projet n'est à distance ≤ 1** de ce mot-clé dicté ; sinon `projet_ambigu` avec les candidats. « Écouché » face à Couchey (distance 2) trouve Écouché ; « Couchey » face à Couchay (distance 1) rend les deux candidats.

1. Coder cette règle dans `.mcp_rapprocher` ; tests : les deux cas ci-dessus ; roxygen et NEWS à jour.
2. Corriger « au lancement.. » dans NEWS ; replier les deux lignes trop longues du roxygen de `.mcp_rapprocher`.
3. **Fusionner `origin/main`** (contient 004, #257) dans la branche (`git merge`, pas de rebase) ; DESCRIPTION : une seule ligne `Version: 2026.10.18.9004` ; NEWS : 9004 au-dessus de 9003, puis 9002.
4. `devtools::test(filter = "mcp|contrat")` vert ; `git diff 35cbf2be -- tests/testthat/test-service_mcp_noms_dictes.R` vide ; aucun marqueur de conflit ; non-ASCII.

## Statut

fusionnée #258 (63cda006), 2026-11-10.

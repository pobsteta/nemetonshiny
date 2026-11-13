# Mission 010 — Confirmation des écritures par jeton en deux temps

> Gabarit (fiches n° 9 et 15 de la méthode). Rempli par l'orchestrateur le 2026-11-09,
> **proposé à Pascal (porte n° 2)**. Une mission = une phrase = une branche = une PR.

## Mission

**En une phrase (quoi, et pourquoi) :** Faire que `appliquer_ugf`, `croiser_onf` (et toute écriture d'un projet existant) n'écrivent rien au premier appel, rendent un aperçu et un jeton à usage unique, et n'écrivent qu'au second appel portant ce jeton, avec un helper réutilisable par `nemetonmcp` pour ses propres écritures (plan d'actions), pour qu'aucune phrase mal transcrite ne modifie un projet (ADR 0002, P4).

**Redite par l'agent :** non rédigée (fiche close au bilan du 2026-11-13)

## Les trois questions (règle de staffing)

| Question | Réponse | Conséquence |
|---|---|---|
| Peut-il redire la mission en une phrase, avec son but ? | oui | |
| Sait-il comment faire ? (sources, conventions nommées ci-dessous) | oui | |
| A-t-il les moyens ? (outils, données d'essai, tests) | oui (après 009) | |

**Temps de la main :** tenir — garde de sécurité qui engage le contrat avec les clients : plan d'abord (registre, portée du jeton, aperçu), validé avant code.

## Ma réponse d'abord

_Rempli par Pascal seul, avant de lire le travail de l'agent._

## L'Homme

- **Agent :** `dev`
- **Modèle :** opus
- **Ce qui ne lui est pas demandé :** le relais vocal (dépôt victor), les permissions AIGORA.

## Harnais

- **Peut :** modifier `R/service_mcp.R`, `CONTRAT.md` (helper ajouté au contrat 001), `R/utils_i18n.R`, NEWS (changement cassant pour les clients : à signaler).
- **Ne doit pas :** sortir du périmètre, toucher à `main`, écrire dans `nemeton`, `nemetonmcp`, `aigora` ou `victor` (lecture seule), mettre un littéral FR/EN hors i18n, ajouter de la logique métier.
- **Sources autorisées :** ADR 0002, spec P4 et critère 4, tests 009, `.mcp_garde_ecriture`, `nemetonmcp/r/R/plan.R` (lecture seule) pour le besoin du helper.

## Valeur provisoire (chiffre, réglage)

- jeton 10 min : provisoire (voir 009).

## Dépend de / rouge attendu jusqu'à

- 001, 003, 009 ; ordre 001 → 003 → 009 → 010.

## Tests

- **Comment saura-t-on que c'est juste :** tests de 009 verts ; tests existants de `appliquer_ugf`/`croiser_onf` adaptés et annoncés dans la PR (seule retouche permise, car le comportement change par décision). Commande : `.claude/test-command` (`devtools::test()`).

## Terrain

- **Échantillon que Pascal vérifiera :** Pascal dit « applique les UGF ONF » à VICTOR : il entend l'aperçu, confirme, et seulement alors le projet change.

## Qui veille, qui tranche

- **Relecteur :** `relecteur`, puis Pascal.
- **Critère d'arrêt de la relecture :** 3 allers-retours ; au-delà, la décision remonte à Pascal.
- **Point de suivi :** à la remise de la PR (ou du compte rendu), et au plan d'abord si la main est tenue.
- **Qui tranche :** Pascal.

## Comment saura-t-on que ça marche ?

un « applique » mal compris ne modifie rien.

## Contrat fixé le 2026-11-10 (Pascal, sur la relecture de 009)

- Premier appel : erreur `confirmation_requise` + `jeton`, `apercu`, `expire_a` ; jeton lié à outil + projet + paramètres (fichier + md5, `remplacer`, `purger` résolu) ; registre en mémoire (ADR 0002), rien d'écrit dans le dossier projet à l'aperçu ; périmètre `appliquer_ugf` et `croiser_onf` (tout `purger`) ; erreurs métier et gardes avant l'aperçu, gardes rejouées à la confirmation ; horloge : `Sys.time()` (les tests la simulent par `local_mocked_bindings(.package = "base")`).
- Les deux `skip` de `test-service_mcp_erreurs.R` (002) sont à activer par 010 ; le cas « fichier absent → confirmation_requise » y contredit l'ordre retenu : seule retouche autorisée de ce fichier, à montrer au relecteur.
- Compléments du 2026-11-10 (Pascal) : **clés de l'aperçu fixées** — `appliquer_ugf` : `n_ugf_remplacees`, `n_ugf_nouvelles` ; `croiser_onf` : `ecartees` (liste d'IDU), `n_ecartees`, `n_retenues` ; **tout jeton connu présenté est consommé**, que l'écriture réussisse ou non (refus de lien, garde rejouée) ; durée **10 min**, une seule constante, à confirmer après l'essai VICTOR. À la confirmation, 010 fixe l'ordre fichier ↔ jeton (second `skip` de 002) et le montre au relecteur.
- Fusion : les tests de 009 entrent en tête de la branche 010 (pas de PR 009 seule) ; 010 part de `main` après fusion de 003 (#256).
- Compléments du 2026-11-10 (Pascal, suite) : le jeton porte l'empreinte de **tous les réglages ONF résolus** du projet (pas seulement `purger`) ; aperçu `croiser_onf` avec purger résolu FALSE : `ecartees` vide, `n_ecartees` 0, `n_retenues` = toutes les parcelles.
- Moyens (2026-11-10) : branche depuis `origin/main` (contient 003). **Premier commit** : `git fetch origin claude/mission-009-tests-confirmation && git checkout 93291ec8 -- tests/testthat/test-service_mcp_confirmation.R` (tests de 009, à ne pas retoucher). Main **tenue** : rendre d'abord un plan (forme du registre en mémoire, empreinte du lien — outil, projet, fichier + md5, `remplacer`, réglages ONF résolus —, consommation, ordre des contrôles fichier ↔ jeton à la confirmation, clés de l'aperçu, activation des 2 `skip` de `test-service_mcp_erreurs.R` et correction du cas « fichier absent »), soumis à Pascal avant toute exécution.

## Plan accepté — 2026-11-10

Plan rendu par le `dev` (main tenue), accepté par Pascal avec ces arbitrages :
- **Empreinte** : outil, projet (id résolu), et en plus le **md5 du fichier de parcelles du projet** (Pascal) ; `appliquer_ugf` : fichier normalisé + md5 + `remplacer` ; `croiser_onf` : `purger` résolu + tous les réglages ONF résolus (`project_onf_params`). L'empreinte ONF n'est portée que par `croiser_onf` (orchestrateur).
- **croiser_onf** : le croisement est **recalculé** à la confirmation (Pascal) ; rien de lourd gardé en mémoire.
- **Aperçu `remplacer = FALSE`** : `n_ugf_remplacees` = UGF actuelles des parcelles du fichier ; `n_ugf_nouvelles` = libellés distincts du fichier (Pascal).
- **Contrat** : `.mcp_jeton_emettre`, `.mcp_jeton_consommer`, `.mcp_garde_ecriture` au § 6 bis, avec l'exception datée étendue jusqu'au brief 013 ; `.mcp_empreinte` reste interne (Pascal).
- **Jeton** : `sample()` sur `[0-9a-z]`, 24 car., pas de nouvelle dépendance (orchestrateur ; « barrière, pas preuve », ADR 0002).
- **Ordre** : gardes → jeton (consommé dès la présentation) → fichier/métier → écriture (orchestrateur, conforme aux décisions du 2026-11-10).
- **Changement cassant** : `appliquer_ugf`/`croiser_onf` n'écrivent plus au premier appel → section « Changements cassants » dans NEWS ; la release qui l'embarquera demandera la confirmation de Pascal (CLAUDE.md).
- Cycle dev : 2026.10.18.9005 (007 prend 9004) + NEWS. Branche depuis `origin/main` ; tests de 009 en premier commit, jamais retouchés ; ajouter un test (dans `test-service_mcp_jeton.R`) pour « parcelles modifiées entre aperçu et confirmation → jeton_invalide » ; vérifier R4 (le vrai `onf_load_parcelles` n'écrit rien dans le dossier projet) et le dire au rendu.
- Attention : 007 touche aussi `R/service_mcp.R` (`.mcp_resolve_project`) ; rester hors de cette fonction.

## Remarques de relecture — complément (2026-11-10)

Relu **prêt** (91 tests / 1759 vérifications, 7 mutations détectées). Point de départ : `origin/claude/mission-010-confirmation-jeton` (bade93a8) ; pousser en fast-forward. Arbitrages de Pascal :
1. **Projet nommé** : le message `mcp_err_confirmation_requise` cite le nom résolu du projet, et le JSON de `confirmation_requise` porte `projet` (id) et `projet_nom` ; test : l'aperçu d'`appliquer_ugf` et de `croiser_onf` contient l'id et le nom exacts.
2. **Exception écrite** : un jeton présenté avec un projet introuvable/ambigu, ou quand les métadonnées ONF sont illisibles (lues avant le jeton), n'est pas consommé ; le dire dans `inst/mcp/README.md`, `CONTRAT.md` et NEWS (au lieu de « tout jeton présenté est consommé, même refusé »).
3. Le README modifié hors harnais est **gardé** (il devenait faux) ; l'annoncer dans la PR.
4. Ne PAS fusionner main ici : ordre de fusion 004 → 007 → 010 ; l'orchestrateur fera fusionner main au moment voulu (NEWS à remettre en ordre 9005 > 9004 > 9003).

## Statut

fusionnée #259 (1af92486), 2026-11-10.

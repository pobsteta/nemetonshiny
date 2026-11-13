# Mission 016 — Registre des jetons de confirmation persisté sur disque

> Gabarit (fiches n° 9 et 15 de la méthode). Rempli par l'orchestrateur le 2026-11-10,
> sur décision de Pascal du 2026-11-10 (journal, ligne « cap » 010/016).
> Une mission = une phrase = une branche = une PR.

## Mission

**En une phrase (quoi, et pourquoi) :** Faire survivre le jeton de confirmation de la mission 010 au redémarrage du serveur MCP en persistant son registre dans un fichier local de nemetonshiny, pour que la confirmation vocale de VICTOR (un `claude -p` neuf, donc un serveur MCP neuf, par tâche) fonctionne : l'aperçu dans la tâche 1, le « oui » et l'écriture dans la tâche 2.

**Redite par l'agent :** non rédigée (fiche close au bilan du 2026-11-13)

## Constat (vérifié par l'orchestrateur, 2026-11-10)

- `victor/victor/server.py`, `_run_task` : `claude -p <prompt>` neuf par tâche, sans `--resume`, stdin fermé.
- `aigora/aigora-nemeton/CLAUDE.md:123-124` : « Each `claude -p` restarts the server ».
- Le registre `.mcp_jetons` (environnement en mémoire, `R/service_mcp.R`) meurt donc avec le processus : 2e temps → `jeton_invalide`.

## Les trois questions (règle de staffing)

| Question | Réponse | Conséquence |
|---|---|---|
| Peut-il redire la mission en une phrase, avec son but ? | oui | |
| Sait-il comment faire ? (sources, conventions nommées ci-dessous) | oui | |
| A-t-il les moyens ? (outils, données d'essai, tests) | oui (010 fusionnée, tests 009) | |

**Temps de la main :** appuyer — la conception est tranchée par Pascal (registre sur disque) ; le contrat client (forme de `confirmation_requise`, usage unique, 10 min, empreinte) ne change pas.

## Ma réponse d'abord

_Rempli par Pascal seul, avant de lire le travail de l'agent._

## L'Homme

- **Agent :** `dev`
- **Modèle :** opus
- **Ce qui ne lui est pas demandé :** les briefs (mission 013, reprise après 016), le relais vocal côté victor, un serveur MCP persistant.

## Ce qui est attendu

1. **Emplacement** : par défaut `tools::R_user_dir("nemetonshiny", "data")/mcp/jetons.rds` (ou un fichier par jeton dans `mcp/jetons/` si c'est plus sûr en concurrence) ; surchargeable par la variable d'environnement `NEMETON_MCP_JETONS_DIR` (tests, déploiement). Répertoire créé au besoin, droits `0700` répertoire / `0600` fichier (sous Unix).
2. **Contrat inchangé** : `.mcp_jeton_emettre` / `.mcp_jeton_consommer` gardent leur signature et leur comportement (usage unique, `.MCP_JETON_DUREE` = 10 min, empreinte identique, règles de non-consommation documentées dans CONTRAT.md § 6 bis). Le contrat reste à 35 objets sauf ajout justifié.
3. **Concurrence** : deux serveurs MCP peuvent tourner en même temps (VICTOR + AIGORA). Écriture atomique (fichier temporaire dans le même répertoire puis `file.rename`) ; la consommation d'un jeton ne doit pas pouvoir réussir deux fois (un fichier par jeton supprimé à la consommation, `file.remove` faisant foi, est une option simple — à justifier).
4. **Purge** : les jetons expirés sont purgés à l'émission et à la consommation ; un fichier illisible ou corrompu est ignoré (jeton inconnu → `jeton_invalide`), jamais une erreur R non classée.
5. **Jeton** : tiré d'une source non prévisible et non affectée par `set.seed()` du processus (ex. `openssl::rand_bytes` si déjà disponible, sinon lecture de `/dev/urandom`, sinon repli documenté) — le jeton est désormais lisible sur disque par l'utilisateur, il ne doit pas être devinable. Ne pas ajouter de dépendance `Imports` sans le signaler.
6. **Rien de secret ni de métier sur disque** au-delà de : jeton, empreinte, outil, id projet, `expire_a`. Pas de géométrie, pas d'aperçu complet.

## Harnais

- **Peut :** modifier `R/service_mcp.R`, `CONTRAT.md`, `inst/mcp/README.md`, NEWS, DESCRIPTION (version), tests `tests/testthat/test-service_mcp_*.R`.
- **Ne doit pas :** sortir du périmètre, toucher à `main`, écrire dans `nemeton`, `nemetonmcp`, `aigora` ou `victor` (lecture seule), mettre un littéral FR/EN hors i18n, `print/cat/message` en prod, écrire dans le vrai `R_user_dir` pendant les tests (toujours `NEMETON_MCP_JETONS_DIR` sur un `withr::local_tempdir()`).
- **Sources autorisées :** fiche 010, ADR 0002, CONTRAT.md § 6 bis, tests 009 (`test-service_mcp_confirmation.R`), `victor/victor/server.py` (lecture seule).

## Valeur provisoire (chiffre, réglage)

- jeton 10 min : provisoire (inchangé depuis 009).

## Dépend de / rouge attendu jusqu'à

- 010 fusionnée (#259). Aucun rouge attendu. Se fusionne avant ou après 008 (#260) : seules les fonctions du jeton changent dans `R/service_mcp.R`.

## Tests

- Tous les tests 009/010 restent verts sans modification de leurs attentes (sauf l'isolation du répertoire).
- Nouveau : **émettre dans un sous-processus R, consommer dans un autre** (`Rscript` + `pkgload::load_all`, garde « sources R absentes » comme 007) → écriture acceptée ; rejouer → `jeton_invalide`.
- Nouveau : jeton expiré sur disque → refusé et purgé ; fichier corrompu → `jeton_invalide` ; `set.seed()` identique dans deux processus → jetons différents.
- Commande : `.claude/test-command` (`devtools::test(filter = "mcp")`).

## Terrain

- **Échantillon que Pascal vérifiera :** dire à VICTOR « applique les UGF ONF à Couchey » → aperçu lu (nom + id) ; tâche suivante « oui » → écriture effective.

## Qui veille, qui tranche

- **Relecteur :** `relecteur`, puis Pascal.
- **Critère d'arrêt de la relecture :** 3 allers-retours ; au-delà, la décision remonte à Pascal.
- **Qui tranche :** Pascal.

## Comment saura-t-on que ça marche ?

La confirmation vocale aboutit en deux tâches VICTOR sans `jeton_invalide`.

## Statut

fusionnée #261 (348ff3df), 2026-11-11.

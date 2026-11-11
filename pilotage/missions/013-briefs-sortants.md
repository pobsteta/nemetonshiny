# Mission 013 — Briefs vers nemetonmcp, aigora et victor

> Gabarit (fiches n° 9 et 15 de la méthode). Rempli par l'orchestrateur le 2026-11-09,
> **proposé à Pascal (porte n° 2)**. Une mission = une phrase = une branche = une PR.

## Mission

**En une phrase (quoi, et pourquoi) :** Rédiger les briefs que la spec § 7 prévoit (nemetonmcp : contrat, jeton, isError, noms dédoublonnés, renommage incomplet ; aigora : lien `nemeton-vues` cassé, permissions ; victor : relais vocal de la confirmation), pour que les trois clients suivent ce qui change ici.

**Redite par l'agent :** _à compléter par l'agent avant d'agir — s'il n'y arrive pas, il s'arrête._

## Les trois questions (règle de staffing)

| Question | Réponse | Conséquence |
|---|---|---|
| Peut-il redire la mission en une phrase, avec son but ? | oui | |
| Sait-il comment faire ? (sources, conventions nommées ci-dessous) | oui | |
| A-t-il les moyens ? (outils, données d'essai, tests) | oui (après 010) | |

**Temps de la main :** appuyer — rédaction, pas de code.

## Ma réponse d'abord

_Rempli par Pascal seul, avant de lire le travail de l'agent._

## L'Homme

- **Agent :** `archiviste`
- **Modèle :** sonnet
- **Ce qui ne lui est pas demandé :** écrire dans les dépôts destinataires.

## Harnais

- **Peut :** écrire dans `briefs/vers-nemetonmcp/`, `briefs/vers-aigora/`, `briefs/vers-victor/` de ce dépôt.
- **Ne doit pas :** sortir du périmètre, toucher à `main`, écrire dans `nemeton`, `nemetonmcp`, `aigora` ou `victor` (lecture seule), mettre un littéral FR/EN hors i18n, ajouter de la logique métier.
- **Sources autorisées :** spec § 7, ADR 0001-0002-0004, PR fusionnées de 001, 003, 010.

## Valeur provisoire (chiffre, réglage)

- aucune.

## Dépend de / rouge attendu jusqu'à

- 001, 003, 010 fusionnées (un brief décrit ce qui est livré, pas ce qui est prévu).

## Tests

- **Comment saura-t-on que c'est juste :** chaque brief cite les SHA, les fonctions et les tests concernés. Commande : `.claude/test-command` (`devtools::test()`).

## Terrain

- **Échantillon que Pascal vérifiera :** Pascal ouvre chaque brief dans la session du dépôt destinataire.

## Qui veille, qui tranche

- **Relecteur :** `relecteur`, puis Pascal.
- **Critère d'arrêt de la relecture :** 3 allers-retours ; au-delà, la décision remonte à Pascal.
- **Point de suivi :** à la remise de la PR (ou du compte rendu), et au plan d'abord si la main est tenue.
- **Qui tranche :** Pascal.

## Comment saura-t-on que ça marche ?

les trois dépôts suivent sans que Pascal réexplique.

## Ajout du 2026-11-10 (Pascal)

- Brief supplémentaire vers le dépôt `victor` : journaliser le texte dicté (transcription STT) à côté des appels d'outils, pour que la fixture `noms_dictes.csv` (mission 005) puisse être alimentée par de vraies dictées. Rien n'est écrit dans `victor` depuis cette session.
- Brief vers `nemetonmcp` (2026-11-10, Pascal) ; lancer son serveur avec `TZ=Europe/Paris` et une locale UTF-8 (comme `inst/mcp/server.R`) : appeler `.ns(".mcp_vers_outil")(...)` sur le résultat de `etat_calcul_vue` et `exporter_gpkg_vue` (`r/R/outils.R`) — et surtout pas `.mcp_marquer`, qui échoue hors de nemetonshiny (il cherche `.mcp_vers_outil` dans l.espace de noms de la fonction enveloppée) pour que leurs erreurs sortent `isError: true` ; aligner le `.mcp_call` de `r/R/utils.R` sur la forme `{ok, classe, code, langue, erreur, action, candidats}` ; rappeler que le contrôle « nemetonmcp n'appelle rien hors contrat » est sauté en CI (chemin en dur, `NEMETONMCP_R_DIR`).

## Décisions du 2026-11-11 (Pascal), sur la relecture 1/3

- Transit du jeton entre deux tâches VICTOR : la tâche 1 termine sa sortie par un **bloc JSON balisé** `{outil, projet, projet_nom, jeton, expire_a, arguments}` (chemins absolus) ; VICTOR le garde sans le lire à voix haute et le réinjecte tel quel dans le prompt de la tâche 2 après un « oui » explicite. Format identique dans les briefs victor et aigora.
- Voie rapide de VICTOR : ne résout que l'id ou le nom exact (normalisé) ; tout autre nom passe par le MCP (règles 007, `candidats`). Pas de second moteur tolérant.
- La garde passe avant la consommation du jeton (mission 012) : un refus `calcul_en_cours`/`verrou` ne consomme plus le jeton — à annoncer comme à venir (non livré).
- Autorisé hors harnais : ajouter `^briefs$` à `.Rbuildignore` (sinon NOTE R CMD check).

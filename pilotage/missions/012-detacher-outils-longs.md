# Mission 012 — Détacher les outils longs mesurés

> Gabarit (fiches n° 9 et 15 de la méthode). Rempli par l'orchestrateur le 2026-11-09,
> **proposé à Pascal (porte n° 2)**. Une mission = une phrase = une branche = une PR.

## Mission

**En une phrase (quoi, et pourquoi) :** Détacher sur le modèle `lancer_calcul`/`etat_calcul` les outils que la mission 011 a mesurés au-delà de 60 s, pour qu'aucun appel ne dépasse 600 s (ADR 0004, P3).

**Redite par l'agent :** _à compléter par l'agent avant d'agir — s'il n'y arrive pas, il s'arrête._

## Les trois questions (règle de staffing)

| Question | Réponse | Conséquence |
|---|---|---|
| Peut-il redire la mission en une phrase, avec son but ? | oui | |
| Sait-il comment faire ? (sources, conventions nommées ci-dessous) | oui | |
| A-t-il les moyens ? (outils, données d'essai, tests) | non à la 3e (mesure absente) → attend 011 | |

**Temps de la main :** appuyer — mécanisme existant à réutiliser.

## Ma réponse d'abord

_Rempli par Pascal seul, avant de lire le travail de l'agent._

## L'Homme

- **Agent :** `dev`
- **Modèle :** sonnet
- **Ce qui ne lui est pas demandé :** détacher ce qui n'a pas été mesuré au-delà du seuil.

## Harnais

- **Peut :** modifier `R/service_mcp.R`, NEWS.
- **Ne doit pas :** sortir du périmètre, toucher à `main`, écrire dans `nemeton`, `nemetonmcp`, `aigora` ou `victor` (lecture seule), mettre un littéral FR/EN hors i18n, ajouter de la logique métier.
- **Sources autorisées :** ADR 0004, résultats de 011, `.mcp_spawn`/`.mcp_job_*`.

## Valeur provisoire (chiffre, réglage)

- seuil 60 s : provisoire jusqu'à 011.

## Dépend de / rouge attendu jusqu'à

- 011 (et 003 pour les erreurs). Mission conditionnelle : annulée si 011 ne trouve rien au-delà de 60 s.

## Tests

- **Comment saura-t-on que c'est juste :** à écrire par un `testeur` après 011 (état `en_cours`/`termine`/`echec` + fin de log). Commande : `.claude/test-command` (`devtools::test()`).

## Terrain

- **Échantillon que Pascal vérifiera :** Pascal dit « fais-moi le rapport de Couchey » : VICTOR rend la main et annonce la fin.

## Qui veille, qui tranche

- **Relecteur :** `relecteur`, puis Pascal.
- **Critère d'arrêt de la relecture :** 3 allers-retours ; au-delà, la décision remonte à Pascal.
- **Point de suivi :** à la remise de la PR (ou du compte rendu), et au plan d'abord si la main est tenue.
- **Qui tranche :** Pascal.

## Comment saura-t-on que ça marche ?

plus de tâche VICTOR coupée à 600 s.

## Décision du 2026-11-10 (Pascal, sur la mesure 011)

- Outils à détacher : `croiser_onf` (79-160 s, jamais sous 60 s) et `generer_rapport` (49 s en régime établi, 220 s au premier appel d'un process neuf — tuiles `maptiles` en cache dans `tempdir()`).
- Modèle : `lancer_calcul` / `etat_calcul`. Moyens : oui/oui/**oui** (la mesure est faite).
- Chiffres : journal du 2026-11-10 ; données brutes `/tmp/claude-1000/-home-pascal-dev-nemetonshiny/dd84f686-3feb-4ed0-97c1-7db1cc6e0c3d/scratchpad/bench/resultats.tsv` (scratchpad, non durable).

## Décision du 2026-11-11 (Pascal) : croiser_onf détaché et jeton

- Le 2e appel (avec `jeton`) valide et consomme le jeton, lance le croisement en tâche de fond et rend **tout de suite un `job_id`** ; l'avancement se suit par `etat_calcul`. Le 1er appel (aperçu) reste synchrone.
- À mesurer et rapporter : la durée de l'aperçu de `croiser_onf` (s'il refait le croisement complet, il dépasse 60 s et la décision remonte à Pascal).
- Ordre : `testeur` d'abord (tests d'acceptation sans lire le code), puis `dev` après la fusion de 016 (#261, même fichier `R/service_mcp.R`). Version du dev : à fixer au lancement.

## Décisions du 2026-11-11 (Pascal), sur les questions du testeur

1. Suivi : `etat_calcul(projet)` inchangé ; la réponse dit quel travail tourne (`type` : calcul, rapport, onf) et son `job_id`.
2. Erreur d'un job : au premier niveau, `statut = "echec"` + champs d'erreur MCP habituels (`classe`, `code`, `langue`, `erreur`, `action`) à côté de `statut`.
3. Un seul job à la fois par projet (`data/compute_job.json`) : un second lancement (calcul, rapport ou ONF) est refusé en `calcul_en_cours` (action : attendre, suivre avec `etat_calcul`) ; pour `croiser_onf`, le jeton n'est alors **pas** consommé.

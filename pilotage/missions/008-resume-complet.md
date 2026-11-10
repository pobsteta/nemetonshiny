# Mission 008 — Résumé de projet complet en un appel

> Gabarit (fiches n° 9 et 15 de la méthode). Rempli par l'orchestrateur le 2026-11-09,
> **proposé à Pascal (porte n° 2)**. Une mission = une phrase = une branche = une PR.

## Mission

**En une phrase (quoi, et pourquoi) :** Compléter `mcp_resume_projet` (UGF, date de calcul, heures ISO 8601 avec fuseau) pour qu'il rende tout ce qu'affiche l'onglet Synthèse, à écart 0, et qu'un seul appel suffise à « où en est Dabo ? » (P1, critère 2).

**Redite par l'agent :** _à compléter par l'agent avant d'agir — s'il n'y arrive pas, il s'arrête._

## Les trois questions (règle de staffing)

| Question | Réponse | Conséquence |
|---|---|---|
| Peut-il redire la mission en une phrase, avec son but ? | oui | |
| Sait-il comment faire ? (sources, conventions nommées ci-dessous) | oui | |
| A-t-il les moyens ? (outils, données d'essai, tests) | oui (après 006) | |

**Temps de la main :** appuyer — lecture seule, appel à `nemeton::compute_general_index()` déjà en place.

## Ma réponse d'abord

_Rempli par Pascal seul, avant de lire le travail de l'agent._

## L'Homme

- **Agent :** `dev`
- **Modèle :** sonnet
- **Ce qui ne lui est pas demandé :** aucune logique métier nouvelle.

## Harnais

- **Peut :** modifier `R/service_mcp.R`, `R/api.R`, NEWS.
- **Ne doit pas :** sortir du périmètre, toucher à `main`, écrire dans `nemeton`, `nemetonmcp`, `aigora` ou `victor` (lecture seule), mettre un littéral FR/EN hors i18n, ajouter de la logique métier.
- **Sources autorisées :** spec P1, critère 2 ; tests 006 ; `R/mod_synthesis.R`.

## Valeur provisoire (chiffre, réglage)

- aucune.

## Dépend de / rouge attendu jusqu'à

- 006.

## Tests

- **Comment saura-t-on que c'est juste :** tests de 006 (résumé) verts. Commande : `.claude/test-command` (`devtools::test()`).

## Terrain

- **Échantillon que Pascal vérifiera :** Pascal compare la réponse de VICTOR à l'écran Synthèse de Dabo.

## Qui veille, qui tranche

- **Relecteur :** `relecteur`, puis Pascal.
- **Critère d'arrêt de la relecture :** 3 allers-retours ; au-delà, la décision remonte à Pascal.
- **Point de suivi :** à la remise de la PR (ou du compte rendu), et au plan d'abord si la main est tenue.
- **Qui tranche :** Pascal.

## Comment saura-t-on que ça marche ?

écart 0 entre la voix et l'écran.

## Décisions du 2026-11-10 (Pascal)

- Champs : `description`, `created_at`, `updated_at` (ISO 8601 + fuseau), `calcule_le`, `chm_suspect`, `chm_suspect_max`, `familles$n_indicateurs` ; scores à 2 décimales comme la Synthèse ; NDP et nom par famille.
- Heures naïves lues dans le fuseau du process, rendues en Europe/Paris.
- `calcule_le` : `completed_at` > `fin_a` si `statut == "termine"` > mtime de `data/indicators.parquet` > NA.

## Moyens — 2026-11-10

- Branche depuis `origin/main` (contient 003 et 004).
- **Premier commit** : `git fetch origin claude/mission-006-tests-noms-resume && git checkout 35cbf2be -- tests/testthat/test-service_mcp_resume.R` (tests d'acceptation de 006, à ne pas retoucher ; ils doivent passer au vert sans retouche).
- Décisions de Pascal : voir « Décisions du 2026-11-10 » ci-dessus, plus `familles$ndp` (même valeur sur les 12 familles, comme la Synthèse), nom de famille = `name_fr` en FR par défaut.
- Aucune logique métier dans l'app : lire ce que la Synthèse affiche (`R/mod_synthesis.R`, `R/service_synthesis.R`) et le rendre ; scores à 2 décimales comme l'écran (écart réel aujourd'hui : C 53,3 contre 53,33).
- Cycle dev : 2026.10.18.9006 + NEWS (007 = 9004, 010 = 9005, pas encore sur main).
- Rester dans `mcp_resume_projet` et ses helpers : 007 touche `.mcp_resolve_project`, 010 `appliquer_ugf`/`croiser_onf`.

## Complément — 2026-11-10 (Pascal)

Relu **prêt** (3d87cca0 : écart 0 sur Dabo, Couchey, Marsannay). Point de départ : `origin/claude/mission-008-resume-complet` (3d87cca0) ; pousser en fast-forward ; ne PAS fusionner main (ordre 007 → 010 → 008, l'orchestrateur le fera).
1. **Fuseau du serveur MCP** forcé à Europe/Paris au lancement : dans `inst/mcp/server.R`, `Sys.setenv(TZ = "Europe/Paris")` au démarrage (à côté de la locale UTF-8 posée par 007 — si `inst/mcp/server.R` de ta base n'a pas encore ce bloc, ajoute le tien en tête, après la vérification de mcptools) ; README MCP : le dire.
2. NEWS : `updated_at` n'est pas nouveau, seul son **format** change (heure naïve → ISO 8601 avec fuseau) : le dire dans « Changements cassants ».
3. `devtools::test(filter = "mcp|contrat")` vert ; `git diff 35cbf2be -- tests/testthat/test-service_mcp_resume.R` vide.

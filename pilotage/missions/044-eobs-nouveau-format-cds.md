# Mission 044 — E-OBS : nouveau format du CDS, cache partagé, plus de faux `ok`

> Créée par l'orchestrateur le 2026-11-23 (Pascal : « oui », coder le brief). Solde
> `~/dev/briefs/vers-nemetonshiny/2026-11-22-eobs-nouveau-format-cds.md` — **le lire en entier**
> (constat Colombière, run `run_20261122154945_eghc` : toutes les requêtes E-OBS en « invalid request »).

## Mission

**En une phrase :** l'app télécharge E-OBS par le cache global du cœur (un seul jeu de fichiers
pour tous les projets), retrouve le fichier par `nemeton::eobs_cached_file()`, et les étapes
`regen_eobs_rr` / `regen_eobs_tg` ne répondent plus `ok` quand rien n'a été téléchargé.

## État du cœur

- nemeton **v2026.11.39 taguée** (2026-11-22) : nouvelle requête CDS (34.0e, `licence_type`),
  cache global par défaut quand `cache_dir = NULL`, nouvel export `eobs_cached_file(var)`
  (chemin ou `NULL`). Installée localement : 2026.11.38 → installer la release :
  `Rscript -e 'pak::pkg_install("pobsteta/nemeton@v2026.11.39")'` (depuis GitHub, **ne jamais
  toucher `/home/pascal/dev/nemeton`**). Vérifier la signature réelle de `eobs_cached_file()`
  (`args(nemeton::eobs_cached_file)`) avant de coder.

## Travail attendu (points 1 à 6 du brief, lieux à vérifier sur `main`)

1. `R/service_regeneration.R` : `run_regeneration_detect_years()` (~1536),
   `regen_fetch_eobs_rr()` (~248), `regen_fetch_eobs_tg()` (~276) appellent
   `load_eobs_source()` **sans** `cache_dir` du projet. `eobs_status.json`
   (`.regen_eobs_progress_cb`) peut rester dans le dossier du projet.
2. `regen_eobs_cached_nc()` (~214) délègue à `nemeton::eobs_cached_file(var)`, repli sur
   l'ancien motif local (projets déjà téléchargés). Vérifier tous ses appelants (moteur, carte).
3. Fenêtre d'années : retirer le commentaire périmé (blocs CDS, 2011 minimum, 30.0e) ; fenêtre
   se terminant à `annee_courante - 1`.
4. Pipeline + bouton : la valeur rendue par la tâche (`TRUE`/`FALSE`) fixe le statut —
   `FALSE` → `error` (ou `skipped` avec motif, message du cœur repris), jamais `ok`. Toast
   « échec » côté bouton (`regen_eobs_rr_failed` existe, ~1678 de `R/mod_regeneration.R`).
   Décision dans un helper pur testable (`ExtendedTask` ne tourne pas sous `testServer`).
5. Textes : « ~800 MB » → « ~4,3 Go au premier téléchargement, puis partagé entre projets »
   (roxygen de `regen_fetch_eobs_*`, `R/utils_theme.R:205`, i18n `R/utils_i18n.R` ~6569 et
   ~6633, FR et EN, `\uXXXX`).
6. `DESCRIPTION` : `nemeton (>= 2026.11.39)` (section « Dépendances » de NEWS).

## Tests

Ceux du brief : `load_eobs_source()` appelé sans `cache_dir` (mock, contrôle des arguments) ;
`regen_eobs_cached_nc()` délègue à `eobs_cached_file()` avec repli local ; tâche qui rend
`FALSE` → `error`/`skipped`, jamais `ok`. Mutations rouges (remettre `cache_dir` projet ;
statut tiré du seul `success`). Fichiers `test-service_regeneration*.R`,
`test-mod_regeneration*.R`, `test-service_pipeline.R`, `test-utils_i18n.R`, `timeout 540`,
fichier par fichier. `grep -nP "[^\x00-\x7F]" R/*.R` sans ajout.

## Versionnage

`2026.11.3.9003`, entrée NEWS (`fix`), section « Dépendances ».

## Moyens

- Homme : `dev` (sonnet), worktree depuis `origin/main`, branche `claude/044-eobs-nouveau-format-cds`.
- Relecteur sur la PR. Temps de la main : *appuyer*.
- Terrain (Pascal) : Colombière, « Tout calculer » → `regen_annees` `ok` (deux années),
  `regen_eobs_rr`/`tg` `ok`, un seul jeu de fichiers dans le cache global, aucun dans
  `cache/regeneration/eobs` du projet ; un second projet ne retélécharge rien.

## Statut

Terminée le 2026-11-23 : PR #318 fusionnée (`b2d9117f`, `.9003`). Relecture : un bloquant (climatologie sans `years`, tranché par Pascal : dix ans) et trois mineurs, corrigés en `e7f70fd3` ; le point N-1 contre 34.0e est parti en brief vers le cœur. Reste l'essai terrain sur la Colombière.

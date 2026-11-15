# Mission 031 — La synthèse compte les UGF du projet, pas les lignes de scores

> Créée par l'orchestrateur le 2026-11-15 sur ordre de Pascal (« traite-les tous […] ne demande
> rien »). Solde le brief `~/dev/briefs/vers-nemetonshiny/2026-11-15-fiche-zero-ugf-indicateurs-perimes.md`
> (session victor, essai vocal du 2026-11-15) — **le lire en entier**.

## Mission

**En une phrase :** `project_synthesis_summary()` rend le vrai nombre d'UGF même quand les
indicateurs sont périmés ou jamais calculés, et signale `indicateurs_perimes = TRUE` quand un
`indicators.perime-*.parquet` est le seul fichier d'indicateurs présent.

## Constat (orchestrateur, lu sur `main` le 2026-11-15)

- `R/service_synthesis.R:172-193` : `n_ugf = if (is.null(family_sf)) 0L else nrow(family_sf)` —
  sans indicateurs, 0 UGF. Sur Couchey (`~/.local/share/nemeton/projects/20261008_212542_uywc`,
  `ug_count: 63`), la fiche dit « 0 unité de gestion » après `croiser_onf`.
- `R/service_project.R:899-970` : l'écriture des UGF renomme `data/indicators.parquet` en
  `data/indicators.perime-<horodatage>.parquet` (jamais supprimé).
- Le brief cite `api.R:80` comme modèle (`metadata$ug_count`) : le retrouver.

## Travail attendu

1. `n_ugf` = `nrow(project$ugs)` (ou l'objet UGF du projet, selon son nom réel), à défaut
   `metadata$ug_count`, à défaut `nrow(family_sf)`, à défaut 0.
2. Champ nouveau `indicateurs_perimes` (logique) : `TRUE` quand il n'y a pas
   d'`indicators.parquet` mais au moins un `indicators.perime-*.parquet` ; `FALSE` sinon. Le
   documenter (roxygen) et l'ajouter au contrat nemetonmcp si `project_synthesis_summary` y figure
   (`CONTRAT.md`, `tests/testthat/test-contrat_nemetonmcp.R`) — ajout seul, rien de cassant.
3. Si l'écran Synthèse ou `resume_projet` (MCP) affiche `n_ugf` ou un score vide, vérifier qu'il dit
   « scores à recalculer » quand `indicateurs_perimes` (clé i18n NMT, si un texte est ajouté).

## Tests

Projet de test avec UGF et sans `indicators.parquet` → `n_ugf` = nombre d'UGF, `global_score = NA`,
`indicateurs_perimes = FALSE` ; même projet avec un `indicators.perime-*.parquet` → `TRUE` ; projet
calculé → inchangé. Mutation rouge (remettre `nrow(family_sf)`). Filtre `synthesis|mcp`,
`timeout 540`.

## Lecteurs et appelants

`nemetonmcp` (`r/R/fiche.R` l. 81, 112, 118, lecture seule), `resume_projet` (MCP), écran Synthèse,
rapport. Après la release, nemetonmcp doit être réinstallé : l'orchestrateur le note au brief retour.

## Versionnage

Version donnée par l'orchestrateur au lancement.

## Moyens

- Homme : `dev` (sonnet), worktree depuis `main`, branche `claude/031-synthese-n-ugf`.
- Relecteur sur la PR. Temps de la main : *appuyer*.

## Statut

Terminée le 2026-11-15 : PR #285 fusionnée (`93299204`), `.9003`. Reste : réinstaller nemetonmcp après la prochaine release, rouvrir la fiche de Couchey.

# Mission 042 — Inventaire vidéo : le CVAT du projet part dans le projet terrain

> Créée par l'orchestrateur le 2026-11-22 (Pascal : « c'est fait », nemeton v2026.11.37 publiée).
> Solde `~/dev/briefs/vers-nemetonshiny/2026-11-20-cvat-path-inventaire-video.md` — **le lire**
> (mise à jour du 2026-11-22 en bas).

## Mission

**En une phrase :** quand le projet a déjà un CVAT calculé, la génération du projet d'inventaire
vidéo le passe au cœur (`cvat_path`), qui l'écrit en `rasters/cvat.tif` du projet terrain.

## Constat (orchestrateur, `main` au 2026-11-22, `5f98aaab`)

- `DESCRIPTION:15` : `nemeton (>= 2026.11.35)`. `cvat_path` n'existe qu'à partir de
  **nemeton 2026.11.37** (PR nemeton #668). Installée localement : 2026.11.36 → installer la
  release : `Rscript -e 'pak::pkg_install("pobsteta/nemeton@v2026.11.37")'` (depuis GitHub, **ne
  jamais toucher `/home/pascal/dev/nemeton`**).
- `R/service_inventaire_video.R:~163` `video_args_generation()` construit les arguments de
  `creer_projet_inventaire_video()` (chemins, jamais de pointeur terra : passage au worker `future`).
- Le CVAT Byte 0-255 de `build_cvat_precomputed()` est à côté du MNT :
  `.rvt_precomputed_path(mnt_path)` (`R/service_rvt.R:~179`), MNT du projet via
  `.acc_rvt_mnt_path(project_path)` (`R/service_accessibility.R`). **Ne pas** utiliser
  `generate_rvt()` / `.acc_cvat_overlay_raster()` : ils peuvent rendre un cache flottant [0,1] et
  ne doivent pas déclencher de calcul.

## Travail attendu

1. `DESCRIPTION` : `nemeton (>= 2026.11.37)` (section « Dépendances » de NEWS).
2. `video_args_generation()` : si un CVAT précalculé existe pour le projet, ajouter
   `cvat_path = <chemin>` ; sinon **ne pas passer** l'argument (aucun calcul pour l'occasion).
   Résolution du chemin dans un petit helper pur testable (ex. `.video_cvat_path(project_path)`).
3. Vérifier l'appelant (`R/mod_inventaire_video.R` ~198) : rien d'autre à câbler si l'argument
   suit la liste ; garder la compatibilité de `.video_appeler()` (si le cœur installé n'a pas
   `cvat_path`, ne pas le passer — le plancher le garantit, mais le dire dans la PR).
4. Couverture (brief) : l'emprise du cœur = union des UGF + 50 m. Ne rien recalculer ; si le CVAT
   ne couvre pas, le cœur met 0 (nodata). Le dire dans la PR, pas de code.

## Tests

Helper : projet avec `<mnt>_CVAT_8bit_foretaccess.tif` → chemin rendu ; sans CVAT → `NULL` et
`cvat_path` absent de la liste (`!"cvat_path" %in% names(args)`) ; présence → `args$cvat_path`
égal au chemin. Mutation rouge (toujours passer l'argument → rouge ; ne jamais le passer → rouge).
`test-service_inventaire_video*.R`, `test-mod_inventaire_video*.R`, `timeout 540`, fichier par
fichier. `grep -nP "[^\x00-\x7F]" R/*.R` sans ajout.

## Versionnage

`2026.11.2.9012`, entrée NEWS (`feat` → PATCH), section « Dépendances ».

## Moyens

- Homme : `dev` (sonnet), worktree depuis `origin/main`, branche `claude/042-inventaire-video-cvat`.
- Relecteur sur la PR. Temps de la main : *appuyer*.
- Terrain (Pascal) : générer un projet d'inventaire vidéo sur un projet qui a un CVAT →
  `rasters/cvat.tif` présent dans le `.qgz`.

## Statut

Terminée le 2026-11-22 : PR #314 fusionnée (`207f0d7f`, `.9012`, plancher `nemeton (>= 2026.11.37)`). Relecture : fusionnable, mineurs non bloquants. Reste l essai terrain.

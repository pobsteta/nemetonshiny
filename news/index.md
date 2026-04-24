# Changelog

## nemetonshiny 0.20.0 (2026-04-24)

#### New feature — LiDAR HD integration (E5.d)

- **LiDAR HD MNH as preferred CHM source**. The download path now tries
  `download_ign_lidar_hd(product = "mnh")` via `happign` first — a
  direct airborne measurement (~0.5 m vertical accuracy, NDP 2
  precision). Open-Canopy ML remains the fallback when LiDAR HD tiles
  are missing for the AOI.
- **LiDAR HD MNT promoted to the `dem` slot** (1 m vs 25 m BD ALTI) so
  W3 (TWI), R1 (feu), R2 (tempête), R3 (sécheresse) and the erosion risk
  all run at LiDAR HD resolution.
- **NDP 1 “Observation” auto-lifts** whenever any LiDAR HD product (MNH
  or MNT) is cached for the AOI, via
  `attr(compute_unit, "has_lidar_hd")` consumed by
  [`nemeton::detect_ndp()`](https://pobsteta.github.io/nemeton/reference/detect_ndp.html).
- **Stratified GRTS kicks in on the sampling plan**. Two new reactives
  (`chm_raster`, `mnt_raster`) load the cached CHM / MNT with the same
  LiDAR-first / fallback order and pass them to
  [`nemeton::create_sampling_plan()`](https://pobsteta.github.io/nemeton/reference/create_sampling_plan.html).
  The core upgrades from LPM2 to stratified GRTS whenever CHM + MNT + BD
  Forêt are all available. The draw method is surfaced in the generation
  toast.
- **New “Hauteur LiDAR HD” badge** on the Synthesis tab
  (`augmented_height_lidar_*` i18n keys) — green, distinct from the cyan
  “Hauteur ML” used for Open-Canopy.
- `chm_phase:lidar_hd_download` progress key translated so the compute
  status line reads “Téléchargement CHM LiDAR HD (IGN)…” instead of the
  raw key.

#### New feature — Sampling polish

- **`forest_mask` passed to the sampling plan**: reuse the project’s
  cached BD Forêt v2 polygons (filtered to true forest) so points
  falling in water, fields or roads are filtered by the
  `min_forest_cover = 0.7` constraint. Fixes the Couchey lake scenario.
- **Map zoom fixed to the UGF extent**, not BD Forêt’s (which is fetched
  with a buffer and was dominating the auto-fit).
- **Immediate toast on Générer les placettes** with a spinning gear,
  matching the Projet chargé / Retry pattern. Dispatched on the root
  session.
- **Tooltip on the Source du CV radio** explicitly states that the
  choice controls the CV value (Cochran), not the draw method (GRTS /
  LPM2 / random).
- **Sampling method note rewritten** to describe the full pipeline:
  candidates on a regular 50 m grid, filtered by the forest mask, then
  GRTS → LPM2 → random depending on what is provided.

#### Fixed

- Duplicate PostGIS-sync toast at compute completion — removed the
  second occurrence in `mod_progress`; only the `mod_home` one fires
  now.
- Immediate toast when clicking *Réessayer* on the compute-error card,
  dispatched on the root session.

#### Dependencies

- Bumped `nemeton` minimum to `>= 0.19.5` (for `height_lidar` augmented
  flag and TSP tour integration).

## nemetonshiny 0.19.0 (2026-04-24)

#### New feature — Sampling UX polish

- **Tooltips** on six sidebar inputs of the Export terrain sub-tab
  (target error, alpha risk, over-sample ratio, CV position, seed,
  region). Each tooltip explains the statistical or biological meaning
  of the parameter.
- **TSP legend on the map**: when a sampling plan with ≥ 2 Base plots is
  drawn, a legend panel appears at the bottom-left with three inline-SVG
  glyphs — dashed magenta line (*Ordre de visite*), open triangle
  (*Départ*), double concentric circle (*Arrivée*) — matching the
  markers and route on the map.
- **Retry toast**: clicking *Réessayer* now fires an immediate
  notification with a spinning arrow-clockwise icon (“Projet
  réinitialisé — prêt à relancer le calcul.”), dispatched on the root
  session so it lands in the top-level toast stack.

#### Fixed

- **Duplicate PostGIS-sync toast** on compute completion: the same
  notification used to fire both from `mod_home` and `mod_progress` with
  slightly different wording (“la base PostGIS” vs “la base de données
  PostGIS”). Kept the `mod_home` one (orchestrator), dropped the
  `mod_progress` one.
- **Package documentation icon in RStudio’s Packages pane**: added `URL`
  and `BugReports` fields to `DESCRIPTION` so the icon is now rendered
  alongside the globe and uninstall icons.

#### Docs

- README: synced counters to the real state (31 indicators, 13 expert
  profiles, 504 i18n translation keys). Previously 29 / 16 / 293.
- i18n: `sampling_tt_region` tooltip says *QGIS*, not *QField* (the
  species dropdown is defined in the QGIS project descriptor).

## nemetonshiny 0.18.0 (2026-04-24)

#### New feature — Sample size from target error + BD Forêt v2 CV (E5.c)

- **`R/mod_sampling.R`** — the sidebar accordion in the Export terrain
  sub-tab gains a *Mode de dimensionnement* radio (*Taille fixe* /
  *Erreur cible*). In *Erreur cible* mode the user picks a relative
  error (default 10 %), an alpha risk (default 5 %), an over-sample
  ratio (default 20 %) and either a manual CV or an automatic CV derived
  from BD Forêt v2 via
  [`nemeton::cv_from_bdforet()`](https://pobsteta.github.io/nemeton/reference/cv_from_bdforet.html).
  The computed sample size is shown live under the inputs, along with
  diagnostics on the BD Forêt v2 coverage and any ambiguous / unmapped
  TFV codes.
- BD Forêt v2 is read from the project cache populated during the first
  compute run (`<project>/cache/layers/bdforet.gpkg`). When the cache is
  absent, the UI points the user at the manual mode via an explicit
  warning.
- The TFV column is auto-detected (`TFV`, `tfv`, `code_tfv`) to cope
  with different WFS layouts.
- The existing *Taille fixe* path is preserved via a `conditionalPanel`;
  `create_sampling_plan()` is called with `n_base` / `n_over` computed
  upstream depending on the mode.
- Bumped nemeton dependency to `>= 0.19.0.9000` (the dev version
  introducing `compute_sample_size()`, `cv_from_bdforet()` and the
  editable CV typology CSVs).
- 19 new FR/EN i18n keys (`sampling_sizing_mode`, `sampling_mode_*`,
  `sampling_target_error_label`, `sampling_alpha_label`,
  `sampling_over_ratio_label`, `sampling_cv_source_*`,
  `sampling_cv_position*`, `sampling_cv_compute`,
  `sampling_cv_bdforet_hint`, `sampling_cv_bdforet_missing`,
  `sampling_cv_computed`, `sampling_cv_ambiguous`,
  `sampling_cv_unmapped`, `sampling_n_computed*`).
- Tests: 6 new testServer assertions covering the Cochran sizing path
  (manual CV) and the bail-out when CV is zero. Full suite 5145 / 0
  failure.

#### New feature — Field ingest (E5.b — QField return path)

- **`R/mod_field_ingest.R`** — new “Ingestion terrain” tab that closes
  the terrain → plateforme loop. A field agent drops the GeoPackage
  returned by QField; the module runs
  [`nemeton::import_qfield_gpkg()`](https://pobsteta.github.io/nemeton/reference/import_qfield_gpkg.html) +
  `validate_field_data()`, renders a validation report (counts, errors,
  warnings), and previews the placettes / arbres on the project map.
- **NDP bump on attach**: clicking *Rattacher au projet* calls
  `aggregate_plot_metrics()` + `attach_field_data_to_units()` on the
  project’s UGF sf, tags it via `tag_field_data_sources()`, runs
  `detect_ndp()` along the alternative field path (NDP 2 with plots
  only, NDP 3 from 10 trees/plot on average), persists the GPKG to
  `<project>/data/field_data.gpkg` and updates project metadata so the
  bumped NDP is picked up by every downstream module (synthesis badge,
  family tabs). Before/after NDP badges make the change visible to the
  user.
- **MVP scope**: this iteration persists the field data and bumps the
  NDP, but does not rerun `compute_all_indicators()`. The indicators
  consuming field aggregates (P1, P2, B2, C1, R2) are picked up on the
  next compute triggered from the Home tab.
- i18n: 22 new FR/EN keys (`tab_field_ingest`, `field_ingest_*`,
  `field_ingest_ndp_before` / `_after`, report headers).
- Tests: `tests/testthat/test-mod_field_ingest.R` — 24 assertions
  covering UI controls, reactive NULL state, the validate flow on a
  real-ish GPKG (placettes + arbres) and the attach flow with mocked
  persistence (GPKG written to the project dir + metadata update
  recorded).

#### Sampling module now uses the library-level GRTS pipeline

- **`R/mod_sampling.R`** — replace the temporary
  `sf::st_sample(..., type = "random")` draw with
  [`nemeton::create_sampling_plan()`](https://pobsteta.github.io/nemeton/reference/create_sampling_plan.html),
  which delivers GRTS stratification when CHM/DEM/BD Forêt layers are
  provided and falls back to spatially-balanced LPM2 or plain random
  otherwise. The notification now appends the draw method (`GRTS`,
  `LPM2`, `RANDOM`) so users can see which path was taken.
- i18n: `sampling_method_note` rewritten to describe the new behaviour.

#### New feature — Field sampling / QField export (E5.a)

- **`R/mod_sampling.R`** — new “Terrain” tab: given the current
  project’s study area (union of `indicators_sf` polygons), the user
  sets `n_base` / `n_over` / seed / biogeographic region, clicks
  *Générer*, and previews the sample plots on a leaflet map. A
  *Télécharger le projet QField (.qgz)* button produces a QField-ready
  project via
  [`nemeton::create_qfield_project()`](https://pobsteta.github.io/nemeton/reference/create_qfield_project.html)
  (placettes + empty arbres layer + pre-configured forms).
- First iteration uses a spatial random draw
  ([`sf::st_sample`](https://r-spatial.github.io/sf/reference/st_sample.html)).
  The full stratified GRTS + TSP pipeline from the 09-sampling tutorial
  will be lifted to
  [`nemeton::create_sampling_plan()`](https://pobsteta.github.io/nemeton/reference/create_sampling_plan.html)
  in a follow-up.
- `DESCRIPTION` now requires `nemeton (>= 0.18.0.9000)` for
  `create_qfield_project()`.
- i18n: 14 new FR/EN keys (`tab_sampling`, `sampling_*`, `qfield_*`).
- Tests: `tests/testthat/test-mod_sampling.R` — 23 assertions covering
  UI controls, reactive draw, empty-state handling and a round-trip .qgz
  built from the module’s generated plots.

#### Changes — F1 soil fertility

- **F1 now uses the absolute SoilGrids CEC scoring path** from the core
  package. `compute_single_indicator()` passes `source = "soilgrids"` to
  `indicateur_f1_fertilite()`, which streams the 250 m CEC topsoil
  raster on demand via
  [`nemeton::load_raster_source()`](https://pobsteta.github.io/nemeton/reference/load_raster_source.html)
  and applies
  [`nemeton::cec_to_fertility_score()`](https://pobsteta.github.io/nemeton/reference/cec_to_fertility_score.html)
  (absolute 0-100). Scores are now comparable across projects instead of
  being min-maxed per AOI.
- **Removed the duplicated `download_soilgrids_cec()`** and its entry in
  `DATA_SOURCES$rasters$soil`. The core package owns the download path
  (ADR-009), so the app no longer stages a SoilGrids layer in
  `layers$rasters$soil`. One less pre-compute step surfaces in the
  progress UI.
- **Bumped the `nemeton` dependency** to `>= 0.17.0.9000` (the dev
  version introducing `load_raster_source()`, `source = "soilgrids"`,
  and the UTS → fertility crosswalk).

## nemetonshiny 0.16.0

First release targeting the v0.17.0 nemeton core. End-to-end integration
of the Open-Canopy CHM pipeline, live per-step progress feedback, and a
consolidated i18n layer.

#### New Features — CHM / Open-Canopy

- **Auto-detected Open-Canopy CHM** — the UI no longer forces the user
  to pick “CHM: none / Open-Canopy” before every run. The pipeline fires
  automatically when the `opencanopy` package is installed, unless the
  user opts out via `options(nemetonshiny.chm = "none")` or
  `NEMETONSHINY_DISABLE_CHM=1`. Each synthesis view gets two provenance
  badges:
  - ⚡ **Hauteur ML** — CHM was consumed by height-aware indicators (C1,
    B2, R2, P2).
  - 📋 **Inventaire estimé ML** — P1 / P3 / E1 ran, meaning `dbh` /
    `density` were synthesised from the CHM via
    [`nemeton::ensure_inventory_fields()`](https://pobsteta.github.io/nemeton/reference/ensure_inventory_fields.html)
    (Charru 2012 self-thinning).
- **BD Forêt enrichment for P2** — UGFs are enriched with `species` /
  `age` from BD Forêt V2 once up-front via
  [`nemeton::enrich_parcels_bdforet()`](https://pobsteta.github.io/nemeton/reference/enrich_parcels_bdforet.html)
  when `indicateur_p2_station` is scheduled, so the CHM mode can run
  instead of falling back on the legacy `fertility` / `climate` path
  that never had its inputs.

#### New Features — progress UX

- **Live step-by-step status** replaces the “frozen on Inférence CHM”
  ~8-minute silence on large AOIs. The task toast now paints:
  - “Étape 1/5 : chargement de l’AOI…”
  - “Étape 2/5 : téléchargement ortho IGN…” + “Téléchargement ortho IGN
    RVB : tuile 5/28…” per WMS tile
  - “Étape 3/5 : configuration Python + téléchargement modèle…”
  - “Étape 4/5 : inférence du modèle pvtv2…” + “Inférence CHM : tuile
    2/3…” per inference tile
  - “Étape 5/5 : export des résultats…”
- **Initialisation spinner** — the toast paints ⚙ + “Initialisation des
  calculs…” the moment the user clicks “Lancer les calculs” or
  “Réessayer”, so the 1-3 s gap before the async worker writes its first
  progress event is no longer silent.
- **Task translator unified** — `mod_progress.R` no longer ships its own
  partial `translate_task()`; it delegates to the canonical
  `translate_task_message()` in `utils_i18n.R`, so every new task prefix
  is routed to its label in one place.

#### New Features — i18n

- **Single source of truth** — the `TRANSLATIONS` list in
  `R/utils_i18n.R` is now the only runtime dictionary. The stale
  `inst/app/i18n/{fr,en}.json` files (339 keys, 19 behind the R list)
  have been removed and the unused `shiny.i18n` suggested dependency
  dropped. `export_translations_json()` remains available for one-way R
  → JSON exports to external translators.

#### Bug Fixes

- **`download_chm_opencanopy()`** — unwraps the bare `SpatRaster` / `sf`
  returned by `download_{raster,vector}_source()` instead of chasing a
  `$object` attribute that didn’t exist. The previous code called `[[`
  on a `SpatRaster` looking for a layer named “object” and triggered a
  terra `[subset] invalid name(s)` error that aborted the whole CHM
  pipeline and forced P2 back into legacy mode.
- **Open-Canopy pipeline resume** — the retry and recompute paths now
  reset the project to “draft” and wait for the user to re- launch the
  run, instead of silently firing a new `compute_task` invocation. One
  entry point is the confirmation modal.
- **Resume from a legacy progress file** — `translate_task_message()`
  now maps the pre-`e74bdcc` literal `"download:source_chm_opencanopy"`
  to the new `"chm_inference_opencanopy"` label so re-opening an older
  project no longer spams “Translation key not found:
  source_chm_opencanopy”.
- **Tests** — `NEMETONSHINY_DISABLE_CHM=1` is now scoped to the test run
  via `withr::local_envvar(.local_envir = testthat::teardown_env())` in
  a dedicated `setup-chm.R`, so `devtools::test()` in an interactive
  session no longer leaves the CHM pipeline silently disabled.

#### Breaking changes

- None. The CHM toggle that was previously visible on the compute button
  disappeared, but the underlying metadata is still written (now
  reflecting the *outcome* of the auto-detected run, not the user’s a
  priori choice), so the synthesis badge keeps working on old projects.

## nemetonshiny 0.15.1

See git history.

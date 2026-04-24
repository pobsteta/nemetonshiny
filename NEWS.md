# nemetonshiny 0.19.0.9000 (development)

# nemetonshiny 0.18.0 (2026-04-24)

### New feature — Sample size from target error + BD Forêt v2 CV (E5.c)

* **`R/mod_sampling.R`** — the sidebar accordion in the Export
  terrain sub-tab gains a *Mode de dimensionnement* radio
  (*Taille fixe* / *Erreur cible*). In *Erreur cible* mode the user
  picks a relative error (default 10 %), an alpha risk (default 5 %),
  an over-sample ratio (default 20 %) and either a manual CV or an
  automatic CV derived from BD Forêt v2 via
  `nemeton::cv_from_bdforet()`. The computed sample size is shown
  live under the inputs, along with diagnostics on the BD Forêt v2
  coverage and any ambiguous / unmapped TFV codes.
* BD Forêt v2 is read from the project cache populated during the
  first compute run (`<project>/cache/layers/bdforet.gpkg`). When the
  cache is absent, the UI points the user at the manual mode via an
  explicit warning.
* The TFV column is auto-detected (`TFV`, `tfv`, `code_tfv`) to cope
  with different WFS layouts.
* The existing *Taille fixe* path is preserved via a
  `conditionalPanel`; `create_sampling_plan()` is called with
  `n_base` / `n_over` computed upstream depending on the mode.
* Bumped nemeton dependency to `>= 0.19.0.9000` (the dev version
  introducing `compute_sample_size()`, `cv_from_bdforet()` and the
  editable CV typology CSVs).
* 19 new FR/EN i18n keys (`sampling_sizing_mode`, `sampling_mode_*`,
  `sampling_target_error_label`, `sampling_alpha_label`,
  `sampling_over_ratio_label`, `sampling_cv_source_*`,
  `sampling_cv_position*`, `sampling_cv_compute`,
  `sampling_cv_bdforet_hint`, `sampling_cv_bdforet_missing`,
  `sampling_cv_computed`, `sampling_cv_ambiguous`,
  `sampling_cv_unmapped`, `sampling_n_computed*`).
* Tests: 6 new testServer assertions covering the Cochran sizing
  path (manual CV) and the bail-out when CV is zero. Full suite
  5145 / 0 failure.

### New feature — Field ingest (E5.b — QField return path)

* **`R/mod_field_ingest.R`** — new "Ingestion terrain" tab that closes
  the terrain → plateforme loop. A field agent drops the GeoPackage
  returned by QField; the module runs
  `nemeton::import_qfield_gpkg()` + `validate_field_data()`, renders
  a validation report (counts, errors, warnings), and previews the
  placettes / arbres on the project map.
* **NDP bump on attach**: clicking *Rattacher au projet* calls
  `aggregate_plot_metrics()` + `attach_field_data_to_units()` on the
  project's UGF sf, tags it via `tag_field_data_sources()`, runs
  `detect_ndp()` along the alternative field path (NDP 2 with plots
  only, NDP 3 from 10 trees/plot on average), persists the GPKG to
  `<project>/data/field_data.gpkg` and updates project metadata so
  the bumped NDP is picked up by every downstream module (synthesis
  badge, family tabs). Before/after NDP badges make the change
  visible to the user.
* **MVP scope**: this iteration persists the field data and bumps
  the NDP, but does not rerun `compute_all_indicators()`. The
  indicators consuming field aggregates (P1, P2, B2, C1, R2) are
  picked up on the next compute triggered from the Home tab.
* i18n: 22 new FR/EN keys (`tab_field_ingest`, `field_ingest_*`,
  `field_ingest_ndp_before` / `_after`, report headers).
* Tests: `tests/testthat/test-mod_field_ingest.R` — 24 assertions
  covering UI controls, reactive NULL state, the validate flow on a
  real-ish GPKG (placettes + arbres) and the attach flow with mocked
  persistence (GPKG written to the project dir + metadata update
  recorded).

### Sampling module now uses the library-level GRTS pipeline

* **`R/mod_sampling.R`** — replace the temporary
  `sf::st_sample(..., type = "random")` draw with
  `nemeton::create_sampling_plan()`, which delivers GRTS
  stratification when CHM/DEM/BD Forêt layers are provided and
  falls back to spatially-balanced LPM2 or plain random otherwise.
  The notification now appends the draw method (`GRTS`, `LPM2`,
  `RANDOM`) so users can see which path was taken.
* i18n: `sampling_method_note` rewritten to describe the new
  behaviour.

### New feature — Field sampling / QField export (E5.a)

* **`R/mod_sampling.R`** — new "Terrain" tab: given the current
  project's study area (union of `indicators_sf` polygons), the user
  sets `n_base` / `n_over` / seed / biogeographic region, clicks
  *Générer*, and previews the sample plots on a leaflet map. A
  *Télécharger le projet QField (.qgz)* button produces a QField-ready
  project via `nemeton::create_qfield_project()` (placettes + empty
  arbres layer + pre-configured forms).
* First iteration uses a spatial random draw (`sf::st_sample`). The
  full stratified GRTS + TSP pipeline from the 09-sampling tutorial
  will be lifted to `nemeton::create_sampling_plan()` in a follow-up.
* `DESCRIPTION` now requires `nemeton (>= 0.18.0.9000)` for
  `create_qfield_project()`.
* i18n: 14 new FR/EN keys (`tab_sampling`, `sampling_*`, `qfield_*`).
* Tests: `tests/testthat/test-mod_sampling.R` — 23 assertions
  covering UI controls, reactive draw, empty-state handling and a
  round-trip .qgz built from the module's generated plots.

### Changes — F1 soil fertility

* **F1 now uses the absolute SoilGrids CEC scoring path** from the
  core package. `compute_single_indicator()` passes
  `source = "soilgrids"` to `indicateur_f1_fertilite()`, which
  streams the 250 m CEC topsoil raster on demand via
  `nemeton::load_raster_source()` and applies
  `nemeton::cec_to_fertility_score()` (absolute 0-100). Scores are
  now comparable across projects instead of being min-maxed per
  AOI.
* **Removed the duplicated `download_soilgrids_cec()`** and its
  entry in `DATA_SOURCES$rasters$soil`. The core package owns the
  download path (ADR-009), so the app no longer stages a SoilGrids
  layer in `layers$rasters$soil`. One less pre-compute step
  surfaces in the progress UI.
* **Bumped the `nemeton` dependency** to `>= 0.17.0.9000` (the dev
  version introducing `load_raster_source()`, `source = "soilgrids"`,
  and the UTS → fertility crosswalk).

# nemetonshiny 0.16.0

First release targeting the v0.17.0 nemeton core. End-to-end
integration of the Open-Canopy CHM pipeline, live per-step
progress feedback, and a consolidated i18n layer.

### New Features — CHM / Open-Canopy

* **Auto-detected Open-Canopy CHM** — the UI no longer forces the
  user to pick "CHM: none / Open-Canopy" before every run. The
  pipeline fires automatically when the `opencanopy` package is
  installed, unless the user opts out via
  `options(nemetonshiny.chm = "none")` or
  `NEMETONSHINY_DISABLE_CHM=1`. Each synthesis view gets two
  provenance badges:
    * ⚡ **Hauteur ML** — CHM was consumed by height-aware
      indicators (C1, B2, R2, P2).
    * 📋 **Inventaire estimé ML** — P1 / P3 / E1 ran, meaning
      `dbh` / `density` were synthesised from the CHM via
      `nemeton::ensure_inventory_fields()` (Charru 2012
      self-thinning).
* **BD Forêt enrichment for P2** — UGFs are enriched with
  `species` / `age` from BD Forêt V2 once up-front via
  `nemeton::enrich_parcels_bdforet()` when
  `indicateur_p2_station` is scheduled, so the CHM mode can run
  instead of falling back on the legacy `fertility` / `climate`
  path that never had its inputs.

### New Features — progress UX

* **Live step-by-step status** replaces the "frozen on Inférence
  CHM" ~8-minute silence on large AOIs. The task toast now paints:
    * "Étape 1/5 : chargement de l'AOI…"
    * "Étape 2/5 : téléchargement ortho IGN…" +
      "Téléchargement ortho IGN RVB : tuile 5/28…" per WMS tile
    * "Étape 3/5 : configuration Python + téléchargement modèle…"
    * "Étape 4/5 : inférence du modèle pvtv2…" +
      "Inférence CHM : tuile 2/3…" per inference tile
    * "Étape 5/5 : export des résultats…"
* **Initialisation spinner** — the toast paints ⚙ + "Initialisation
  des calculs…" the moment the user clicks "Lancer les calculs"
  or "Réessayer", so the 1-3 s gap before the async worker writes
  its first progress event is no longer silent.
* **Task translator unified** — `mod_progress.R` no longer ships
  its own partial `translate_task()`; it delegates to the canonical
  `translate_task_message()` in `utils_i18n.R`, so every new task
  prefix is routed to its label in one place.

### New Features — i18n

* **Single source of truth** — the `TRANSLATIONS` list in
  `R/utils_i18n.R` is now the only runtime dictionary. The stale
  `inst/app/i18n/{fr,en}.json` files (339 keys, 19 behind the R
  list) have been removed and the unused `shiny.i18n` suggested
  dependency dropped. `export_translations_json()` remains
  available for one-way R → JSON exports to external translators.

### Bug Fixes

* **`download_chm_opencanopy()`** — unwraps the bare `SpatRaster`
  / `sf` returned by `download_{raster,vector}_source()` instead
  of chasing a `$object` attribute that didn't exist. The previous
  code called `[[` on a `SpatRaster` looking for a layer named
  "object" and triggered a terra `[subset] invalid name(s)` error
  that aborted the whole CHM pipeline and forced P2 back into
  legacy mode.
* **Open-Canopy pipeline resume** — the retry and recompute paths
  now reset the project to "draft" and wait for the user to re-
  launch the run, instead of silently firing a new `compute_task`
  invocation. One entry point is the confirmation modal.
* **Resume from a legacy progress file** — `translate_task_message()`
  now maps the pre-`e74bdcc` literal
  `"download:source_chm_opencanopy"` to the new
  `"chm_inference_opencanopy"` label so re-opening an older
  project no longer spams "Translation key not found:
  source_chm_opencanopy".
* **Tests** — `NEMETONSHINY_DISABLE_CHM=1` is now scoped to the
  test run via `withr::local_envvar(.local_envir =
  testthat::teardown_env())` in a dedicated `setup-chm.R`, so
  `devtools::test()` in an interactive session no longer leaves
  the CHM pipeline silently disabled.

### Breaking changes

* None. The CHM toggle that was previously visible on the compute
  button disappeared, but the underlying metadata is still written
  (now reflecting the *outcome* of the auto-detected run, not the
  user's a priori choice), so the synthesis badge keeps working on
  old projects.

# nemetonshiny 0.15.1

See git history.

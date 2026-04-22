# nemetonshiny (development version)

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

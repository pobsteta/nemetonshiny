# Changelog

All notable changes to this project are documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

For a narrative, per-feature description of each release, see
[NEWS.md](NEWS.md). This file is the concise, categorised trail.

## [Unreleased]

## [0.19.0] - 2026-04-24

### Added

- Tooltips on six sidebar inputs of the Export terrain sub-tab
  (target error, alpha risk, over-sample ratio, CV position, seed,
  region).
- Custom TSP legend on the leaflet map (inline-SVG glyphs for the
  route, start and finish).
- Immediate toast notification when clicking *Réessayer* on the
  compute-error card, dispatched on the root session.
- `URL` and `BugReports` fields in `DESCRIPTION` so the RStudio
  Packages pane shows the documentation icon next to the package.

### Changed

- README counters synced to the real state (31 indicators, 13
  expert profiles, 504 i18n keys).
- `sampling_tt_region` tooltip wording says QGIS, not QField.

### Fixed

- Duplicate PostGIS-sync toast at compute completion — removed the
  second occurrence in `mod_progress`; the `mod_home` one remains.

## [0.18.0] - 2026-04-24

### Added

- **Terrain top-level tab** with two sub-tabs via
  `bslib::navset_card_underline()`:
  - *Export terrain* — design a sampling plan, render a leaflet map
    with the BD Forêt v2 overlay (coloured by sylvicultural
    context) + the UGF polygons + the placettes, export a QField
    `.qgz` project.
  - *Import terrain* — ingest a GeoPackage returned by QField,
    validate it, attach aggregates to the project and bump the
    NDP.
- **Sampling sizing modes** in the Export terrain sidebar:
  fixed-size (legacy path) or *target error + CV* (new).
  The CV source can be manual, or derived automatically from the
  project's cached BD Forêt v2 layer via
  `nemeton::cv_from_bdforet()`. The computed sample size, Student
  quantile and ambiguous / unmapped TFV codes are displayed live.
- **TSP route on the map** — dashed magenta polyline connecting
  Base plots in `visit_order`, with inline-SVG orienteering
  symbols (open triangle for Départ, double concentric circle for
  Arrivée).
- **BD Forêt v2 overlay** coloured by resolved forest context
  (futaie régulière résineuse / feuillue, futaie irrégulière, TSF,
  taillis simple) with a toggleable layer control.
- **Field ingest module** (`R/mod_field_ingest.R`, E5.b) — closes
  the QField return loop: validate, aggregate, attach, persist to
  `<project>/data/field_data.gpkg`, update metadata, bump the NDP,
  reload the project.
- **Sampling export module** (`R/mod_sampling.R`, E5.a) — UI +
  `downloadHandler` producing a QField-ready `.qgz`.
- **Package-level help** (`R/nemetonshiny-package.R`) so
  `?nemetonshiny` works and RStudio shows the documentation icon
  in the Packages pane.
- `CITATION.cff` and `CHANGELOG.md` — release-metadata files.

### Changed

- `mod_sampling` now uses `nemeton::create_sampling_plan()`
  (GRTS / LPM2 / random) instead of a plain `sf::st_sample()`.
  The generation notification reports the selected method.
- The Export terrain map now draws per-UGF polygons (matching the
  Import terrain style) instead of a single unioned zone; the
  unioned zone is still used internally by
  `create_sampling_plan()`.
- Sidebar forms in mod_sampling and mod_field_ingest are wrapped
  in Bootstrap collapsible cards (same pattern as the
  "Informations projet" accordion in the Selection tab).
- `default_project_name` reactive — the QField project name input
  pre-fills with the sanitised current-project name, falling back
  to its id or `"echantillon"`.
- Renamed the "Inventaire estimé ML" badge in the Synthesis tab
  to "Inventaire ML"; both augmented-NDP tooltips now prefix
  "ML = Machine Learning" for discoverability.
- Renamed the QField download button from
  "Télécharger le projet QField (.qgz)" to
  "Télécharger le projet QGIS".
- Shortened the CV-compute button label from
  "Calculer le CV depuis BD Forêt v2" to "Calculer le CV".
- Bumped the `nemeton` dependency to `>= 0.19.0`.

### Fixed

- BD Forêt v2 mapping diagnostics: the sizing report now lists the
  actual ambiguous and unmapped TFV codes (with libellé, resolved
  context and alternative) instead of a bare count.
- TFV column auto-detection in `mod_sampling` widened to
  `TFV / tfv / CODE_TFV / code_tfv / essence / ESSENCE / LIB_FV /
  LIBELLE`.

## Prior versions

See [NEWS.md](NEWS.md) for the complete narrative history
(0.1.0 onwards).

[Unreleased]: https://github.com/pobsteta/nemetonshiny/compare/v0.19.0...HEAD
[0.19.0]: https://github.com/pobsteta/nemetonshiny/compare/v0.18.0...v0.19.0
[0.18.0]: https://github.com/pobsteta/nemetonshiny/compare/v0.16.0...v0.18.0

# Changelog

All notable changes to this project are documented in this file.

The format is based on [Keep a
Changelog](https://keepachangelog.com/en/1.1.0/), and this project
adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

For a narrative, per-feature description of each release, see
[NEWS.md](https://pobsteta.github.io/nemetonshiny/NEWS.md). This file is
the concise, categorised trail.

## [Unreleased](https://github.com/pobsteta/nemetonshiny/compare/v0.20.0...HEAD)

## \[0.24.8\] - 2026-05-12

### Added

- Progression “X/N tuiles Sentinel-2 téléchargées” pendant l’ingestion
  FAST, et “FORDEAD — phase : %s (X/N)” pendant le diagnostic santé.
  Couplé à `nemeton@v0.21.2` qui introduit `progress_callback` sur
  `ingest_sentinel2_timeseries()` et `run_fordead_dieback()`. Le worker
  async écrit un `<project>/data/{ingest,fordead}_progress.json`
  atomique, le main process polle via `reactivePoll(500ms)` et rend un
  toast persistant remplacé à chaque tick.
- 4 nouvelles clés i18n FR/EN : `monitoring_ingest_progress_fmt`,
  `monitoring_ingest_progress_named_fmt`, `monitoring_health_phase_fmt`,
  `monitoring_health_phase_simple_fmt`.

### Fixed

- Boutons **“Lancer le diagnostic FAST”** et **“Lancer le diagnostic
  FORDEAD”** muets au clic. Cause : `tagAppendAttributes(disabled = NA)`
  HTML-disablait les boutons au premier rendu, et la classe
  `btn-primary` masque visuellement l’état `disabled` Bootstrap — le
  navigateur refuse le clic alors que l’utilisateur croit le bouton
  actif. Correctif : suppression du wrapper `disabled = NA`,
  simplification du `updateActionButton(disabled = is_running)` (greying
  uniquement pendant la tâche async), garde `is_running` en tête des
  `observeEvent` pour avaler les double-clics.

### Changed

- `DESCRIPTION`: `Imports: nemeton (>= 0.21.2)`,
  `Remotes: pobsteta/nemeton@v0.21.2`.

## \[0.24.7\] - 2026-05-12

### Fixed

- Migration de la base DuckDB du Suivi sanitaire : bump de `nemeton`
  vers `v0.21.1` (DDL portable Postgres/DuckDB via `CREATE SEQUENCE` +
  `DEFAULT nextval(...)`, remplace
  `INTEGER PRIMARY KEY GENERATED ALWAYS AS IDENTITY` qui cassait DuckDB
  avec *“syntax error at or near GENERATED”*).
- `DESCRIPTION`: `Imports: nemeton (>= 0.21.1)`,
  `Remotes: pobsteta/nemeton@v0.21.1`.

## \[0.23.5\] - 2026-05-09

### Added

- Plan d’actions chat: two new controls just below the history — **scope
  radio** (all UGFs / current selection) and **overwrite checkbox**
  (replace existing actions). Same semantics as the “Generate actions
  (AI)” modal. When overwrite is on, the apply modal surfaces a warning
  banner listing the number of targeted UGFs.
- New i18n keys `action_plan_chat_scope_sel` and
  `action_plan_chat_apply_overwrite_warn_fmt`.

### Fixed

- Language toggle FR ↔︎ EN in the navbar selector now actually applies.
  Two combined bugs:
  - The handler wrote to `nemeton.app_language` but `app_ui` reads
    `getOption("nemeton.app_options")$language`, so the choice did not
    survive a page reload. Now persists into the right option key.
  - The handler showed a toast saying “Reload the page” without actually
    reloading. Replaced by `session$reload()` so the UI rebuilds
    automatically. Anti-init guard: if the new value equals
    `app_state$language`, the observer returns early to avoid an
    unwanted reload at session start.

### Removed

- Orphaned i18n key `language_changed` (was only used by the dropped
  manual-reload toast).

## \[0.23.4\] - 2026-05-09

### Changed

- Plan d’actions chat: role labels in the conversation history now go
  through i18n. The raw LLM keys (“user” / “assistant”) no longer
  surface in the UI; they render as **“Vous”** / **“Assistant”** (FR) or
  **“You”** / **“Assistant”** (EN), switching live with the language
  toggle. The underlying data model still uses the English keys so the
  prompt builder is unchanged.

### Added

- New i18n keys `action_plan_chat_role_user` and
  `action_plan_chat_role_assistant`.

## \[0.23.3\] - 2026-05-09

### Added

- Plan d’actions chat: clicking **Send** now displays a persistent
  **bottom-right toast** with a **spinning gear icon** and the label
  *“L’IA réfléchit…”* / *“AI is thinking…”*. The toast stays visible
  until the LLM response arrives or the call fails. Implemented via
  `shiny::showNotification(duration = NULL, closeButton = FALSE)` paired
  with an `on.exit(removeNotification(...))` hook so every return path
  (success, LLM error, parse error) clears the toast.
- New i18n key `action_plan_chat_thinking`.

## \[0.23.2\] - 2026-05-09

### Added

- Plan d’actions: chat history **auto-scrolls to the bottom** on every
  update so the latest message is always visible. Implemented via an
  inline `setTimeout(0)` script appended to each `chat_history_ui`
  render that sets `el.scrollTop = el.scrollHeight` on the
  `.chat-history` div (now carrying a stable id).

### Changed

- Plan d’actions: chat panel **moves from a left sidebar to the right
  sidebar**, sitting below the “Tableau des actions” panel. The nested
  [`bslib::layout_sidebar`](https://rstudio.github.io/bslib/reference/sidebar.html)
  introduced in 0.23.1 is replaced by a single right sidebar containing
  both cards stacked top-to-bottom.
- Plan d’actions: button label “Générer (IA)” renamed to “Générer les
  actions (IA)” (FR) / “Generate actions (AI)” (EN) for clarity.

## \[0.23.1\] - 2026-05-09

### Added

- Plan d’actions: **AI chat now lives in a persistent left sidebar**
  (350 px, collapsible) instead of a modal. The conversation stays
  visible while the user navigates map / table / Kanban. Layout switches
  to a nested `layout_sidebar` (left chat / right action panel / main
  content).

### Changed

- Plan d’actions: “Ouvrir le chat” button removed from the right action
  panel (made redundant by the persistent sidebar). `input$open_chat`
  observer (~30 LOC of `showModal`) dropped.

### Fixed

- Plan d’actions map ↔︎ table sync: clicking a parcelle on the **map**
  now selects every corresponding row in the table. The
  `input$map_shape_click` handler now also calls
  [`DT::selectRows()`](https://rdrr.io/pkg/DT/man/proxy.html). The
  reverse direction (table → map) was already working. No reactive loop:
  `reactiveVal` dedupes by
  [`identical()`](https://rdrr.io/r/base/identical.html) so the
  round-trip stops after one pass.

### Removed

- i18n keys `action_plan_open_chat` and `action_plan_chat_input_label`
  (orphaned by the chat refactor).

## \[0.23.0\] - 2026-05-09

### Added

- Kanban: double-click on a card opens an **edit modal** pre-filled with
  statut / priorité / année / commentaire. Primary use-case is editing
  long commentaires (DT inline cell-edit is single-line). Delegated
  dblclick listener at the board level with cleanup between renders.
- Kanban cards: each card now displays the **commentaire** under the
  type/year/UGF block when non-empty.
- Kanban columns: cards are **sorted by `annee_realisation`** ascending
  (NAs last) so each column reads chronologically.

### Changed

- Kanban: **free movement between any columns**. The proposée → validée
  → planifiée → réalisée → abandonnée DAG no longer gates drag-drop.
  `update_action_in_plan()` accepts any known status, rejects only
  unknown strings. `is_valid_status_transition()` and
  `ACTION_PLAN_TRANSITIONS` stay as informational documentation of the
  natural workflow.
- Kanban: per-card **“Déplacer”** dropdown removed (made redundant by
  free drag-drop). The `kanban_move_*` dispatcher observer (~50 LOC) and
  the unused `KANBAN_STATUSES` constant are gone too.
- Action plan table: **action count** moved from bottom-left to
  bottom-right. DT `dom` switched to a custom flex layout
  (`<"top"f>rt<"… dt-bottom-row"<"… "lp>i>`) with scoped CSS rules to
  override the default DT floats.
- Add action modal: the **UGF dropdown** now shows `ug_label` (sorted)
  instead of the raw `ug_id`; **Année cible** is now a real calendar
  year (default `base_year + 1`, range `base_year + 1` …
  `base_year + horizon`), converted to the internal offset on save.

### Fixed

- Add action modal: previously surfaced the internal offset (1, 2, …)
  for “Année cible” and the raw `ug_id` for the UGF dropdown, both
  confusing for end-users.

### Removed

- i18n keys `action_plan_kanban_move` and
  `action_plan_kanban_drop_invalid_fmt` (orphaned by the Kanban
  refactor).

## \[0.22.4\] - 2026-05-09

### Changed

- Action plan table: the page-size selector (“Afficher 5/10/25/50/All”)
  moved **below the table**, next to the info count and pagination. Top
  of the table now only carries the global search box. DT `dom` switched
  from `"lfrtip"` to `"frtilp"`.
- Action plan table: only **UGF + Année** are frozen during horizontal
  scroll. `DISPLAY_COLS` reordered so hidden columns (`id`, `ug_id`,
  `annee_cible`) sit at the tail; `fixedColumns.leftColumns` reduced
  from 5 to 2 to match the count of visible frozen columns (DT’s
  FixedColumns counts every DOM column, hidden included).
- Action plan map: leaflet legend titles now translated. `legend_title`
  literals (`"annee"`, `"type"`, `"priority"`) swapped for
  `i18n$t("action_plan_col_*")` so the map shows “Année / Type /
  Priorité” in FR and “Year / Type / Priority” in EN, switching with the
  language toggle.

### Fixed

- `mod_auth_server()` no longer crashes on startup in anonymous mode
  when `NEMETON_AUTH_DEV_ROLES` is set. The dev-roles branch
  interpolated `{auth_state$user_roles}` through
  [`cli::cli_alert_info()`](https://cli.r-lib.org/reference/cli_alert.html)
  / glue outside any reactive consumer, which `reactiveValues` rejects.
  The parsed roles are now captured in a local `parsed_roles` before
  being assigned to `auth_state$user_roles`; the log message reads the
  local instead of the reactiveValues. Regression introduced by \#41 in
  v0.22.3.

## [0.20.0](https://github.com/pobsteta/nemetonshiny/compare/v0.19.0...v0.20.0) - 2026-04-24

### Added

- LiDAR HD MNH fetched via `happign` as the preferred CHM source
  (Open-Canopy ML retained as fallback). E5.d phase 1.
- LiDAR HD MNT promoted to the canonical `dem` slot (1 m vs 25 m BD
  ALTI) so W3 / R1 / R2 / R3 / erosion use LiDAR resolution.
- `has_lidar_hd` attribute auto-lifts NDP to 1 when any LiDAR HD product
  is cached.
- New “Hauteur LiDAR HD” badge on the Synthesis tab, distinct from
  “Hauteur ML”.
- Reactive loaders for cached CHM / MNT in mod_sampling; passed to
  `create_sampling_plan()` so stratified GRTS kicks in.
- `forest_mask` passed to the sampling plan (BD Forêt v2 filtered) —
  points stop falling in water / roads.
- Immediate spinning-gear toast when clicking *Générer les placettes*.
- Tooltip on *Source du CV* radio clarifying that it only picks the CV
  value, not the draw method.

### Changed

- Sampling-method note rewritten to describe candidates on a regular 50
  m grid, forest mask filter, then GRTS → LPM2 → random selection.
- Map auto-zoom fixed to the UGF extent instead of BD Forêt’s (which is
  fetched with a buffer).
- `chm_phase:lidar_hd_download` progress key translated.
- Bumped `nemeton` minimum to `>= 0.19.5`.

### Fixed

- Duplicate PostGIS-sync toast at compute completion (kept the
  `mod_home` one, dropped the `mod_progress` one).
- Retry button now emits an immediate toast on the root session.

## [0.19.0](https://github.com/pobsteta/nemetonshiny/compare/v0.18.0...v0.19.0) - 2026-04-24

### Added

- Tooltips on six sidebar inputs of the Export terrain sub-tab (target
  error, alpha risk, over-sample ratio, CV position, seed, region).
- Custom TSP legend on the leaflet map (inline-SVG glyphs for the route,
  start and finish).
- Immediate toast notification when clicking *Réessayer* on the
  compute-error card, dispatched on the root session.
- `URL` and `BugReports` fields in `DESCRIPTION` so the RStudio Packages
  pane shows the documentation icon next to the package.

### Changed

- README counters synced to the real state (31 indicators, 13 expert
  profiles, 504 i18n keys).
- `sampling_tt_region` tooltip wording says QGIS, not QField.

### Fixed

- Duplicate PostGIS-sync toast at compute completion — removed the
  second occurrence in `mod_progress`; the `mod_home` one remains.

## [0.18.0](https://github.com/pobsteta/nemetonshiny/compare/v0.16.0...v0.18.0) - 2026-04-24

### Added

- **Terrain top-level tab** with two sub-tabs via
  [`bslib::navset_card_underline()`](https://rstudio.github.io/bslib/reference/navset.html):
  - *Export terrain* — design a sampling plan, render a leaflet map with
    the BD Forêt v2 overlay (coloured by sylvicultural context) + the
    UGF polygons + the placettes, export a QField `.qgz` project.
  - *Import terrain* — ingest a GeoPackage returned by QField, validate
    it, attach aggregates to the project and bump the NDP.
- **Sampling sizing modes** in the Export terrain sidebar: fixed-size
  (legacy path) or *target error + CV* (new). The CV source can be
  manual, or derived automatically from the project’s cached BD Forêt v2
  layer via
  [`nemeton::cv_from_bdforet()`](https://pobsteta.github.io/nemeton/reference/cv_from_bdforet.html).
  The computed sample size, Student quantile and ambiguous / unmapped
  TFV codes are displayed live.
- **TSP route on the map** — dashed magenta polyline connecting Base
  plots in `visit_order`, with inline-SVG orienteering symbols (open
  triangle for Départ, double concentric circle for Arrivée).
- **BD Forêt v2 overlay** coloured by resolved forest context (futaie
  régulière résineuse / feuillue, futaie irrégulière, TSF, taillis
  simple) with a toggleable layer control.
- **Field ingest module** (`R/mod_field_ingest.R`, E5.b) — closes the
  QField return loop: validate, aggregate, attach, persist to
  `<project>/data/field_data.gpkg`, update metadata, bump the NDP,
  reload the project.
- **Sampling export module** (`R/mod_sampling.R`, E5.a) — UI +
  `downloadHandler` producing a QField-ready `.qgz`.
- **Package-level help** (`R/nemetonshiny-package.R`) so
  [`?nemetonshiny`](https://pobsteta.github.io/nemetonshiny/reference/nemetonshiny-package.md)
  works and RStudio shows the documentation icon in the Packages pane.
- `CITATION.cff` and `CHANGELOG.md` — release-metadata files.

### Changed

- `mod_sampling` now uses
  [`nemeton::create_sampling_plan()`](https://pobsteta.github.io/nemeton/reference/create_sampling_plan.html)
  (GRTS / LPM2 / random) instead of a plain
  [`sf::st_sample()`](https://r-spatial.github.io/sf/reference/st_sample.html).
  The generation notification reports the selected method.
- The Export terrain map now draws per-UGF polygons (matching the Import
  terrain style) instead of a single unioned zone; the unioned zone is
  still used internally by `create_sampling_plan()`.
- Sidebar forms in mod_sampling and mod_field_ingest are wrapped in
  Bootstrap collapsible cards (same pattern as the “Informations projet”
  accordion in the Selection tab).
- `default_project_name` reactive — the QField project name input
  pre-fills with the sanitised current-project name, falling back to its
  id or `"echantillon"`.
- Renamed the “Inventaire estimé ML” badge in the Synthesis tab to
  “Inventaire ML”; both augmented-NDP tooltips now prefix “ML = Machine
  Learning” for discoverability.
- Renamed the QField download button from “Télécharger le projet QField
  (.qgz)” to “Télécharger le projet QGIS”.
- Shortened the CV-compute button label from “Calculer le CV depuis BD
  Forêt v2” to “Calculer le CV”.
- Bumped the `nemeton` dependency to `>= 0.19.0`.

### Fixed

- BD Forêt v2 mapping diagnostics: the sizing report now lists the
  actual ambiguous and unmapped TFV codes (with libellé, resolved
  context and alternative) instead of a bare count.
- TFV column auto-detection in `mod_sampling` widened to
  `TFV / tfv / CODE_TFV / code_tfv / essence / ESSENCE / LIB_FV / LIBELLE`.

## Prior versions

See [NEWS.md](https://pobsteta.github.io/nemetonshiny/NEWS.md) for the
complete narrative history (0.1.0 onwards).

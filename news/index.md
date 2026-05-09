# Changelog

## nemetonshiny 0.23.2 (2026-05-09)

#### Plan d’actions — chat dans la sidebar droite + auto-scroll

- `feat(action_plan)` — le **chat IA** rejoint la sidebar droite **sous
  la carte “Tableau des actions”**, plutôt que dans une sidebar gauche
  dédiée. Disposition haut → bas de la sidebar droite : *Tableau des
  actions* (Sélection / IA / Manuel / Exports) puis *Affiner le plan
  avec l’IA* (historique scrollable + textarea + boutons Effacer /
  Envoyer). Le double
  [`bslib::layout_sidebar`](https://rstudio.github.io/bslib/reference/sidebar.html)
  imbriqué introduit en v0.23.1 disparaît, on revient à un seul
  `layout_sidebar` avec les deux cartes empilées dans la sidebar droite
  — meilleure densité d’information sur les écrans portables.
- `feat(action_plan)` — la zone d’historique du chat **scrolle
  automatiquement vers le bas** à chaque mise à jour. La div
  `.chat-history` reçoit un `id` stable (`ns("chat_history")`) ;
  `output$chat_history_ui` injecte à la fin de chaque render un
  `tags$script` qui fait
  `setTimeout(function(){ el.scrollTop = el.scrollHeight; }, 0)` — le
  `setTimeout(0)` diffère au prochain tick pour que le DOM soit à jour
  quand `scrollHeight` est lu. Le dernier message reste visible sans
  intervention manuelle.

#### Plan d’actions — libellé bouton

- `i18n(action_plan)` — bouton **“Générer (IA)”** renommé **“Générer les
  actions (IA)”** (FR) / **“Generate actions (AI)”** (EN), pour
  clarifier que la cible est bien la génération du plan d’actions et pas
  autre chose (ex. : un rapport).

## nemetonshiny 0.23.1 (2026-05-09)

#### Plan d’actions — chat IA en sidebar gauche

- `feat(action_plan)` — le **chat IA** quitte la modal et s’installe
  dans une **sidebar gauche persistante** de 350 px. Carte collapsible
  (header `bg-info` avec icône `chat-dots`), historique scrollable
  (max-height 50 vh, min-height 160 px, fond gris clair),
  `textAreaInput` 3 lignes resize=vertical, boutons *Effacer* /
  *Envoyer* en flex. La conversation reste visible pendant que
  l’utilisateur navigue map / table / Kanban — auparavant un bouton
  “Ouvrir le chat” déclenchait un modal qui se fermait à chaque
  interaction.
- `refactor(action_plan)` — le bouton **“Ouvrir le chat”** dans la
  section IA de la sidebar droite est retiré (doublon avec le panel
  persistant). L’observer `input$open_chat` (~30 LOC qui faisait
  `showModal()`) supprimé. Layout passe à un double
  [`bslib::layout_sidebar`](https://rstudio.github.io/bslib/reference/sidebar.html)
  imbriqué : sidebar gauche (chat) → sidebar droite (action panel) →
  contenu principal. Les deux sidebars se collapsent indépendamment via
  le bouton bslib en bordure (utile sur écran portable).

#### Plan d’actions — sync carte ↔︎ tableau

- `fix(action_plan)` — clic sur une parcelle dans la **carte**
  sélectionne maintenant **toutes les lignes correspondantes** dans le
  tableau. Le handler `input$map_shape_click` (le toggle qui mettait à
  jour `selected_ug_rv` et la couche orange Selection) ne propageait pas
  la sélection à `DT` ; un appel
  `DT::selectRows(proxy, which(df$ug_id %in% cur))` est ajouté. Le sens
  table → carte (déjà fonctionnel via l’observer
  `input$action_table_rows_selected`) reste inchangé. Pas de boucle
  réactive : `reactiveVal` dedupe par
  [`identical()`](https://rdrr.io/r/base/identical.html) donc le
  round-trip map → selected_ug_rv → table → reverse-observer →
  selected_ug_rv s’arrête au 2e pas.

#### Removed

- i18n: `action_plan_open_chat`, `action_plan_chat_input_label`
  (orphelines après la refonte du chat en sidebar).

## nemetonshiny 0.23.0 (2026-05-09)

#### Plan d’actions — Kanban libre + édition par double-clic

- `feat(action_plan)` — **drag-and-drop libre entre toutes les colonnes
  du Kanban**. La sémantique du DAG (proposée → validée → planifiée →
  réalisée → abandonnée) qui empêchait certaines transitions disparaît :
  un utilisateur peut désormais déplacer n’importe quelle fiche vers
  n’importe quelle colonne. Le service `update_action_in_plan()`
  n’enforce plus le DAG ; il valide uniquement que le nouveau statut
  figure dans `ACTION_PLAN_TRANSITIONS`. La fonction
  `is_valid_status_transition()` reste exportée comme documentation du
  flux naturel mais ne gate plus les writes.
- `feat(action_plan)` — **double-clic sur une fiche Kanban ouvre une
  modal d’édition** pré-remplie avec les valeurs courantes (statut,
  priorité, année calendaire, commentaire). Le commentaire est éditable
  en `textAreaInput` 6 lignes, c’est le cas d’usage principal de la
  modal — l’édition inline du tableau DT est single-line et restait peu
  pratique pour des commentaires longs. Le handler `dblclick` est
  délégué au niveau du board (un seul listener pour toutes les cartes),
  avec cleanup entre re-renders pour éviter les fuites.
- `refactor(action_plan)` — le bouton **“Déplacer”** dans la dropdown de
  chaque carte Kanban est supprimé : avec le drag-drop libre il faisait
  doublon. ~50 lignes d’observer dispatcher `kanban_move_*` retirées en
  conséquence. La constante `KANBAN_STATUSES` (uniquement utilisée par
  la dropdown) retirée également.
- `feat(action_plan)` — **tri chronologique des cartes par colonne** :
  dans chaque statut Kanban, les fiches sont triées par
  `annee_realisation` ascendante (NAs en queue), pour qu’une colonne se
  lise du plus tôt au plus tard du haut vers le bas.
- `feat(action_plan)` — **commentaire affiché sur chaque carte Kanban**.
  Une div `.kanban-card-comment` (small text-muted, mt-1, word-break)
  apparaît sous le bloc type/année/UGF si le commentaire est non-vide ;
  rien si vide pour préserver la hauteur minimale.

#### Plan d’actions — fiche d’ajout d’action

- `fix(action_plan)` — la dropdown **UGF** dans la modal “Ajouter une
  action” affichait le `ug_id` brut (ex. `ugf_42`) au lieu du libellé
  humain. Construction d’un `ug_choices` via `setNames(ids, labels)` à
  partir de `sf$label` mappé sur `sf$ug_id`, trié par label. Fallback
  sur les IDs si `ug_sf_4326()` est indisponible.
- `fix(action_plan)` — le champ **Année cible** de la même modal
  affichait l’offset interne (1, 2, 3 …) au lieu d’une année calendaire.
  Le `numericInput` montre désormais l’année réelle (default =
  `base_year + 1`, min = `base_year + 1`, max = `base_year + horizon`) ;
  la conversion en offset (`year - base_year`) se fait au moment du save
  dans l’observer `add_run`.

#### Plan d’actions — UX du tableau

- `ui(action_plan)` — **total des actions affiché en bas à droite** du
  tableau DT (auparavant à gauche). Le `dom` DT passe à un layout custom
  `<"top"f>rt<"d-flex … dt-bottom-row"<"d-flex gap-3 align-items-center"lp>i>`
  ; règles CSS scoped sur `.dt-bottom-row` neutralisent les `float`/
  `clear` par défaut de `dataTables_info`/`_length`/`_paginate` et
  alignent l’info à droite via `text-align: right`.

## nemetonshiny 0.22.4 (2026-05-09)

#### Plan d’actions — UX polish

- `ui(action_plan)` — le sélecteur **“Afficher 5/10/25/50/All”** passe
  **sous le tableau** (à côté de l’info “*TOTAL* action(s)” et de la
  pagination Préc./Suiv.). Le `dom` DT passe de `"lfrtip"` à `"frtilp"`
  : la barre de recherche reste seule au-dessus du tableau, ce qui aère
  l’en-tête de la card.
- `ui(action_plan)` — **figeage strict** des colonnes UGF + Année lors
  du scroll horizontal. `DISPLAY_COLS` réordonné pour placer les
  colonnes cachées (`id`, `ug_id`, `annee_cible`) en queue ;
  `fixedColumns: leftColumns` passe de 5 à 2 — l’extension
  `FixedColumns` de DT compte toutes les colonnes du DOM (y compris
  `visible:FALSE`), donc seul le décompte sur les colonnes visibles
  évite les artefacts d’en-têtes clones. `colname_map` et
  `hidden_targets` ajustés en conséquence (cibles 13-15).
- `i18n(action_plan)` — titres de légende de la **carte des actions**
  traduits. Les littéraux `"annee"` / `"type"` / `"priority"` passés à
  `leaflet::addLegend(title = …)` sont remplacés par
  `i18n$t("action_plan_col_annee" | "_type" | "_priorite")` ;
  ré-utilisation des clés des en-têtes de colonnes pour rester cohérent
  avec le tableau. Affiche désormais “Année” / “Type” / “Priorité” en FR
  (et “Year” / “Type” / “Priority” en EN), avec switch dynamique au
  changement de langue.

#### Auth — fix démarrage anonyme avec dev roles

- `fix(auth)` — `mod_auth_server()` plantait au démarrage en mode
  anonyme dès que `NEMETON_AUTH_DEV_ROLES` était défini : l’appel
  `cli::cli_alert_info("…dev roles: {.val {auth_state$user_roles}}.")`
  faisait évaluer `{auth_state$user_roles}` par `glue` hors d’un
  `reactive()/observe()`, ce que `reactiveValues` interdit (“Can’t
  access reactive value ‘user_roles’ outside of reactive consumer”). La
  valeur parsée est désormais capturée dans une locale `parsed_roles`
  avant l’assignation à `auth_state$user_roles` ; le message `cli`
  interpole la locale, plus le reactiveValues. Régression introduite par
  [\#41](https://github.com/pobsteta/nemetonshiny/issues/41) (v0.22.3).

## nemetonshiny 0.22.3 (2026-05-09)

#### Plan d’actions — UX polish

- `feat(action_plan)` — global DT search box now uses **regex with OR
  semantics**
  ([\#35](https://github.com/pobsteta/nemetonshiny/issues/35)). Typing
  `eclaircie|plantation` in the search box returns rows matching either
  term. Search is case-insensitive (`caseInsensitive = TRUE`) so
  accent-less typing keeps working.
- `feat(action_plan)` — new **Surface totale** badge in the totals strip
  above the action table
  ([\#36](https://github.com/pobsteta/nemetonshiny/issues/36), plus
  reorder in this release). Sums `surface_ha` over the rows currently
  visible in the DT, formatted with two decimals + `ha`, rendered in
  `text-primary`. The badge sits **after Bilan** so the monetary totals
  (Coût / Revenu / Bilan) read first and the surface tally is the
  trailing metric. The `pill()` helper now accepts optional `unit` and
  `digits` arguments (defaulting to `"EUR"` / `0`) for backward-compat
  with the three monetary pills.
- `refactor(action_plan)` — DT table paginated at **5 rows per page**
  (was 50 with a 60 vh internal scroll). New `lengthMenu` lets users
  expand to 10 / 25 / 50 / All on demand; `dom` switched from `"frtip"`
  to `"lfrtip"` so the length selector sits left of the global search.
  Removed `scrollY` + `scrollCollapse` so the table card now contracts
  around the visible rows instead of padding to 60 vh. `scrollX = TRUE`
  and the two pinned columns (UGF + Année) are unchanged.
- `ui(action_plan)` — sidebar title now carries an icon and reads
  “Tableau des actions”
  ([\#37](https://github.com/pobsteta/nemetonshiny/issues/37)) for a
  tighter visual link to the table card it controls.
- `ui(action_plan)` — bulk-status block (the *Statut Kanban* section
  that duplicated the per-card Kanban moves) dropped in favour of a
  **collapsible action card**
  ([\#38](https://github.com/pobsteta/nemetonshiny/issues/38)) so the
  sidebar stays scannable.
- `ui(action_plan)` — right action panel resized to **350 px** with
  **dual collapse** behavior
  ([\#39](https://github.com/pobsteta/nemetonshiny/issues/39)): the
  bslib sidebar itself can collapse, and the action card inside it has
  its own collapse toggle.

#### Plan d’actions — role-based permissions (Lot 6 S15)

- `feat(action_plan)` — **role-based write permissions**
  ([\#40](https://github.com/pobsteta/nemetonshiny/issues/40)). New
  helper `can_edit_action_plan(auth_state)` centralises the role
  convention used across the tab: roles `proprietaire`, `editeur`,
  `admin`, `manager`, `owner`, `editor` are allowed to mutate the plan;
  anonymous sessions default to *editor* to preserve the current dev
  experience; the `lecteur` role (and any other unrecognised role) is
  read-only. `app_state$auth` now exposes the auth reactive so the
  action plan module can subscribe to it. Read-only sessions see a
  banner above the action table and 8 server-side mutation observers
  (add / edit / delete / bulk status / IA generation / Kanban drop /
  GeoPackage write / PDF write) bounce back with a toast. The Kanban
  drop handler also rolls back the optimistic UI move when the user
  lacks edit rights. 5 unit tests on `can_edit_action_plan()`. Closes
  [\#22](https://github.com/pobsteta/nemetonshiny/issues/22), refs
  [\#7](https://github.com/pobsteta/nemetonshiny/issues/7).

#### Auth

- `feat(auth)` — **`NEMETON_AUTH_DEV_ROLES` env override**
  ([\#41](https://github.com/pobsteta/nemetonshiny/issues/41)). In
  anonymous mode (no OAuth client configured), `auth_state$user_roles`
  is now seeded from the comma-separated env var
  `NEMETON_AUTH_DEV_ROLES` (e.g. `NEMETON_AUTH_DEV_ROLES=lecteur` to
  test the read-only banner). Lets developers exercise the role-based
  mutation guards landed in PR
  [\#40](https://github.com/pobsteta/nemetonshiny/issues/40) without
  standing up a Keycloak realm. Refs
  [\#7](https://github.com/pobsteta/nemetonshiny/issues/7).

#### i18n

- New key `action_plan_total_surface` (FR: *Surface totale* / EN: *Total
  area*).
- New keys for the Lot 6 read-only banner + permission-denied toast
  (FR/EN), bundled in PR
  [\#40](https://github.com/pobsteta/nemetonshiny/issues/40).

## nemetonshiny 0.22.2 (2026-05-06)

#### Plan d’actions — table & Kanban polish

- `refactor(action_plan)` — DT table trimmed and stabilised. Removed
  columns *Type libre*, *Objectifs*, *RDI*, and *Source* — they were
  rarely used, made horizontal scroll worse, and the underlying fields
  are still editable through the row-level form. Per-column filter row
  dropped (`filter = "top"` → `filter = "none"`); the global search box
  is the single filter exposed. Rows now have a uniform height: the
  datatable carries `class = "compact stripe hover nowrap"` and a scoped
  `.dt-truncate` rule
  (`max-width: 220px; overflow: hidden; text-overflow: ellipsis`) keeps
  long commentaire / labels on a single line. The two pinned left
  columns (UGF + Année) are unchanged.
- `feat(action_plan)` — Kanban board layout reorganised. The four active
  workflow stages (*Proposée*, *Validée*, *Planifiée*, *Réalisée*) sit
  side by side as a 4-column grid; *Abandonnée* is rendered full-width
  below as a separate, less prominent archive lane. Empty columns now
  reserve a 60 px drop zone so cards can be dragged into them.
- `feat(action_plan)` — **Drag-and-drop on the Kanban board**. Cards can
  be moved between columns by dragging. SortableJS 1.15.6 is vendored
  under `inst/app/www/js/Sortable-1.15.6.min.js` (MIT, ~45 KB), wired up
  by a small init script (`action_plan_kanban.js`) that re-binds on
  every renderUI tick to avoid stale instances. On drop, the JS pushes
  `input$kanban_drop = list(action_id, target_status, source_status, nonce)`
  to the server, where a new observer validates the transition through
  the existing `is_valid_status_transition()` rules:
  - **Allowed transition** → `update_action_in_plan()` +
    `save_action_plan()`, then `plan_rv()` is bumped, which triggers a
    renderUI re-run that confirms the move.
  - **Refused transition** (e.g. trying to drag a *Réalisée* card back
    to *Proposée*) → a warning toast surfaces with the offending pair,
    and `kanban_render_token` is bumped to re-render the board, which
    puts the card back where the data says it belongs. The previous
    per-card *Déplacer* dropdown is preserved — both paths share the
    same validator + persistence code.

#### i18n

- New key `action_plan_kanban_drop_invalid_fmt` (FR/EN) for the
  refused-transition toast.

#### Tests

- `tests/testthat/test-mod_action_plan.R` — three new test_thats:
  vendored asset existence (`Sortable-1.15.6.min.js` +
  `action_plan_kanban.js`), validator coverage for legal vs refused
  drag-drop transitions, and presence of the new i18n key in both
  locales.

## nemetonshiny 0.22.1 (2026-05-06)

#### Bug fixes

- `fix(action_plan)` — empty action plans no longer crash the reactive
  chain. `actions_df_all()` was assigning a length-1 `NA_character_` to
  `df$ug_label` on a 0-row data.frame, which R rejects with
  *“replacement has 1 row, data has 0”*. The 0-row branch now returns
  early with explicit empty columns. Surfaced on a fresh project with no
  actions yet (regression introduced in v0.22.0 with the new Plan
  d’actions tab).

#### Improvements

- `feat(db)` — app schema (`nemeton.projects` and friends) is now
  initialized automatically on first connection through
  `get_db_connection()`. The existing idempotent
  [`db_init_schema()`](https://pobsteta.github.io/nemetonshiny/reference/db_init_schema.md)
  used to be exported but never invoked, so a freshly provisioned
  database surfaced *“relation "nemeton.projects" does not exist”* on
  the first project save. Memoized once per R session via
  `.nemeton_env`.
- `feat(monitoring)` — monitoring-DB migrations (`monitoring_zone`,
  `alert`, `obs_pixel`, …) are now applied automatically on first
  connection through `get_monitoring_db_connection()` by calling
  [`nemeton::db_migrate()`](https://pobsteta.github.io/nemeton/reference/db_migrate.html).
  The Monitoring tab no longer warns *“relation "monitoring_zone" does
  not exist”* on a fresh TimescaleDB. Memoized once per R session via
  `.nemeton_env`.

## nemetonshiny 0.22.0 (2026-05-06)

#### New feature — “Plan d’actions” tab

A full new tab dedicated to building, visualising and exporting
multi-year forest action plans, delivered across PRs
[\#23](https://github.com/pobsteta/nemetonshiny/issues/23)..#34.

- **Lot 1+2 (S1..S6,
  [\#23](https://github.com/pobsteta/nemetonshiny/issues/23))** —
  scaffold of the tab, interactive Leaflet map + DT table with two-way
  sync, per-UGF action rows.
- **Lot 3 (S7+S8,
  [\#25](https://github.com/pobsteta/nemetonshiny/issues/25))** —
  LLM-powered plan generation through a new `planificateur` expert
  profile, Kanban board view, and an audit modal exposing the prompt,
  model, latency and token counts for each generation.
- **Lot 4 (S9..S11,
  [\#30](https://github.com/pobsteta/nemetonshiny/issues/30))** —
  cumulative balance sparkline per UGF, Gantt timeline of scheduled
  actions, and bridges from each row to the Terrain tab (jump to the
  matching plot).
- **Lot 5 (S12+S13,
  [\#32](https://github.com/pobsteta/nemetonshiny/issues/32))** —
  GeoPackage export of the full plan and per-UGF PDF export through a
  new Quarto template `inst/quarto/action_plan_template.qmd`.
- **Bilan column + steered LLM
  ([\#29](https://github.com/pobsteta/nemetonshiny/issues/29))** —
  derive a per-UGF balance column (`revenu_eur - cout_eur`, cumulative)
  and steer the LLM prompt toward solutions that keep the cumulative
  balance positive over the planning horizon.
- **`revenu_eur` field
  ([\#28](https://github.com/pobsteta/nemetonshiny/issues/28))** — new
  revenue column alongside `cout_eur`, propagated through all views,
  exports and prompts.
- **UI polish** — 50/50 layout with native DT filters and frozen UGF
  label column
  ([\#26](https://github.com/pobsteta/nemetonshiny/issues/26)), map
  auto-refit on bbox change with calendar year display
  ([\#27](https://github.com/pobsteta/nemetonshiny/issues/27)),
  categorical year legend with UGF label in popup
  ([\#31](https://github.com/pobsteta/nemetonshiny/issues/31)), Kanban
  board with sticky DT header
  ([\#33](https://github.com/pobsteta/nemetonshiny/issues/33)),
  right-hand bslib action sidebar grouping all toolbar buttons by intent
  ([\#34](https://github.com/pobsteta/nemetonshiny/issues/34), refs
  [\#7](https://github.com/pobsteta/nemetonshiny/issues/7)).
- **Robustness
  ([\#24](https://github.com/pobsteta/nemetonshiny/issues/24))** — guard
  map color palettes when the plan is empty so the tab never crashes on
  a project with no actions.

#### Other improvements

- `feat(project)` — the *Informations projet* card now shows the storage
  directory of the loaded project, so users can locate the GeoPackage
  and metadata files without leaving the app.
- `feat(home)` — the PostGIS sync toast now reports the actual target as
  `dbname@host:port`, so users immediately know which database their
  commune cache is going to.
- `feat(home)` — load the active project into the Monitoring zone picker
  so users do not have to redraw the AOI when switching tabs.
- `feat(monitoring)` — the “Enregistrer comme zone” button now exposes a
  tooltip explaining why it is disabled (no AOI drawn, no project
  loaded, etc.).
- `i18n(monitoring)` — clarify FAST naming in the quick-mode labels.
- `refactor(monitoring)` — rename the health-export “QField” labels to
  “QGIS” in the UI to match what is actually produced (.qgz project
  file).
- `fix(db)` — `service_db.R` now honors `NEMETON_DB_URL` priority over
  the legacy `POSTGRESQL_ADDON_*` Clever Cloud variables, so local
  overrides are respected.
- `fix(sampling)` — surface silent `save_samples()` failures with a
  user-visible toast instead of swallowing the error.

#### Internal

- New module `R/mod_action_plan.R` (~1816 LOC) and service layer
  `R/service_action_plan.R` (~686 LOC).
- New expert profile `inst/experts/planificateur.yml`.
- New Quarto template `inst/quarto/action_plan_template.qmd`.
- New tests `test-action_plan_prompts.R`, `test-mod_action_plan.R`,
  `test-service_action_plan.R` (~500 LOC of testthat).
- `R/utils_i18n.R` — +263 LOC of new keys (NMT convention) for the Plan
  d’actions tab in FR/EN.

## nemetonshiny 0.21.0 (2026-04-30)

#### New feature — Forest health monitoring (E6.c.5, spec 008)

The Monitoring tab is now a two-mode forest health workstation.

- **Mode 1 — Surveillance rapide** (existing E6.b NDVI/NBR rolling
  window, kept as-is) detects recent shocks (cuts, windthrows, fires) in
  seconds.
- **Mode 2 — Diagnostic sanitaire (FORDEAD)** wraps
  [`nemeton::run_fordead_dieback()`](https://pobsteta.github.io/nemeton/reference/run_fordead_dieback.html)
  (CRSWIR + harmonic model via reticulate, GPL-3 isolated to the Python
  frontier) in a
  [`shiny::ExtendedTask`](https://rdrr.io/pkg/shiny/man/ExtendedTask.html).
  Detects progressive dieback (bark beetle, drought) on conifers in
  minutes-to-hours. Both pipelines write to the same `alert` table.
- **G1 — class filter**. By default the leaflet shows only
  `confidence_class` 3-forte and 4-sol-nu (\>70% true positives per
  ONF/DSF 2024). A “Inclure faible/moyenne” toggle adds the 1-2 classes
  and surfaces a `border-warning` banner citing the up-to-50%
  false-positive rate.
- **G2 — disturbance classification**. Alerts go through
  [`nemeton::classify_disturbance()`](https://pobsteta.github.io/nemeton/reference/classify_disturbance.html)
  server-side, so the popup carries a `disturbance_type` to separate
  progressive dieback from mechanical intervention.
- **G3 — validity banners + confirmation modal**.
  [`nemeton::check_fordead_validity()`](https://pobsteta.github.io/nemeton/reference/check_fordead_validity.html)
  is called on the current zone whenever the user enters health mode.
  Two `border-warning` banners fire when the AOI overlaps the 5
  validated departments (88, 39, 01, 73, 74) under 50%, or when épicéa +
  sapin pectiné drops under 70%. Launching FORDEAD on an out-of-domain
  area pops a modal citing the ONF/DSF caveat; “Run anyway” forwards to
  the task.
- **G4 — QField field-validation workflow**. A new card in health mode
  lets the user pick *n* plots (default 30) and a sampling method (GRTS
  / random) and download a `.qgz`. Re-uploading the filled GPKG via the
  new “Validation sanitaire” sub-tab in *Données terrain* runs
  [`nemeton::ingest_health_validation()`](https://pobsteta.github.io/nemeton/reference/ingest_health_validation.html),
  reports counts (confirmed / false-positive / unmatched), and updates
  `validation_status` + `validation_cause` per alert.
- **G5 — R5 dieback indicator**. The radar’s R-family picks up R5
  automatically through `nemeton::INDICATOR_FAMILIES$R` (no UI change
  needed; the cœur computes it via
  [`nemeton::indicateur_r5_deperissement()`](https://pobsteta.github.io/nemeton/reference/indicateur_r5_deperissement.html)).
- **Plotly time series**: in health mode shows the alert distribution by
  `confidence_class`. Quick-mode time series (NDVI/NBR per plot) ships
  with E6.b phase 3.
- **Persistence**: each FORDEAD launch writes `monitoring_mode`,
  `monitoring_threshold_anomaly`, `monitoring_vegetation_index`,
  `monitoring_dates_training`, plus the validity intersection
  percentages, to `metadata.json`. The module restores these inputs
  whenever a project is reopened.
- **i18n**: ~30 new keys (`monitoring_mode_*`, `monitoring_warning_*`,
  `monitoring_class_*`, `monitoring_qfield_*`, `health_validation_*`,
  `r5_*`). The tab itself is renamed *Suivi sanitaire* / *Forest health
  monitoring*.
- **Dependencies**: `plotly` promoted to Imports;
  `nemeton (>= 0.20.1.9004)` (FORDEAD pipeline + helpers).

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
  [`nemeton::import_qfield_gpkg()`](https://pobsteta.github.io/nemeton/reference/import_qgis_gpkg.html) +
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
  [`nemeton::create_qfield_project()`](https://pobsteta.github.io/nemeton/reference/create_qgis_project.html)
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

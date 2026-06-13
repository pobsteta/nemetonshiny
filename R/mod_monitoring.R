#' Monitoring Module for nemetonApp (E6.b + E6.c.5 — health monitoring)
#'
#' @description
#' Two-mode forest health monitoring:
#'
#' * **Mode 1 — Surveillance rapide** (E6.b): rolling-window NDVI/NBR on
#'   Sentinel-2, detects recent shocks (cuts, windthrows, fires).
#'   Seconds-scale.
#' * **Mode 2 — Diagnostic sanitaire** (E6.c.5, spec 008): FORDEAD via
#'   reticulate (CRSWIR + harmonic model), detects progressive dieback
#'   (bark beetle, drought). Minutes-to-hours scale.
#'
#' Both pipelines write to the same `alert` table (alert_type
#' discriminant). The G3 garde-fou (geographic + species validity check
#' via `nemeton::check_fordead_validity`) is enforced before any FORDEAD
#' run, with a confirmation modal when the user forces an out-of-domain
#' run.
#'
#' @name mod_monitoring
#' @keywords internal
NULL


# ---- UI --------------------------------------------------------------

#' Monitoring Module UI
#'
#' @param id Character. Module namespace ID.
#' @return Shiny UI elements.
#' @noRd
mod_monitoring_ui <- function(id) {
  ns <- shiny::NS(id)

  opts <- get_app_options()
  lang <- opts$language %||% "fr"
  i18n <- get_i18n(lang)

  bslib::layout_sidebar(
    fillable = TRUE,

    sidebar = bslib::sidebar(
      id = ns("sidebar"),
      width = 360,
      open = TRUE,

      # Collapsible card — same pattern as mod_field_ingest.
      htmltools::tags$div(
        class = "card mb-3",
        htmltools::tags$div(
          class = "card-header bg-success text-white py-2",
          style = "cursor: pointer;",
          `data-bs-toggle` = "collapse",
          `data-bs-target` = paste0("#", ns("monitor_collapse")),
          `aria-expanded` = "true",
          `aria-controls` = ns("monitor_collapse"),
          htmltools::div(
            class = "d-flex align-items-center justify-content-between",
            htmltools::div(
              class = "d-flex align-items-center",
              bsicons::bs_icon("activity", class = "me-2"),
              i18n$t("monitoring_accordion_title")
            ),
            bsicons::bs_icon("chevron-down", class = "collapse-icon")
          )
        ),
        htmltools::tags$div(
          id = ns("monitor_collapse"),
          class = "collapse show",
          htmltools::tags$div(
            class = "card-body",

            htmltools::tags$p(class = "text-muted small",
                              i18n$t("monitoring_subtitle")),

            # --- Mode toggle (E6.c.5 — T6app.1) ---------------------
            shiny::radioButtons(
              ns("mode"), i18n$t("monitoring_mode_label"),
              choices = stats::setNames(
                c("quick", "health", "reconfort"),
                c(i18n$t("monitoring_mode_quick"),
                  i18n$t("monitoring_mode_health"),
                  i18n$t("monitoring_mode_reconfort"))
              ),
              selected = "quick",
              inline   = FALSE
            ),
            shiny::uiOutput(ns("mode_help")),

            # --- Common: zone + date range --------------------------
            shiny::selectInput(
              ns("zone_id"), i18n$t("monitoring_zone_label"),
              choices = character(0),
              selected = NULL
            ),

            # Bridge between the loaded project (Home tab) and the
            # monitoring DB: registers the project's UGF union + the
            # placettes generated in the Sampling tab as a fresh
            # `monitoring_zone` row, then auto-selects it above.
            # Common to both modes (FAST + FORDEAD). Disabled until
            # The hint below the button surfaces which precondition
            # is missing. The button is NOT disabled at the HTML
            # level — clicking it when preconditions are not met
            # triggers a clear error notification (see observer
            # below). Hardcoding `disabled = NA` here would prevent
            # the click entirely and confuse the user.
            shiny::actionButton(
              ns("register"), i18n$t("monitoring_register_btn"),
              icon  = bsicons::bs_icon("geo-alt-fill"),
              class = "btn-outline-primary btn-sm w-100"
            ),
            shiny::uiOutput(ns("register_hint"), class = "mb-3"),

            shiny::dateRangeInput(
              ns("date_range"), i18n$t("monitoring_date_range"),
              start = Sys.Date() - 365L,
              end   = Sys.Date(),
              language  = lang,
              separator = i18n$t("date_range_separator")
            ),

            # --- Quick-mode parameters (NDVI/NBR) -------------------
            shiny::conditionalPanel(
              condition = sprintf("input['%s'] == 'quick'", ns("mode")),
              # v0.61.0 — Le `checkboxGroupInput("bands")` est retiré.
              # NDVI et NBR sont systématiquement téléchargés lors du
              # Diagnostic FAST (les radios NDVI/NBR des sidebars droits
              # des onglets Alertes FAST + Carte FAST pilotent
              # l'AFFICHAGE des deux indices, indépendamment du
              # téléchargement). Le câblage de `bands` est désormais en
              # dur dans `fast_task$invoke()`.
              # v0.36.1 / v0.42.0 — défauts et range alignés sur la
              # sémantique « seuil absolu » consommée par
              # nemeton::read_fast_alert_raster() (spec 013, nemeton@v0.46.0).
              # Un pixel est en alerte quand son NDVI (ou NBR) tombe SOUS
              # la valeur du slider. NDVI forestier sain est typiquement
              # 0.6-0.8, NBR sain 0.4-0.6, d'où range 0.10-0.80. Défauts
              # cœur : 0.40 / 0.30.
              # v0.52.14 — Le radio « Indice FAST » a été déplacé du
              # sidebar parent vers le sidebar DROIT de chaque onglet
              # (Alertes FAST + Carte FAST, symétriques). Chaque onglet
              # pilote son propre indice indépendamment. Les 2 sliders
              # de seuil restent ici (un par indice) ; le module
              # consommateur lit le seuil correspondant à l'indice
              # sélectionné dans son sidebar.
              shiny::sliderInput(
                ns("threshold_ndvi"), i18n$t("monitoring_threshold_ndvi"),
                min = 0.10, max = 0.80, value = 0.40, step = 0.01
              ),
              shiny::sliderInput(
                ns("threshold_nbr"), i18n$t("monitoring_threshold_nbr"),
                min = 0.10, max = 0.80, value = 0.30, step = 0.01
              ),
              # NDMI (humidité) : seuil minimum sous lequel un pixel est
              # en alerte (NDMI baisse sous stress hydrique). NDMI sain
              # est plus bas que NDVI/NBR, d'où un défaut moindre.
              # Consommé par l'onglet Alertes/Carte FAST quand l'indice
              # NDMI est sélectionné (nemeton >= 0.64.0).
              shiny::sliderInput(
                ns("threshold_ndmi"), i18n$t("monitoring_threshold_ndmi"),
                min = 0.10, max = 0.80, value = 0.20, step = 0.01
              ),
              shiny::numericInput(
                ns("window_days"), i18n$t("monitoring_window_days"),
                value = 30L, min = 7L, max = 90L, step = 1L
              ),
              # Checkbox + popover info icon (same pattern as the
              # "Tout" button popover in mod_synthesis): keeps the
              # sidebar tight while making the long explanation
              # one click away instead of pre-filling vertical space.
              htmltools::div(
                class = "d-flex align-items-center mb-1",
                shiny::checkboxInput(
                  ns("reprime_cache"),
                  i18n$t("monitoring_reprime_cache_label"),
                  value = FALSE,
                  width = "100%"
                ),
                bslib::popover(
                  htmltools::tags$span(
                    class = "text-info ms-2",
                    style = "cursor: help;",
                    shiny::icon("circle-info", class = "fa-sm")
                  ),
                  htmltools::div(
                    style = paste("max-width: 380px;",
                                  "font-size: 0.85rem;",
                                  "line-height: 1.5;"),
                    htmltools::HTML(i18n$t("monitoring_reprime_cache_help"))
                  ),
                  options = list(customClass = "popover-lg"),
                  title = NULL
                )
              ),
              shiny::actionButton(
                ns("run"), i18n$t("monitoring_run_btn"),
                icon  = bsicons::bs_icon("play-fill"),
                class = "btn-primary w-100"
              ),
              # "Cancel / reset" button — only visible while the worker
              # is running. Force-unlocks the UI without killing the
              # background future (which Shiny's ExtendedTask cannot
              # cancel anyway). See the run_cancel observer for the
              # full story.
              shiny::uiOutput(ns("run_cancel_panel")),
              shiny::uiOutput(ns("cache_hint"), class = "small text-muted mt-1")
            ),

            # --- Health-mode parameters (FORDEAD) -------------------
            shiny::conditionalPanel(
              condition = sprintf("input['%s'] == 'health'", ns("mode")),
              shiny::dateRangeInput(
                ns("dates_training"),
                i18n$t("monitoring_dates_training_label"),
                start     = as.Date("2016-01-01"),
                end       = as.Date("2017-12-31"),
                language  = lang,
                separator = i18n$t("date_range_separator")
              ),
              shiny::selectInput(
                ns("vegetation_index"),
                i18n$t("monitoring_vegetation_index"),
                choices  = c(CRSWIR = "CRSWIR", NDVI = "NDVI", NDWI = "NDWI"),
                selected = "CRSWIR"
              ),
              shiny::sliderInput(
                ns("threshold_anomaly"),
                i18n$t("monitoring_threshold_anomaly"),
                min = 0.05, max = 0.50, value = 0.16, step = 0.01
              ),
              shiny::actionButton(
                ns("run_health"), i18n$t("monitoring_run_health_btn"),
                icon  = bsicons::bs_icon("activity"),
                class = "btn-primary w-100"
              ),
              shiny::uiOutput(ns("run_health_cancel_panel"))
            ),
            # ----- RECONFORT params (spec 021, L6) ---------------------
            # Mode "reconfort" : dépérissement feuillus. Le run lourd
            # (conda IOTA²/GEODES) est opt-in ; sur un déploiement sans
            # ce bundle, le bouton signale l'indisponibilité (la carte +
            # le diagnostic pixel restent utilisables sur les runs déjà
            # produits — cf. Limite #1 spec 021).
            shiny::conditionalPanel(
              condition = sprintf("input['%s'] == 'reconfort'", ns("mode")),
              shiny::numericInput(
                ns("reconfort_s2_year"),
                i18n$t("monitoring_reconfort_s2_year"),
                value = as.integer(format(Sys.Date(), "%Y")),
                min = 2016L, max = as.integer(format(Sys.Date(), "%Y")),
                step = 1L
              ),
              shiny::actionButton(
                ns("run_reconfort"), i18n$t("monitoring_run_reconfort_btn"),
                icon  = bsicons::bs_icon("activity"),
                class = "btn-primary w-100"
              ),
              shiny::uiOutput(ns("run_reconfort_cancel_panel"))
            )
          )
        )
      )
    ),

    # Main area: DB status + G3 banners + 3 sub-tabs (Alerts / Per-plot
    # time series / Pixel map). v0.28.0 wraps the previously single-
    # column layout into a navset_card_tab so the new spec-010 "Carte
    # pixel" view fits naturally without burying the existing alerts
    # map deep below.
    htmltools::tags$div(
      class = "p-2",
      shiny::uiOutput(ns("db_status")),
      shiny::uiOutput(ns("validity_banners")),
      # v0.77.0 — Bandeau « Surfaces des zones de suivi » : en mode FAST
      # (quick), rappelle la surface (ha) + la part (%) des 4 strates
      # projet `_tot/_feu/_res/_mix` au-dessus des sous-onglets. Symétrie
      # visuelle avec les bandeaux de validité FORDEAD. Masqué en mode
      # health (FORDEAD) et quand aucune zone n'est encore générée.
      shiny::uiOutput(ns("fast_zone_surfaces")),
      bslib::navset_card_tab(
        id = ns("subtab"),
        # ---------- FAST mode sub-tabs (visible en mode quick) -------
        # ----- Sub-tab — Alertes FAST (raster depuis v0.42.0) --------
        # Carte Leaflet d'un raster d'alerte pixel-par-pixel
        # (10 m S2) produit par `nemeton::read_fast_alert_raster()`
        # (spec 013, nemeton@v0.46.0). Toggle count / rolling exposé
        # par le sous-module. Masqué en mode health par l'observer
        # mode-driven.
        bslib::nav_panel(
          title = i18n$t("monitoring_subtab_alerts_fast"),
          value = "alerts_fast",
          icon  = bsicons::bs_icon("bell"),
          mod_monitoring_fast_alerts_ui(ns("fast_alerts"))
        ),
        # ----- Sub-tab — Carte FAST (spec 010) -----------------------
        # Visible en mode quick — raster NDVI/NBR + slider date.
        # Masqué en mode health par l'observer mode-driven du server.
        bslib::nav_panel(
          title = i18n$t("monitoring_subtab_pixel_map_fast"),
          value = "pixel_map_fast",
          icon  = bsicons::bs_icon("map"),
          mod_monitoring_pixel_map_ui(ns("pixel_map"))
        ),
        # ---------- FORDEAD mode sub-tabs (visible en mode health) ---
        # ----- Sub-tab — Alertes FORDEAD (FORDEAD map + QGIS) --------
        # Anciennement « Alertes » avant v0.35.0. Renommé pour
        # symétrie avec Alertes FAST et Carte FORDEAD. La logique
        # reactive (alerts(), output$alerts_panel, output$alerts_map,
        # output$qgis_panel) reste identique — seul le label et la
        # value du nav_panel changent.
        bslib::nav_panel(
          title = i18n$t("monitoring_subtab_alerts_fordead"),
          value = "alerts_fordead",
          icon  = bsicons::bs_icon("exclamation-triangle"),
          # The "include low confidence classes" toggle used to live
          # inside card_header next to the title, but its long label
          # wrapped awkwardly on narrow viewports. Lifted into its own
          # row so it always sits on a single line above the alerts.
          # Plus de conditionalPanel mode=='health' depuis v0.35.0 :
          # tout le sous-onglet est caché en mode quick.
          htmltools::div(
            class = "px-3 pt-2",
            shiny::checkboxInput(
              ns("include_low"),
              i18n$t("monitoring_include_low"),
              value = FALSE
            )
          ),
          shiny::uiOutput(ns("alerts_panel")),
          # Health-mode QGIS generator (E6.c.5 — T6app.12, wired in C6)
          shiny::uiOutput(ns("qgis_panel"))
        ),
        # ----- Sub-tab — Carte FORDEAD (wired v0.36.0) ---------------
        # Raster catégoriel 0-4 du dépérissement (sain/faible/moyenne/
        # forte/sol-nu) lu via `nemeton::read_fordead_dieback_mask()`
        # (shipped en nemeton@v0.25.0). Le reader retourne NULL tant
        # que le writer cœur (persist hook dans run_fordead_dieback)
        # n'a pas shippé — la sub-tab affiche alors un empty-state.
        # Masqué en mode quick par l'observer mode-driven.
        bslib::nav_panel(
          title = i18n$t("monitoring_subtab_pixel_map_fordead"),
          value = "pixel_map_fordead",
          icon  = bsicons::bs_icon("tree"),
          mod_monitoring_fordead_map_ui(ns("fordead_map"))
        ),
        # ----- Sub-tab — Carte RECONFORT (spec 021, L6) --------------
        # Alertes vectorielles de dépérissement feuillus (RECONFORT) +
        # diagnostic pixel CRSWIR/CRre au clic. Masqué hors mode
        # "reconfort" par l'observer mode-driven ci-dessous.
        bslib::nav_panel(
          title = i18n$t("monitoring_subtab_pixel_map_reconfort"),
          value = "pixel_map_reconfort",
          icon  = bsicons::bs_icon("tree-fill"),
          mod_monitoring_reconfort_map_ui(ns("reconfort_map"))
        ),
        # ----- Sub-tabs — Plan de validation (spec 014, v0.43.0) -----
        # v0.43.3 — passe de 1 onglet single avec radio à 2 sous-onglets
        # mode-driven, symétriques avec les couples Alertes/Carte :
        #   mode = "quick"  → Plan de validation FAST
        #   mode = "health" → Plan de validation FORDEAD
        # Cohérent avec la logique FAST vs FORDEAD du reste du module ;
        # la source est implicite, pas de radio à choisir.
        bslib::nav_panel(
          title = i18n$t("validation_sampling_title_fast"),
          value = "validation_sampling_fast",
          icon  = bsicons::bs_icon("compass"),
          mod_validation_sampling_ui(ns("validation_sampling_fast"),
                                     source = "FAST")
        ),
        bslib::nav_panel(
          title = i18n$t("validation_sampling_title_fordead"),
          value = "validation_sampling_fordead",
          icon  = bsicons::bs_icon("compass"),
          mod_validation_sampling_ui(ns("validation_sampling_fordead"),
                                     source = "FORDEAD")
        )
      )
    )
  )
}


# ---- Server ----------------------------------------------------------

#' Monitoring Module Server
#'
#' @param id Character. Module namespace ID.
#' @param app_state reactiveValues carrying `language` and (later)
#'   monitoring config persisted on `current_project`.
#'
#' @return A list of reactives (currently `zones` only — phases 2+
#'   add ingestion result and alerts).
#' @noRd
mod_monitoring_server <- function(id, app_state) {
  shiny::moduleServer(id, function(input, output, session) {

    i18n_r <- shiny::reactive({
      get_i18n(app_state$language %||% "fr")
    })

    # Mode-driven sub-tab visibility (v0.34.0 → v0.35.0). Quatre
    # sous-onglets, deux affichés à la fois selon `input$mode` :
    #
    #   mode = "quick"  → Alertes FAST    + Carte FAST    + Plan val. FAST
    #   mode = "health" → Alertes FORDEAD + Carte FORDEAD + Plan val. FORDEAD
    #
    # Symétrie : chaque mode a son trio (liste d'alertes + carte + plan
    # de validation). Pattern uniforme `bslib::nav_show()` / `nav_hide()`
    # plutôt qu'un conditionalPanel interne, parce qu'on veut aussi
    # éviter de déclencher les reactives lourdes (build_index_stack
    # côté FAST, list_alerts_for_zone côté FORDEAD, generate_validation_plan
    # côté Plan val.) quand l'onglet est masqué.
    shiny::observe({
      mode <- input$mode
      # Helper : hide a set of sub-tab targets.
      .hide <- function(targets) for (t in targets)
        bslib::nav_hide("subtab", target = t, session = session)
      .show <- function(targets) for (t in targets)
        bslib::nav_show("subtab", target = t, session = session)

      fast_tabs     <- c("alerts_fast", "pixel_map_fast",
                         "validation_sampling_fast")
      fordead_tabs  <- c("alerts_fordead", "pixel_map_fordead",
                         "validation_sampling_fordead")
      reconfort_tabs <- c("pixel_map_reconfort")

      if (identical(mode, "quick")) {
        .show(fast_tabs)
        .hide(c(fordead_tabs, reconfort_tabs))
        # v0.37.1 — re-anchor the active tab onto a visible one.
        # Without this, when the user toggles mode the navset keeps
        # its previously-active pane — which may now be hidden —
        # leaving the content area in an inconsistent state.
        bslib::nav_select("subtab", selected = "alerts_fast",
                          session = session)
      } else if (identical(mode, "health")) {
        .hide(c(fast_tabs, reconfort_tabs))
        .show(fordead_tabs)
        bslib::nav_select("subtab", selected = "alerts_fordead",
                          session = session)
      } else if (identical(mode, "reconfort")) {
        .hide(c(fast_tabs, fordead_tabs))
        .show(reconfort_tabs)
        bslib::nav_select("subtab", selected = "pixel_map_reconfort",
                          session = session)
      }
    })

    # Bumped after each FORDEAD run so the alerts reactive (wired in C5)
    # re-fetches from the DB.
    alerts_refresh <- shiny::reactiveVal(0L)

    # spec 021 (L6) — bumped after a RECONFORT run completes / when the
    # RECONFORT sub-tab opens, so the reconfort map's alerts reactive
    # re-fetches from the DB. Independent of `alerts_refresh` (FAST /
    # FORDEAD) so a FORDEAD run does not needlessly re-query RECONFORT.
    reconfort_refresh <- shiny::reactiveVal(0L)

    # v0.52.16 — `obs_refresh` reactiveVal supprimé : il servait à
    # signaler aux consommateurs de `obs_pixel_data()` une nouvelle
    # ingestion. Comme le reactive est supprimé (FAST = pure raster
    # per-pixel, plus de lecture de `obs_pixel`), ce compteur n'a plus
    # de consommateur. `fast_reload` ci-dessous suffit pour signaler
    # le rafraîchissement aux modules raster qui dépendent du cache
    # COG (mod_monitoring_pixel_map, mod_monitoring_fast_alerts).

    # v0.42.0 — generic FAST refresh signal. Wired into sub-modules
    # whose reactives have no other path back to a post-ingestion
    # state change. Bumped from the FAST success handler (see below).
    # Consumed by :
    #   - mod_monitoring_fast_alerts (alerts() / future raster reactive)
    #   - mod_monitoring_pixel_map   (cache_dir_r — dir.exists() is not
    #                                 a reactive dep, so the reactive
    #                                 stays frozen on its pre-ingest
    #                                 NULL value otherwise).
    # Symmetric with how `alerts_refresh` is wired into FORDEAD-side
    # modules.
    fast_reload <- shiny::reactiveVal(0L)

    # ----- Async DB probe (E6.x — persistent loading feedback) ------
    # Open db_connect + db_migrate in a future worker so the user sees
    # a real "loading" state (spinning gear card) while the schema
    # bootstraps. On warm cache the round-trip is < 100 ms; on a fresh
    # SQLite file or a slow Postgres it can take a couple of seconds —
    # that is exactly when a visible state matters most.
    #
    # The worker only probes reachability: DBI connections are not
    # serializable across processes, so the actual connection used by
    # validity()/alerts()/zones() is re-opened (sync, fast — schema
    # already migrated) in the main process once the probe succeeded.
    db_probe_task <- shiny::ExtendedTask$new(function(db_url) {
      if (requireNamespace("future", quietly = TRUE)) {
        plan_classes <- class(future::plan())
        is_parallel <- any(c("multisession", "multicore", "cluster") %in%
                             plan_classes)
        if (!is_parallel) future::plan("multisession")
      }
      promises::future_promise({
        # The worker only depends on `nemeton` (a hard dependency in
        # DESCRIPTION) — never on nemetonshiny. Going through
        # nemetonshiny's internal wrapper introduced two flavours of
        # silent failure:
        #   * `getFromNamespace("last_monitoring_db_error")` would throw
        #     "objet 'X' introuvable" when an older nemetonshiny was
        #     loaded in the worker (v0.24.4 regression);
        #   * `get_monitoring_db_connection(db_url = ...)` would throw
        #     "argument inutile (db_url = db_url)" when an older
        #     nemetonshiny without the `db_url` parameter was loaded
        #     (v0.24.5 regression — typically a stale dev checkout via
        #     pkgload, or a binary cached by pak that shadowed the
        #     fresh install for the worker subprocess).
        # Calling nemeton:: directly bypasses both classes of mismatch.
        if (!nzchar(db_url)) {
          return(list(ok = FALSE, error = "no_url"))
        }
        if (!requireNamespace("nemeton", quietly = TRUE)) {
          return(list(
            ok    = FALSE,
            error = "Package 'nemeton' not available in worker."
          ))
        }

        probe_err <- NULL
        con <- tryCatch(
          nemeton::db_connect(db_url),
          error = function(e) {
            probe_err <<- conditionMessage(e)
            NULL
          }
        )
        if (is.null(con)) {
          msg <- if (!is.null(probe_err) && nzchar(probe_err))
                   probe_err
                 else
                   "connect_failed"
          return(list(ok = FALSE, error = msg))
        }

        # Migration is itself idempotent (schema_migration table). On
        # a fresh SQLite file it creates the schema; on an already-
        # migrated base it's a single SELECT.
        mig_ok <- tryCatch({
          nemeton::db_migrate(con)
          TRUE
        }, error = function(e) {
          probe_err <<- paste0("Migration failed: ", conditionMessage(e))
          FALSE
        })
        # Always close, even on migration failure.
        tryCatch(nemeton::db_disconnect(con), error = function(e) NULL)
        if (!isTRUE(mig_ok)) {
          return(list(ok = FALSE, error = probe_err))
        }
        list(ok = TRUE, error = NULL)
      }, seed = TRUE)
    })

    # Invoke the probe whenever the project changes (or the env URL
    # changes via the resolver). `bindEvent` would also work but we
    # want a fresh probe even when the same project is re-loaded.
    shiny::observe({
      project <- app_state$current_project
      url     <- .resolve_monitoring_db_url(project)
      backend <- monitoring_db_backend(project = project)
      # Skip probe when there is nothing to probe — output$db_status
      # handles the "no project / RSQLite missing" cases synchronously.
      if (identical(backend, "none") || !nzchar(url)) return()
      db_probe_task$invoke(db_url = url)
    })

    # Turn the task status into a small UI-facing reactive — keeps
    # output$db_status free of ExtendedTask plumbing.
    db_probe_state <- shiny::reactive({
      status <- db_probe_task$status()
      if (identical(status, "initial")) return(list(state = "initial"))
      if (identical(status, "running")) return(list(state = "running"))
      if (identical(status, "error")) {
        err <- tryCatch(
          {
            db_probe_task$result()
            NULL
          },
          error = function(e) conditionMessage(e)
        )
        return(list(state = "error", error = err))
      }
      # status == "success"
      res <- tryCatch(db_probe_task$result(),
                      error = function(e) NULL)
      if (is.null(res) || isFALSE(res$ok)) {
        return(list(state = "error",
                    error = res$error %||% "unknown"))
      }
      list(state = "ready")
    })

    # ----- Restore monitoring config from project metadata ----------
    # Fires whenever the user opens a project. Mirrors the persistence
    # in .invoke_fordead() (and mode/threshold changes via the sidebar).
    shiny::observe({
      m <- app_state$current_project$metadata
      if (is.null(m)) return()
      if (!is.null(m$monitoring_mode)) {
        shiny::updateRadioButtons(session, "mode",
                                  selected = m$monitoring_mode)
      }
      if (!is.null(m$monitoring_threshold_anomaly)) {
        shiny::updateSliderInput(session, "threshold_anomaly",
                                 value = as.numeric(m$monitoring_threshold_anomaly))
      }
      if (!is.null(m$monitoring_vegetation_index)) {
        shiny::updateSelectInput(session, "vegetation_index",
                                 selected = m$monitoring_vegetation_index)
      }
      if (!is.null(m$monitoring_dates_training)) {
        dt <- as.Date(unlist(m$monitoring_dates_training))
        if (length(dt) == 2L && all(!is.na(dt))) {
          shiny::updateDateRangeInput(session, "dates_training",
                                      start = dt[1], end = dt[2])
        }
      }
    })

    # ----- Mode help text -------------------------------------------
    output$mode_help <- shiny::renderUI({
      i18n <- i18n_r()
      key <- switch(input$mode %||% "quick",
                    health    = "monitoring_mode_health_help",
                    reconfort = "monitoring_mode_reconfort_help",
                    "monitoring_mode_quick_help")
      htmltools::tags$p(class = "text-muted small fst-italic", i18n$t(key))
    })

    # ----- G3 — Validity banners (geo + species) --------------------
    # Recomputed when zone or mode changes. Quick mode does not need
    # the FORDEAD validity check, so banners only show in health mode.
    #
    # v0.37.0 — passe `bdforet` à `validity_check_for_zone()` quand le
    # projet a une cache `<project>/cache/layers/bdforet.gpkg` (écrite
    # par `download_ign_bdforet()` pendant le compute des indicateurs).
    # Le cœur (nemeton@v0.26.0+) utilise BD Forêt V2 pour dériver
    # l'essence dominante par parcelle quand `units` n'a pas de
    # colonne d'essence — ce qui est le cas par défaut pour les UGFs
    # de cette app. Sans cache (projet sans compute, ou compute
    # lancé sur une fenêtre où le téléchargement BD Forêt a échoué),
    # `bdforet = NULL` et le comportement v0.25.9 est préservé
    # (warning « species check skipped »).
    validity <- shiny::reactive({
      if (!identical(input$mode, "health")) return(NULL)
      zone <- input$zone_id
      if (!isTRUE(nzchar(zone))) return(NULL)
      proj <- app_state$current_project
      con <- get_monitoring_db_connection(project = proj, read_only = TRUE)
      on.exit(close_monitoring_db_connection(con), add = TRUE)
      units   <- proj$indicators_sf
      bdforet <- .load_project_bdforet(proj)
      validity_check_for_zone(con, as.integer(zone),
                              units   = units,
                              bdforet = bdforet)
    })

    output$validity_banners <- shiny::renderUI({
      v <- validity()
      if (is.null(v)) return(NULL)
      i18n <- i18n_r()
      banners <- list()
      if (isFALSE(v$geo_valid)) {
        pct <- (v$geo_intersection_pct %||% 0) * 100
        banners[[length(banners) + 1L]] <- .monitoring_validity_banner(
          icon  = "geo-alt",
          title = i18n$t("monitoring_warning_geo_title"),
          body  = sprintf(i18n$t("monitoring_warning_geo_body"), pct)
        )
      }
      if (isFALSE(v$species_valid)) {
        pct <- (v$species_resineux_pct %||% 0) * 100
        banners[[length(banners) + 1L]] <- .monitoring_validity_banner(
          icon  = "tree",
          title = i18n$t("monitoring_warning_species_title"),
          body  = sprintf(i18n$t("monitoring_warning_species_body"), pct)
        )
      }
      if (isTRUE(input$include_low)) {
        banners[[length(banners) + 1L]] <- .monitoring_validity_banner(
          icon  = "exclamation-triangle",
          title = i18n$t("monitoring_warning_low_classes"),
          body  = NULL
        )
      }
      if (!length(banners)) return(NULL)
      htmltools::tagList(banners)
    })

    # ----- Alerts reactive (FORDEAD) --------------------------------
    # G1: by default only classes 3-forte + 4-sol-nu. The "include_low"
    # checkbox flips to all classes (1-2 inclusive). Re-fetches whenever
    # the zone, the toggle, or alerts_refresh() change.
    alerts <- shiny::reactive({
      alerts_refresh()
      zone <- input$zone_id
      if (!isTRUE(nzchar(zone))) return(NULL)
      if (!identical(input$mode, "health")) return(NULL)
      classes <- if (isTRUE(input$include_low))
                   c("1-faible", "2-moyenne", "3-forte", "4-sol-nu")
                 else
                   c("3-forte", "4-sol-nu")
      con <- get_monitoring_db_connection(project = app_state$current_project,
                                          read_only = TRUE)
      on.exit(close_monitoring_db_connection(con), add = TRUE)
      list_alerts_for_zone(con, as.integer(zone), classes = classes)
    })

    # ----- Alerts leaflet (T6app.11) + zone-saine card (v0.36.5) ----
    #
    # Trois états :
    #   - alerts() retourne au moins une ligne   → carte Leaflet
    #     (chemin actuel — placettes flaguées par classe)
    #   - alerts() est vide ET un run FORDEAD a abouti dans la
    #     session avec status = "success" → card « Zone saine » avec
    #     la durée du run, pour confirmer à l'utilisateur que
    #     0 anomalie = forêt saine (pas un run en cours, pas un échec
    #     silencieux). Cf. bug report 2026-05-19.
    #   - alerts() est vide ET aucun run dans la session → placeholder
    #     générique (« Lancer un diagnostic FORDEAD pour cette zone »).
    output$alerts_panel <- shiny::renderUI({
      ns <- session$ns
      i18n <- i18n_r()
      a <- alerts()
      last <- fordead_last_result()
      if (!is.null(a) && nrow(a)) {
        return(leaflet::leafletOutput(ns("alerts_map"), height = "55vh"))
      }
      # Empty alerts list. Differentiate "fresh session, no run yet"
      # from "run completed with 0 alerts" (zone saine).
      if (!is.null(last) && identical(last$status, "success")) {
        dur <- as.numeric(last$duration_sec %||% NA_real_)
        ts  <- last$mask_timestamp %||% NA_character_
        meta <- if (is.finite(dur)) {
          sprintf(i18n$t("monitoring_fordead_no_alerts_meta"), dur)
        } else if (isTRUE(last$reconciled) && !is.na(ts) && nzchar(ts)) {
          # Run reconciled from disk after a session reload — no
          # in-session duration, show the persisted-mask timestamp.
          sprintf(i18n$t("monitoring_fordead_no_alerts_meta_date"), ts)
        } else {
          ""
        }
        return(bslib::card(
          class = "border-success mt-2",
          bslib::card_header(
            htmltools::div(
              class = "d-flex align-items-center",
              bsicons::bs_icon("check-circle-fill",
                               class = "me-2 text-success fs-4"),
              htmltools::tags$strong(
                i18n$t("monitoring_fordead_no_alerts_title")
              )
            )
          ),
          bslib::card_body(
            htmltools::tags$p(
              i18n$t("monitoring_fordead_no_alerts_body")
            ),
            if (nzchar(meta))
              htmltools::tags$p(class = "text-muted small mb-0", meta)
          )
        ))
      }
      htmltools::tags$p(class = "text-muted",
                        i18n$t("monitoring_alerts_placeholder"))
    })

    output$alerts_map <- leaflet::renderLeaflet({
      a <- alerts()
      if (is.null(a) || !nrow(a)) return(NULL)
      i18n <- i18n_r()

      a_ll <- if (sf::st_crs(a)$epsg %||% 4326L != 4326L)
                sf::st_transform(a, 4326)
              else a

      colors <- c(
        "1-faible"  = "#FFD27F",  # pale orange
        "2-moyenne" = "#FF9933",  # orange
        "3-forte"   = "#D62728",  # red
        "4-sol-nu"  = "#222222"   # near-black
      )
      cls <- as.character(a_ll$confidence_class)
      cls[!cls %in% names(colors)] <- "3-forte"
      fill <- unname(colors[cls])

      popups <- vapply(seq_len(nrow(a_ll)), function(i) {
        sprintf(
          "<strong>%s</strong>: %s<br/>%s: %.2f<br/>%s: %s<br/>%s: %s<br/>%s: %s",
          i18n$t("monitoring_alert_popup_class"),
          htmltools::htmlEscape(cls[i]),
          i18n$t("monitoring_alert_popup_stress"),
          as.numeric(a_ll$stress_index[i] %||% NA_real_),
          i18n$t("monitoring_alert_popup_date"),
          as.character(a_ll$trigger_date[i] %||% NA),
          i18n$t("monitoring_alert_popup_status"),
          htmltools::htmlEscape(as.character(a_ll$validation_status[i] %||% "")),
          i18n$t("monitoring_alert_popup_disturbance"),
          htmltools::htmlEscape(as.character(a_ll$disturbance_type[i] %||% "-"))
        )
      }, character(1))

      legend_labels <- c(
        i18n$t("monitoring_class_1"),
        i18n$t("monitoring_class_2"),
        i18n$t("monitoring_class_3"),
        i18n$t("monitoring_class_4")
      )

      leaflet::leaflet(a_ll) |>
        leaflet::addProviderTiles("OpenStreetMap",   group = "OSM") |>
        leaflet::addProviderTiles("Esri.WorldImagery", group = "Satellite") |>
        leaflet::addLayersControl(
          baseGroups = c("OSM", "Satellite"),
          options = leaflet::layersControlOptions(collapsed = TRUE)
        ) |>
        leaflet::addCircleMarkers(
          radius = 6, weight = 1, color = "#000",
          fillColor = fill, fillOpacity = 0.85,
          popup = popups
        ) |>
        leaflet::addLegend(
          position = "bottomright",
          colors   = unname(colors),
          labels   = legend_labels,
          title    = i18n$t("monitoring_alert_popup_class"),
          opacity  = 0.85
        )
    })

    # v0.52.16 — Bloc `obs_pixel_inputs` + `obs_pixel_data` supprimé.
    # FAST est désormais une analyse pure raster per-pixel (spec 017
    # cœur + clarification utilisateur 2026-06-02), donc `read_obs_pixel`
    # n'est plus consommé : ni la modale clic-placette (supprimée
    # dans mod_monitoring_pixel_map), ni le `scenes_df_r` qui dérive
    # désormais entièrement du cache COG disque, ne reposent plus sur
    # la table `obs_pixel`.

    # ----- QGIS sanitaire panel (T6app.12) --------------------------
    output$qgis_panel <- shiny::renderUI({
      ns <- session$ns
      i18n <- i18n_r()
      a <- alerts()
      bslib::card(
        bslib::card_header(
          htmltools::div(
            class = "d-flex align-items-center",
            bsicons::bs_icon("clipboard-check", class = "me-2"),
            i18n$t("monitoring_qgis_btn")
          )
        ),
        bslib::card_body(
          if (is.null(a) || !nrow(a))
            htmltools::tags$p(class = "text-muted",
                              i18n$t("monitoring_qgis_no_alerts"))
          else htmltools::tagList(
            shiny::numericInput(
              ns("qgis_n"),
              i18n$t("monitoring_qgis_n_label"),
              value = min(30L, nrow(a)),
              min = 1L, max = nrow(a), step = 1L
            ),
            shiny::radioButtons(
              ns("qgis_method"),
              i18n$t("monitoring_qgis_method"),
              choices  = c(GRTS = "grts", Random = "random"),
              selected = "grts",
              inline   = TRUE
            ),
            shiny::downloadButton(
              ns("qgis_download"),
              i18n$t("monitoring_qgis_btn"),
              icon  = bsicons::bs_icon("download"),
              class = "btn-success w-100"
            )
          )
        )
      )
    })

    # downloadHandler: build the placette plan + zip a .qgz on disk
    # then stream it back. generate_health_validation_plots needs the
    # *pending* alerts (not the leaflet-filtered ones), so we re-fetch
    # without the include_low / class filtering.
    output$qgis_download <- shiny::downloadHandler(
      filename = function() {
        sprintf("nemeton-health-validation-%s.qgz",
                format(Sys.time(), "%Y%m%d-%H%M%S"))
      },
      content = function(file) {
        i18n <- i18n_r()
        con <- get_monitoring_db_connection(project = app_state$current_project,
                                            read_only = TRUE)
        on.exit(close_monitoring_db_connection(con), add = TRUE)
        zone <- input$zone_id
        a <- list_alerts_for_zone(
          con, as.integer(zone),
          classes           = c("1-faible", "2-moyenne",
                                "3-forte",  "4-sol-nu"),
          validation_status = "pending"
        )
        if (is.null(a) || !nrow(a)) {
          shiny::showNotification(i18n$t("monitoring_qgis_no_alerts"),
                                  type = "warning", duration = 6)
          file.create(file)
          return(invisible())
        }
        plots <- nemeton::generate_health_validation_plots(
          a,
          n      = as.integer(input$qgis_n %||% 30L),
          method = input$qgis_method %||% "grts",
          crs    = 2154
        )
        out_dir <- tempfile("qgis-health-")
        dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
        qgz <- nemeton::create_qfield_project(
          plots,
          output_dir   = out_dir,
          project_name = "health-validation",
          region       = "BFC",
          lang         = app_state$language %||% "fr"
        )
        file.copy(qgz, file, overwrite = TRUE)
        shiny::showNotification(
          sprintf(i18n$t("monitoring_qgis_generated"), nrow(plots)),
          type = "message", duration = 6
        )
      }
    )

    # Zones reactive: open a fresh connection, list, close. Re-runs on
    # session start, on demand via zones_refresh(), and whenever the
    # active project changes (each project may have its own local
    # SQLite DB, and even in shared Postgres mode we want a fresh
    # list in case zones were added externally).
    #
    # v0.36.2 — explicit `proj <-` read to force the dep on
    # `app_state$current_project`. Reading via the lazy `project =`
    # function argument was insufficient: in Postgres mode
    # `.resolve_monitoring_db_url()` returns early on the
    # `NEMETON_DB_URL` envvar without ever forcing the promise, so the
    # reactive missed its dep on `current_project` and never re-fetched
    # on project switch. In local SQLite mode the bug was masked
    # because the resolver does end up touching `project$path`.
    zones_refresh <- shiny::reactiveVal(0L)

    # v0.73.0 (spec 020) — Le sélecteur ne liste plus TOUTES les zones
    # de la base (`list_monitoring_zones`) mais UNIQUEMENT celles du
    # projet courant (`nemeton::find_zones_by_project(con,
    # project_uuid)`). Avant ce bump, charger un projet sans zone
    # propre faisait retomber le menu sur la 1ʳᵉ zone alphabétique
    # de la base (ex. « villards ») — l'utilisateur voyait des alertes
    # d'un AUTRE projet sans s'en rendre compte. Désormais le menu
    # est vide tant que `build_project_monitoring_zones()` n'a pas
    # créé les 4 strates `_tot/_feu/_res/_mix` du projet courant.
    zones <- shiny::reactive({
      zones_refresh()                       # explicit refresh trigger
      proj <- app_state$current_project     # explicit dep on project
      if (is.null(proj) || is.null(proj$id)) {
        return(data.frame(id = integer(0), name = character(0),
                          stringsAsFactors = FALSE))
      }
      con <- get_monitoring_db_connection(project = proj, read_only = TRUE)
      on.exit(close_monitoring_db_connection(con), add = TRUE)
      if (is.null(con)) {
        return(data.frame(id = integer(0), name = character(0),
                          stringsAsFactors = FALSE))
      }
      tryCatch(
        nemeton::find_zones_by_project(con, project_uuid = proj$id),
        error = function(e) {
          cli::cli_alert_warning(
            "find_zones_by_project failed: {e$message}")
          data.frame(id = integer(0), name = character(0),
                     stringsAsFactors = FALSE)
        }
      )
    })

    # Push zones into the selectInput. Empty list → empty choices, the
    # status card explains why. If the loaded project was previously
    # registered as a zone (`metadata$monitoring_zone_id`), pre-select
    # that zone instead of the first alphabetic one — saves the user a
    # click on every project re-open.
    #
    # v0.36.2 — when the active project has NO `monitoring_zone_id`
    # (fresh project, zone never registered), clear the selection
    # rather than falling back to `choices[1]`. Pre-selecting the first
    # alphabetic zone was misleading because that zone belongs to
    # *another* project and the user would see its alerts / pixel-map
    # data thinking they were for the new project. An empty selection
    # propagates correctly through downstream reactives (`alerts`,
    # `validity`, `obs_pixel_data`, FAST/FORDEAD modules) which all
    # bail on `!nzchar(input$zone_id)`. The user registers a zone via
    # the dedicated button to bind it to this project.
    shiny::observe({
      z <- zones()
      choices <- if (nrow(z)) stats::setNames(as.character(z$id), z$name)
                 else character(0)

      preferred <- character(0)
      # v0.73.0 (spec 020) — Stratégie de sélection par défaut :
      # 1. La zone `_tot` du projet courant (toujours présente après
      #    `build_project_monitoring_zones`, contient l'union de
      #    toutes les UGFs sans filtre essence).
      # 2. Sinon, le `monitoring_zone_id` mémorisé dans les
      #    métadonnées du projet (back-compat avant spec 020 où
      #    une seule zone était créée par projet).
      # 3. Sinon, vide (selected = "") — le bandeau « générer les
      #    zones » apparaîtra côté UI.
      if (nrow(z)) {
        # Cherche la zone dont le nom se termine par `_tot`
        # (convention spec 020, indépendante du slug projet).
        tot_idx <- grep("_tot$", as.character(z$name))
        if (length(tot_idx) >= 1L) {
          preferred <- as.character(z$id[tot_idx[1]])
        }
      }
      if (!length(preferred)) {
        pmeta_id <- app_state$current_project$metadata$monitoring_zone_id
        if (!is.null(pmeta_id) && length(pmeta_id) == 1L) {
          candidate <- as.character(pmeta_id)
          if (candidate %in% choices) preferred <- candidate
        }
      }
      # v0.72.0 — `selected = ""` force explicitement la
      # non-sélection (le `character(0)` était interprété comme
      # « ne pas changer » par certaines combos Shiny).
      selected <- if (length(preferred)) preferred else ""

      shiny::updateSelectInput(session, "zone_id",
                               choices  = choices,
                               selected = selected)
    })

    # ----- FAST zone surfaces banner (v0.77.0) -----------------------
    # Surface (ha) + part (%) des 4 strates projet `_tot/_feu/_res/_mix`,
    # affichée au-dessus des sous-onglets FAST. Dépend de `zones()` (les
    # zones du projet courant) ; calcule l'aire de chaque polygone via
    # `get_monitoring_zone_aoi()` (EPSG:2154) + `sf::st_area`. NULL hors
    # mode quick ou quand aucune zone n'existe → le bandeau disparaît.
    fast_zone_surfaces <- shiny::reactive({
      if (!identical(input$mode %||% "quick", "quick")) return(NULL)
      z <- zones()
      if (is.null(z) || !nrow(z)) return(NULL)
      proj <- app_state$current_project
      con <- get_monitoring_db_connection(project = proj, read_only = TRUE)
      if (is.null(con)) return(NULL)
      on.exit(close_monitoring_db_connection(con), add = TRUE)
      .compute_zone_surfaces(con, z)
    })

    output$fast_zone_surfaces <- shiny::renderUI({
      df <- fast_zone_surfaces()
      if (is.null(df) || !nrow(df)) return(NULL)
      i18n <- i18n_r()
      # Un segment par strate présente, format « Label : X.X ha (NN %) ».
      items <- lapply(seq_len(nrow(df)), function(i) {
        label <- i18n$t(df$label_key[i])
        txt <- if (identical(df$strata[i], "tot") || is.na(df$pct[i])) {
          sprintf(i18n$t("monitoring_fast_surf_item_tot"),
                  label, df$ha[i])
        } else {
          sprintf(i18n$t("monitoring_fast_surf_item"),
                  label, df$ha[i], df$pct[i])
        }
        htmltools::tags$span(class = "badge bg-light text-dark border me-2", txt)
      })
      # v0.77.1 — Bloc en carte (symétrie avec les bandeaux de validité
      # FORDEAD « Composition d'essences… »), bordure info bleue : icône
      # + titre en gras, surfaces des strates dans le corps.
      htmltools::tags$div(
        class = "card border-info mb-3",
        htmltools::tags$div(
          class = "card-body py-2",
          htmltools::div(
            class = "d-flex align-items-start",
            bsicons::bs_icon("rulers", class = "me-2 fs-4 text-info"),
            htmltools::tags$div(
              htmltools::tags$strong(i18n$t("monitoring_fast_surfaces_title")),
              htmltools::tags$div(
                class = "small d-flex flex-wrap gap-2 mt-1", items)
            )
          )
        )
      )
    })
    shiny::outputOptions(output, "fast_zone_surfaces",
                         suspendWhenHidden = FALSE)

    # ----- Register-this-project-as-zone bridge ----------------------

    # Reactive: does the loaded project have a persisted sampling plan
    # on disk? Reads <project>/data/samples.gpkg directly so this also
    # works after an app restart. Bumped via `app_state$samples_refresh`
    # whenever mod_sampling writes a new plan, so the button enables
    # without forcing a project reload.
    samples_present <- shiny::reactive({
      app_state$samples_refresh  # invalidate dependency on sampling save
      project <- app_state$current_project
      if (is.null(project) || is.null(project$id)) return(FALSE)
      pp <- get_project_path(project$id)
      if (is.null(pp)) return(FALSE)
      file.exists(file.path(pp, "data", "samples.gpkg"))
    })

    register_running <- shiny::reactiveVal(FALSE)

    # Button state: enabled iff project is loaded, samples exist on
    # disk, the DB is configured, and no registration is in flight.
    shiny::observe({
      has_project <- !is.null(app_state$current_project) &&
                     !is.null(app_state$current_project$id)
      has_db <- {
        con <- get_monitoring_db_connection(project = app_state$current_project,
                                            read_only = TRUE)
        on.exit(close_monitoring_db_connection(con), add = TRUE)
        !is.null(con)
      }
      shiny::updateActionButton(
        session, "register",
        disabled = !has_project || !samples_present() ||
                   !has_db || isTRUE(register_running())
      )
    })

    # Hint surfaces the first failing gate so the user knows what to
    # do (load a project / generate a sampling plan / configure the
    # monitoring DB). Hidden when the button is enabled.
    output$register_hint <- shiny::renderUI({
      i18n <- i18n_r()
      has_project <- !is.null(app_state$current_project) &&
                     !is.null(app_state$current_project$id)
      if (!has_project) {
        return(htmltools::tags$small(class = "text-danger d-block",
          i18n$t("monitoring_register_no_project")))
      }
      if (!samples_present()) {
        return(htmltools::tags$small(class = "text-danger d-block",
          i18n$t("monitoring_register_no_samples")))
      }
      # Distinguish "RSQLite pkg missing" from "PG configured but
      # unreachable" so the user gets an actionable hint instead of
      # the generic "non configurée" message.
      backend <- monitoring_db_backend(project = app_state$current_project)
      if (identical(backend, "none")) {
        return(htmltools::tags$small(class = "text-danger d-block",
          i18n$t("monitoring_db_local_pkg_missing")))
      }
      con <- get_monitoring_db_connection(project = app_state$current_project,
                                          read_only = TRUE)
      on.exit(close_monitoring_db_connection(con), add = TRUE)
      if (is.null(con)) {
        return(htmltools::tags$small(class = "text-danger d-block",
          i18n$t("monitoring_register_no_db")))
      }
      NULL
    })

    # v0.73.0 (spec 020) — Click handler refactoré.
    #
    # Avant : `register_project_as_zone(con, project)` créait UNE
    # zone à partir des placettes (`samples.gpkg`). Le sélecteur
    # multi-projets pouvait alors croiser des zones de plusieurs
    # projets (bug villards/Mouthe).
    #
    # Maintenant : `nemeton::build_project_monitoring_zones()` crée
    # jusqu'à 4 zones (`_tot/_feu/_res/_mix`) par croisement de
    # l'union des UGFs avec les strates BD Forêt v2. La fonction
    # core est en `upsert` (`replace = TRUE` par défaut) → un re-clic
    # supprime puis recrée les zones du projet (les `zone_id`
    # changent ; `prune_orphan_zone_caches()` nettoie les anciens
    # dossiers `cache/layers/*/zone_<old_id>/` orphelins).
    #
    # Inputs cœur :
    #   * `project_name = project$metadata$name`  (ex. "Mouthe" ;
    #     le nom vit dans `metadata`, pas au 1er niveau de l'objet
    #     projet — fallback `project$id` si jamais absent)
    #   * `project_uuid = project$id`           (`<ts>_<rand>` = id local)
    #   * `ugf = ug_build_sf(project)`          (sf des UGFs)
    #   * `bdforet = sf::st_read(<proj>/cache/layers/bdforet.gpkg)`
    #     Si absent → message « lancer le calcul du projet d'abord »
    #     (ne PAS appeler la fonction).
    shiny::observe({
      i18n <- i18n_r()
      project <- app_state$current_project
      if (is.null(project) || is.null(project$id)) {
        shiny::showNotification(i18n$t("monitoring_register_no_project"),
                                type = "warning", duration = 4)
        return()
      }
      # Pré-requis BD Forêt : produite au 1er calcul projet via
      # `download_ign_bdforet`. Sans ce GPKG, le croisement avec les
      # strates BD Forêt v2 est impossible.
      bdforet_gpkg <- file.path(project$path, "cache", "layers",
                                "bdforet.gpkg")
      if (!file.exists(bdforet_gpkg)) {
        shiny::showNotification(
          i18n$t("zones_bdforet_missing"),
          type = "warning", duration = 8
        )
        return()
      }
      # Pré-requis UGFs : ug_build_sf renvoie NULL quand le projet
      # n'a pas d'UGF défini (ex. nouveau projet).
      ugf_sf <- tryCatch(ug_build_sf(project), error = function(e) NULL)
      if (is.null(ugf_sf) || nrow(ugf_sf) == 0L) {
        shiny::showNotification(
          i18n$t("monitoring_register_no_samples"),
          type = "warning", duration = 5
        )
        return()
      }
      con <- get_monitoring_db_connection(project = app_state$current_project)
      if (is.null(con)) {
        shiny::showNotification(i18n$t("monitoring_register_no_db"),
                                type = "error", duration = 5)
        return()
      }
      on.exit(close_monitoring_db_connection(con), add = TRUE)

      register_running(TRUE)
      shiny::showNotification(i18n$t("monitoring_register_running"),
                              type = "message", duration = 3,
                              id = session$ns("register_running_notif"))

      bdforet_sf <- tryCatch(
        sf::st_read(bdforet_gpkg, quiet = TRUE),
        error = function(e) NULL
      )
      if (is.null(bdforet_sf)) {
        register_running(FALSE)
        shiny::showNotification(
          sprintf("%s : %s",
                  i18n$t("monitoring_register_error"),
                  "BD Forêt GPKG illisible"),
          type = "error", duration = 8
        )
        return()
      }

      result <- tryCatch(
        nemeton::build_project_monitoring_zones(
          con,
          project_name = project$metadata$name %||% project$id,
          project_uuid = project$id,
          ugf          = ugf_sf,
          bdforet      = bdforet_sf
          # strata=c("tot","feu","res","mix"), replace=TRUE (defaults)
        ),
        error = function(e) e
      )
      register_running(FALSE)

      if (inherits(result, "error")) {
        shiny::showNotification(
          sprintf("%s : %s", i18n$t("monitoring_register_error"),
                  conditionMessage(result)),
          type = "error", duration = 8
        )
        return()
      }

      # v0.73.0 — Best-effort : purger les caches orphelins
      # `cache/layers/*/zone_<old_id>/` (les zone_id changent à
      # chaque upsert, donc les anciens dossiers traînent sinon).
      tryCatch(
        nemeton::prune_orphan_zone_caches(
          con,
          cache_root = file.path(project$path, "cache", "layers")
        ),
        error = function(e) {
          cli::cli_alert_warning(
            "prune_orphan_zone_caches failed: {e$message}")
        }
      )

      # Mémoriser la zone `_tot` (la plus inclusive) comme zone
      # par défaut du projet — l'observer du sélecteur s'aligne
      # déjà sur cette convention via `grep(\"_tot$\", z$name)`.
      tot_id <- result$tot %||% NA_integer_
      if (!is.na(tot_id)) {
        project$metadata$monitoring_zone_id <- tot_id
        app_state$current_project <- project
      }
      zones_refresh(zones_refresh() + 1L)

      # Récap : strates créées (clés non-NULL du résultat) + strates
      # vides (clés manquantes, ex. projet 100% feuillu → `res` absent).
      built_strata <- names(result)[!vapply(
        result, is.null, logical(1))]
      n_built <- length(built_strata)
      shiny::showNotification(
        sprintf(i18n$t("zones_build_success_fmt"),
                n_built,
                paste(built_strata, collapse = ", ")),
        type = "message", duration = 6
      )
    }) |>
      shiny::bindEvent(input$register, input$register_inline,
                       ignoreInit = TRUE)

    # DB status card — seven states:
    #   0. Async probe still running        → spinning gear card
    #   1. No PG env AND no project         → "open a project" hint (warning)
    #   2. No PG env, project loaded, RSQLite pkg missing → install hint (warning)
    #   3. Probe failed (server down, bad URL, migration error, …) → warning + real message
    #   4. Local SQLite ready                → info banner with multi-user hint
    #   5. Postgres connected, zero zones    → info
    #   6. Postgres or SQLite ready, zones available → success
    #
    # The probe (ExtendedTask) is fired by the observer above whenever
    # the project changes. While it runs, the spinning gear card stays
    # visible — no toast race, no perceived freeze, no busy overlay.
    output$db_status <- shiny::renderUI({
      i18n     <- i18n_r()
      project  <- app_state$current_project
      backend  <- monitoring_db_backend(project = project)

      # Diagnose first WITHOUT opening a connection, so we can pick
      # the right user-facing message even when the connection itself
      # would later fail.
      if (identical(backend, "none")) {
        if (is.null(project)) {
          return(.monitoring_status_card(
            icon  = "folder2-open",
            class = "border-warning",
            title = i18n$t("monitoring_db_unavailable"),
            body  = i18n$t("monitoring_db_no_project")
          ))
        }
        # Project loaded, PG env absent, and not classified as a local
        # file backend means RSQLite is missing (otherwise the resolver
        # would have produced a sqlite://… URL).
        return(.monitoring_status_card(
          icon  = "exclamation-circle",
          class = "border-warning",
          title = i18n$t("monitoring_db_unavailable"),
          body  = i18n$t("monitoring_db_local_pkg_missing")
        ))
      }

      # Probe still running or not yet fired — show the spinning gear.
      st <- db_probe_state()
      if (identical(st$state, "initial") || identical(st$state, "running")) {
        title <- if (identical(backend, "local"))
                   i18n$t("monitoring_db_local_creating")
                 else
                   i18n$t("monitoring_db_connecting")
        return(.monitoring_loading_card(
          icon  = "gear-fill",
          title = title,
          body  = i18n$t("monitoring_db_loading_hint")
        ))
      }

      # Probe failed — surface the real cause captured in the worker
      # (db_connect or migration error) so the user can act.
      if (identical(st$state, "error")) {
        err <- st$error
        body <- if (!is.null(err) && nzchar(err) && !identical(err, "no_url")) {
          paste0(i18n$t("monitoring_db_check_env"), " — ", err)
        } else {
          i18n$t("monitoring_db_check_env")
        }
        return(.monitoring_status_card(
          icon  = "exclamation-circle",
          class = "border-warning",
          title = i18n$t("monitoring_db_unavailable"),
          body  = body
        ))
      }

      # Probe succeeded — open a sync connection in the main process to
      # count zones for the success banner. Schema is already migrated
      # so this is a fast handshake (< 100 ms typical). v0.48.2 —
      # read_only : un simple COUNT, ne doit pas tenir un verrou RW
      # qui bloquerait le worker d'ingestion.
      con <- get_monitoring_db_connection(project = project,
                                          read_only = TRUE)
      on.exit(close_monitoring_db_connection(con), add = TRUE)
      if (is.null(con)) {
        err <- last_monitoring_db_error()
        body <- if (!is.null(err) && nzchar(err)) {
          paste0(i18n$t("monitoring_db_check_env"), " — ", err)
        } else {
          i18n$t("monitoring_db_check_env")
        }
        return(.monitoring_status_card(
          icon  = "exclamation-circle",
          class = "border-warning",
          title = i18n$t("monitoring_db_unavailable"),
          body  = body
        ))
      }
      n <- nrow(zones())
      # Local SQLite mode gets its own bandeau so the user sees it is
      # a single-user fallback (not a misconfigured Postgres). Hint
      # mentions how to switch to Postgres for multi-user setups.
      if (identical(backend, "local")) {
        body <- if (n == 0L) {
          paste(i18n$t("monitoring_zone_register_hint"),
                i18n$t("monitoring_db_local_hint"), sep = " — ")
        } else {
          paste(sprintf(i18n$t("monitoring_db_connected"), n),
                i18n$t("monitoring_db_local_hint"), sep = " — ")
        }
        return(.monitoring_status_card(
          icon  = "database",
          class = "border-info",
          title = i18n$t("monitoring_db_local"),
          body  = body
        ))
      }
      if (n == 0L) {
        # v0.52.7 — Bouton d'action INLINE dans le bandeau. Le bouton
        # sidebar `input$register` existe déjà, mais il vit sous la
        # zone "Mode de suivi" et tombe souvent sous le pli sur les
        # écrans courants → l'utilisateur voit le message mais pas
        # l'action. On rajoute donc un bouton dans le bandeau lui-même
        # qui appelle la même logique (via `input$register_inline` ;
        # observer ci-dessous est un alias de `input$register`).
        return(.monitoring_status_card(
          icon  = "info-circle",
          class = "border-info",
          title = i18n$t("monitoring_zone_none"),
          body  = htmltools::tagList(
            htmltools::tags$div(i18n$t("monitoring_zone_register_hint")),
            htmltools::tags$div(
              class = "mt-2",
              shiny::actionButton(
                session$ns("register_inline"),
                label = i18n$t("monitoring_register_btn"),
                icon  = shiny::icon("plus-circle"),
                class = "btn-primary btn-sm"
              )
            )
          )
        ))
      }
      # v0.52.5 — Détection de l'état « orphelin » : la DB contient
      # des zones (n > 0) mais aucune n'est rattachée au projet
      # chargé. Symptôme typique d'un wipe par les tests cœur
      # (`helper-monitoring.R` ligne 82-88 DROP CASCADE 7 tables
      # monitoring sans garde-fou — incident villards 2026-05-31).
      # Sans cette détection, l'utilisateur voyait le bandeau de
      # succès vert « N zone(s) connectée(s) » alors qu'aucune ne
      # lui appartient, le dropdown restait vide, et FAST plantait
      # plus tard en FK violation « plot_id=2 not in plot ». Avec
      # ce bandeau jaune-warning, l'utilisateur sait quoi faire :
      # cliquer sur « Enregistrer ce projet comme zone de suivi »
      # dans la barre latérale (le bouton existe déjà, on ne
      # duplique pas l'action — on guide).
      #
      # Détection : on lit la colonne `project_uuid` de
      # `monitoring_zone` (ajoutée par migration 0003) et on vérifie
      # qu'au moins une zone porte l'id du projet courant. Si
      # toutes les zones ont `project_uuid IS NULL` (stubs de tests)
      # ou correspondent à d'AUTRES projets, on bascule sur le
      # warning. Requête en best-effort : si la colonne n'existe
      # pas encore (migration 0003 non appliquée), on retombe sur
      # le banner de succès classique.
      if (!is.null(project) && !is.null(project$id)) {
        zones_pu <- tryCatch(
          DBI::dbGetQuery(
            con,
            "SELECT project_uuid FROM monitoring_zone WHERE project_uuid IS NOT NULL"
          )$project_uuid,
          error = function(e) NULL
        )
        if (!is.null(zones_pu) && !(project$id %in% zones_pu)) {
          # v0.52.7 — Même logique que la branche n==0 : bouton
          # d'action INLINE dans le bandeau orphelin (le bouton
          # sidebar existant tombe souvent sous le pli).
          return(.monitoring_status_card(
            icon  = "exclamation-triangle-fill",
            class = "border-warning",
            title = i18n$t("monitoring_zone_orphan_title"),
            body  = htmltools::tagList(
              htmltools::tags$div(
                sprintf(i18n$t("monitoring_zone_orphan_body"), n)),
              htmltools::tags$div(
                class = "mt-2",
                shiny::actionButton(
                  session$ns("register_inline"),
                  label = i18n$t("monitoring_register_btn"),
                  icon  = shiny::icon("plus-circle"),
                  class = "btn-warning btn-sm"
                )
              )
            )
          ))
        }
      }
      .monitoring_status_card(
        icon  = "check-circle",
        class = "border-success",
        title = sprintf(i18n$t("monitoring_db_connected"), n),
        body  = NULL
      )
    })

    # ----- Async ingestion (phase 2) ----------------------------------

    # Surface the active Sentinel-2 COG cache path below the Run button
    # so the user knows where bands are persisted between runs (and so
    # they can wipe it from the filesystem if they need to). NULL when
    # no project is open — in that case nemeton falls back to its
    # in-memory legacy path and we display nothing.
    output$cache_hint <- shiny::renderUI({
      i18n <- i18n_r()
      cache_dir <- .resolve_s2_cache_dir(app_state$current_project)
      if (is.null(cache_dir)) return(NULL)
      htmltools::tags$div(
        title = sprintf(i18n$t("monitoring_cache_active_fmt"), cache_dir),
        sprintf(i18n$t("monitoring_cache_active_fmt"), cache_dir)
      )
    })

    fast_task <- run_ingestion_async()

    # Path to the progress.json file written by the ingest worker via
    # the nemeton progress_callback API (>= 0.21.2). NULL until the
    # first invoke. The reactivePoll below watches this path and the
    # toast observer re-renders the persistent "X/N tuiles..." card.
    ingest_progress_path <- shiny::reactiveVal(NULL)
    # v0.70.4 — Garde contre le re-traitement du result du worker
    # FAST. Shiny ExtendedTask$result() peut, dans certains cycles
    # de vie (cascade reactive, transition status, etc.), refire
    # plusieurs fois pour le MÊME result. Sans garde, l'observer
    # de fin de worker (l.~2020) ré-émet le toast
    # `monitoring_ingest_success` (duration = 6 s) → l'utilisateur
    # voit le toast apparaître, disparaître après 6 s, ré-apparaître,
    # plusieurs fois. Le flag est reset à FALSE à chaque
    # `fast_task$invoke()` (cf. observeEvent input$run).
    fast_result_consumed <- shiny::reactiveVal(FALSE)
    # v0.71.1 — Parité FORDEAD : même garde d'idempotence pour
    # `fordead_task$result()`. Symétrique à `fast_result_consumed`
    # (v0.70.4). Sans cela, le toast `fordead_success` est ré-émis
    # plusieurs fois (clignote) ET le bouton « Lancer le diagnostic
    # FORDEAD » peut rester grisé apparemment (l'observer fire en
    # boucle mais le `removeNotification`/`force_unlock_health` est
    # correctement ré-appliqué).
    fordead_result_consumed <- shiny::reactiveVal(FALSE)

    # Path to the worker console log. The worker `sink()`s its stdout
    # and message stream to this file (see service_monitoring.R) so we
    # can tail it from the parent and mirror every `cli::cli_*` line
    # from nemeton into the developer's R console — including the
    # CACHE-DEBUG traces (`NEMETON_S2_CACHE_DEBUG=TRUE`) that nemeton
    # emits inside `.get_s2_band_raster`. Offset tracks how much has
    # already been streamed so we only print new content per tick.
    ingest_log_path   <- shiny::reactiveVal(NULL)
    ingest_log_offset <- shiny::reactiveVal(0L)

    ingest_progress <- shiny::reactivePoll(
      intervalMillis = 500L, session,
      checkFunc = function() {
        p <- ingest_progress_path()
        if (is.null(p) || !file.exists(p)) return("0")
        as.character(file.info(p)$mtime)
      },
      valueFunc = function() {
        p <- ingest_progress_path()
        if (is.null(p) || !file.exists(p)) return(NULL)
        tryCatch(
          jsonlite::read_json(p, simplifyVector = TRUE),
          error = function(e) NULL
        )
      }
    )

    # Tail the worker console log: every 500 ms, check the file size;
    # if it grew since the previous tick, read the new bytes and write
    # them straight to the parent's stdout via cat(). This bypasses
    # `future`'s built-in stdout capture (which would only release the
    # output at task completion) and gives the developer line-by-line
    # tracing in real time — exactly what running `run_app()` from a
    # terminal expects.
    ingest_log_tick <- shiny::reactivePoll(
      intervalMillis = 500L, session,
      checkFunc = function() {
        p <- ingest_log_path()
        if (is.null(p) || !file.exists(p)) return("0")
        as.character(file.info(p)$size)
      },
      valueFunc = function() {
        p <- ingest_log_path()
        if (is.null(p) || !file.exists(p)) return(NULL)
        sz <- as.numeric(file.info(p)$size)
        if (is.na(sz) || sz <= 0) return(NULL)
        off <- as.numeric(ingest_log_offset())
        if (sz <= off) return(NULL)
        chunk <- tryCatch({
          con <- file(p, open = "rb")
          on.exit(close(con), add = TRUE)
          if (off > 0) seek(con, where = off, origin = "start")
          raw <- readBin(con, what = "raw", n = sz - off)
          rawToChar(raw)
        }, error = function(e) "")
        ingest_log_offset(as.integer(sz))
        chunk
      }
    )

    # Forward newly-tailed bytes to the parent console. Single line of
    # cat() per tick keeps the worker's own newlines intact (we don't
    # re-wrap with cli::cli_*).
    shiny::observe({
      chunk <- ingest_log_tick()
      if (is.null(chunk) || !nzchar(chunk)) return()
      cat(chunk, file = stderr())
    })

    # ------------------------------------------------------------------
    # v0.70.0 — Drain NDJSON append-only (brief logs FAST propres).
    # ------------------------------------------------------------------
    # Le worker écrit en parallèle 2 fichiers :
    #   * `<progress_path>.json`    : DERNIER event (replace), pilote le
    #     toast Shiny coalescé (observer `ingest_progress` plus haut).
    #   * `<progress_path>.ndjson`  : journal APPEND-ONLY, une ligne par
    #     event, jamais écrasé. Drainé ici par offset (même pattern que
    #     `ingest_log_tick`), garantit l'ordre 1→120 sans saut ni
    #     entrelacement pour le mirror console des `.log_band_event` /
    #     `.log_ingest_event`.
    #
    # Avant v0.70.0, le mirror console reposait uniquement sur le JSON
    # « dernier event » → tick polling 500 ms, worker émettant 4-5 events
    # par scène en < 50 ms → la quasi-totalité des events était écrasée
    # avant le poll suivant. Cause des « sauts » 1/120 → 3 → 23 → 51 et
    # de la désynchro Tuile / Bande affichées (cf. brief).
    ingest_ndjson_path   <- shiny::reactive({
      p <- ingest_progress_path()
      if (is.null(p) || !nzchar(p)) return(NULL)
      if (grepl("\\.json$", p)) sub("\\.json$", ".ndjson", p)
      else paste0(p, ".ndjson")
    })
    ingest_ndjson_offset <- shiny::reactiveVal(0L)

    ingest_ndjson_lines <- shiny::reactivePoll(
      intervalMillis = 300L, session,
      checkFunc = function() {
        p <- ingest_ndjson_path()
        if (is.null(p) || !file.exists(p)) return("0")
        as.character(file.info(p)$size)
      },
      valueFunc = function() {
        p <- ingest_ndjson_path()
        if (is.null(p) || !file.exists(p)) return(NULL)
        sz <- as.numeric(file.info(p)$size)
        if (is.na(sz) || sz <= 0) return(NULL)
        off <- as.numeric(ingest_ndjson_offset())
        if (sz <= off) return(NULL)
        txt <- tryCatch({
          con <- file(p, open = "rb")
          on.exit(close(con), add = TRUE)
          if (off > 0) seek(con, where = off, origin = "start")
          raw <- readBin(con, what = "raw", n = sz - off)
          rawToChar(raw)
        }, error = function(e) "")
        ingest_ndjson_offset(as.integer(sz))
        if (!nzchar(txt)) return(NULL)
        Filter(nzchar, strsplit(txt, "\n", fixed = TRUE)[[1]])
      }
    )

    # Mirror console : itère sur CHAQUE event NDJSON dans l'ordre.
    # Seule responsabilité = lignes `Tuile X/N` / `⤷ Bande …` —
    # le toast Shiny reste piloté par le JSON dernier-event.
    shiny::observe({
      lines <- ingest_ndjson_lines()
      if (is.null(lines) || !length(lines)) return()
      for (ln in lines) {
        ev <- tryCatch(jsonlite::fromJSON(ln), error = function(e) NULL)
        if (is.null(ev)) next
        current_phase <- as.character(ev$current %||% "")
        # Bandes : mirror systématique (volume principal du flux).
        if (current_phase %in% c("s2:band_cached", "s2:band_fetched")) {
          .log_band_event(ev, current_phase)
          next
        }
        # Scènes : Tuile X/N (les events scene_id / completed / total
        # passent dans .log_ingest_event).
        if (current_phase %in% c("s2:scene", "s2:scene_cached")) {
          status <- ev$status %||% "running"
          i_val  <- as.integer(ev$completed %||% ev$i %||% 0L)
          n_val  <- as.integer(ev$total     %||% ev$n %||% 0L)
          scene  <- as.character(ev$scene_id %||% "")
          .log_ingest_event(ev, status, i_val, n_val, scene)
        }
      }
    })

    # Render the persistent progress toast. Each event from the worker
    # replaces the previous one (same notification id) so we don't
    # stack 50 toasts during a long ingestion. The notification body
    # is built as HTML so we can prepend a spinning gear icon.
    #
    # nemeton@v0.21.2 emits payloads of the shape:
    #   { current: "s2:scene", completed: 0, total: 26,
    #     scene_id: "...", obs_date: "...", cloud_pct: 2.94,
    #     source: "pc" }
    # We also accept `i`/`n`/`status`/`phase` as fallbacks so the
    # observer survives a future schema rename without code changes.
    #
    # The observer also mirrors each event to the R console via
    # `cli::cli_alert_*` so a developer running the app from a
    # terminal sees the same per-scene progress as in the browser
    # (helpful for long ingestions and to spot scene-level errors).
    shiny::observe({
      ev <- ingest_progress()
      if (is.null(ev)) return()
      i18n <- i18n_r()
      status <- ev$status %||% "running"
      if (identical(status, "done")) return()
      current_phase <- as.character(ev$current %||% "")
      # Worker instrumentation events (v0.26.4+) — mirror them to the
      # console so developers see them in the terminal even when the
      # Shiny toast pipeline is debounced. Keep the persistent toast
      # updated so the user knows the worker reached each step.
      if (current_phase %in% c("s2:worker_started",
                               "s2:nemeton_call_starting")) {
        cli::cli_alert_info("Worker event: {current_phase}")
        shiny::showNotification(
          .monitoring_spinning_msg(
            sprintf(i18n$t("monitoring_ingest_worker_event_fmt"),
                    current_phase)
          ),
          id          = session$ns("ingest_progress"),
          type        = "message",
          duration    = NULL,
          closeButton = FALSE
        )
        return()
      }
      # Fatal-error event surfaced by the worker BEFORE the future
      # rejects — gives us a real R error message instead of
      # "MultisessionFuture was interrupted".
      if (identical(current_phase, "s2:fatal_error")) {
        msg <- as.character(ev$error_message %||% "(unknown)")
        cls <- as.character(ev$error_class %||% "")
        cli::cli_alert_danger("Worker fatal error ({cls}): {msg}")
        shiny::showModal(shiny::modalDialog(
          title = i18n$t("monitoring_ingest_fatal_title"),
          shiny::tags$pre(msg),
          if (nzchar(cls)) shiny::tags$p(shiny::tags$small(cls)),
          easyClose = TRUE,
          footer    = shiny::modalButton(i18n$t("close"))
        ))
        return()
      }
      # Band-level success events fire sub-second per scene (2-4 bands
      # per scene) — letting them rewrite the toast would make it
      # flicker and lose the scene-level context.
      # v0.70.0 — Le mirror console (`.log_band_event`) passe désormais
      # par le drain NDJSON (cf. observer dédié plus haut), garanti
      # complet et ordonné. Ici on absorbe l'event sans rien faire
      # côté toast (le toast reste sur le scene-level state précédent).
      if (current_phase %in% c("s2:band_cached", "s2:band_fetched")) {
        return()
      }
      # Band-level FAILURE bubbles up as a non-persistent warning toast
      # (separate id so it stacks alongside the scene-level toast
      # instead of replacing it).
      if (identical(current_phase, "s2:band_fetch_failed")) {
        shiny::showNotification(
          sprintf(i18n$t("monitoring_ingest_band_failed_fmt"),
                  as.character(ev$band %||% "?"),
                  as.character(ev$error_message %||% ev$error %||% "")),
          id          = session$ns("ingest_band_failed"),
          type        = "warning",
          duration    = 6
        )
        return()
      }
      # v0.55.0 — Events `fast_prewarm:<index>_<mode>[ _done|_failed]`
      # émis par le cœur en fin d'ingestion réussie (spec 018
      # nemeton@v0.61.0). On les transforme en toasts localisés à
      # partir des CLÉS MACHINE du payload (index / mode), pas en
      # parsant du texte FR. Mapping mode → libellé i18n :
      #   "count"   → fast_mode_frequence  (Fréquence / Frequency)
      #   "rolling" → fast_mode_intensite  (Intensité / Intensity)
      if (startsWith(current_phase, "fast_prewarm:")) {
        if (identical(current_phase, "fast_prewarm:complete")) {
          # v0.70.1 — Bug observé : sans `removeNotification`, le
          # toast running `fast_prewarm_progress` (id stable,
          # `duration = NULL`, persistent — cf. branche running plus
          # bas l.1601) survivait à la fin du worker. Conséquence
          # UX : « Pré-calcul carte NDMI Intensité en cours… »
          # restait collé en bas à droite alors que le worker était
          # terminé. On retire explicitement le toast running ici,
          # puis on émet un toast court (4 s) confirmant la
          # disponibilité de l'app.
          shiny::removeNotification(session$ns("fast_prewarm_progress"))
          shiny::showNotification(
            i18n$t("monitoring_fast_diagnostic_complete"),
            id       = session$ns("fast_prewarm_complete"),
            type     = "message",
            duration = 4
          )
          cli::cli_alert_info("Worker event: fast_prewarm:complete")
          return()
        }
        if (identical(current_phase, "fast_prewarm:cancelled")) {
          shiny::showNotification(
            i18n$t("fast_prewarm_cancelled"),
            id       = session$ns("fast_prewarm_cancelled"),
            type     = "warning",
            duration = 5
          )
          return()
        }
        idx_payload  <- as.character(ev$index %||% "")
        mode_payload <- as.character(ev$mode  %||% "")
        if (!nzchar(idx_payload) || !nzchar(mode_payload)) {
          # Payload malformé (cœur incohérent). On log et ignore.
          cli::cli_alert_info(
            "Worker event: {current_phase} (payload incomplet)")
          return()
        }
        mode_label <- if (identical(mode_payload, "count")) {
          i18n$t("fast_mode_frequence")
        } else if (identical(mode_payload, "rolling")) {
          i18n$t("fast_mode_intensite")
        } else mode_payload
        if (endsWith(current_phase, "_done")) {
          shiny::showNotification(
            sprintf(i18n$t("fast_prewarm_done"), idx_payload, mode_label),
            id       = session$ns(
              sprintf("fast_prewarm_done_%s_%s",
                      idx_payload, mode_payload)),
            type     = "message",
            duration = 4
          )
        } else if (endsWith(current_phase, "_failed")) {
          err <- as.character(ev$error_message %||% ev$error %||% "")
          msg <- sprintf(i18n$t("fast_prewarm_failed"),
                         idx_payload, mode_label)
          if (nzchar(err)) msg <- paste0(msg, " — ", err)
          shiny::showNotification(
            msg,
            id       = session$ns(
              sprintf("fast_prewarm_failed_%s_%s",
                      idx_payload, mode_payload)),
            type     = "warning",
            duration = 6
          )
        } else {
          # phase running (sans suffixe `_done`/`_failed`).
          shiny::showNotification(
            sprintf(i18n$t("fast_prewarm_running"),
                    idx_payload, mode_label),
            id          = session$ns("fast_prewarm_progress"),
            type        = "message",
            duration    = NULL,
            closeButton = FALSE
          )
        }
        return()
      }
      # PC SAS token rotation is informational only — we surface it so
      # the user can correlate a brief pause with token rotation
      # instead of suspecting a network issue.
      if (identical(current_phase, "s2:pc_token_refreshed")) {
        shiny::showNotification(
          i18n$t("monitoring_ingest_token_refreshed"),
          id       = session$ns("ingest_pc_token"),
          type     = "default",
          duration = 3
        )
        return()
      }
      # DB cache lookup result: helps the user understand why a run is
      # fast (lots of obs already cached, skip_cached=TRUE shortcuts).
      if (identical(current_phase, "s2:cache_lookup")) {
        n_cached     <- as.integer(ev$n_cached     %||% 0L)
        n_to_process <- as.integer(ev$n_to_process %||% ev$n_remaining %||% 0L)
        shiny::showNotification(
          .monitoring_spinning_msg(
            sprintf(i18n$t("monitoring_ingest_cache_lookup_fmt"),
                    n_cached, n_to_process)
          ),
          id          = session$ns("ingest_progress"),
          type        = "message",
          duration    = NULL,
          closeButton = FALSE
        )
        return()
      }
      # v0.70.3 — STAC search complete : initialise le toast à
      # (1/N) AVANT que le 1er event `s2:scene` ne soit capturé.
      # Sans ce handler, le polling 500 ms du JSON dernier-event
      # rate fréquemment la 1ʳᵉ scène (worker pousse `s2:scene
      # completed=0` puis `s2:scene completed=1` en bien moins
      # de 500 ms → le JSON est écrasé deux fois entre 2 polls →
      # le 1er event capturable était souvent `completed=1`,
      # affiché « (2/N) » après le `+1` de v0.70.2). Désormais
      # `(1/N) — démarrage…` apparaît dès `s2:search_done` ;
      # le 1er `s2:scene` capturé remplace le toast par le
      # numéro réel (qui peut sauter directement à `(3/N)`, mais
      # l'utilisateur a vu `(1/N)` au moins une fois).
      if (identical(current_phase, "s2:search_done")) {
        n_val_init <- as.integer(ev$total %||% ev$n %||% 0L)
        if (n_val_init > 0L) {
          shiny::showNotification(
            .monitoring_spinning_msg(
              sprintf(i18n$t("monitoring_ingest_search_done_fmt"),
                      1L, n_val_init)
            ),
            id          = session$ns("ingest_progress"),
            type        = "message",
            duration    = NULL,
            closeButton = FALSE
          )
        }
        return()
      }
      i_val <- as.integer(ev$completed %||% ev$i %||% 0L)
      n_val <- as.integer(ev$total     %||% ev$n %||% 0L)
      scene <- as.character(ev$scene_id %||% "")
      # When scene_id is empty AND completed == 0, we're between the
      # STAC search and the first tile — show "Recherche STAC..." so
      # the user doesn't see a misleading "Tuile Sentinel-2 (0/N)".
      msg <- if (identical(status, "starting")) {
        i18n$t("monitoring_ingest_starting")
      } else if (!nzchar(scene) && i_val == 0L) {
        if (n_val > 0L)
          sprintf(i18n$t("monitoring_stac_search_with_count_fmt"), n_val)
        else
          i18n$t("monitoring_stac_search")
      } else if (nzchar(scene)) {
        # Sentinel-2 scene IDs are 60+ chars (e.g.
        # S2A_MSIL2A_20250610T103631_N0500_R108_T31TFM_20250610T142536)
        # and overflow the ~250 px Shiny notification box. Wrap the
        # scene_id in a small monospace span so the full id stays
        # readable on two short lines instead of being truncated.
        scene_html <- sprintf(
          paste0("<span style=\"font-size:9px;",
                 "font-family:monospace;",
                 "word-break:break-all;\">%s</span>"),
          htmltools::htmlEscape(scene)
        )
        # v0.70.2 — `i_val + 1L` : compteur 1-based pour la tuile
        # en cours (brief console FAST, Partie B). La garde STAC
        # plus haut (l.1664) reste sur `i_val == 0L` brut.
        htmltools::HTML(sprintf(
          i18n$t("monitoring_ingest_progress_named_fmt"),
          scene_html, i_val + 1L, n_val
        ))
      } else {
        sprintf(i18n$t("monitoring_ingest_progress_fmt"),
                i_val + 1L, n_val)
      }
      # v0.70.0 — `.log_ingest_event` (mirror Tuile X/N) déménage
      # dans l'observer NDJSON drain (cf. plus haut). Ici on ne
      # touche plus qu'au toast Shiny (replacement par id stable).
      shiny::showNotification(
        .monitoring_spinning_msg(msg),
        id          = session$ns("ingest_progress"),
        type        = if (identical(status, "scene_error")) "warning" else "message",
        duration    = NULL,
        closeButton = FALSE
      )
    })

    # User-side override that force-unlocks the "Lancer" button even
    # when the worker is technically still running. Used by the
    # run_cancel button (see below): Shiny's ExtendedTask cannot be
    # cancelled, so when the worker is stuck looping over scenes that
    # all 403/404 (e.g. PC SAS token expired mid-run, Azure regional
    # outage), the user has no way to relaunch without restarting the
    # whole app. This flag lets them get a fresh "Lancer" button while
    # the orphan future continues in the background — that's safe
    # because the DB INSERTs are ON CONFLICT DO NOTHING.
    force_unlock_quick <- shiny::reactiveVal(FALSE)

    # Button state: grey out during a running ingestion (double-click
    # protection) AND the user has not overridden via run_cancel. The
    # observer fires every time `fast_task$status()` (a Shiny reactive)
    # transitions, which is the canonical way to track ExtendedTask
    # state.
    #
    # Cross-lock (v0.39.2): also greyed while a FORDEAD run is in
    # flight. FAST and FORDEAD share the project Sentinel-2 band cache
    # (`cache/layers/sentinel2/<scene>/<band>.tif`) — running both at
    # once risks two workers racing on the same `<band>.tif.tmp`. The
    # lock respects the *other* task's force-unlock: if FORDEAD was
    # abandoned via its cancel button, FAST is launchable again.
    shiny::observe({
      fast_running    <- identical(fast_task$status(), "running") &&
                         !isTRUE(force_unlock_quick())
      fordead_running <- identical(fordead_task$status(), "running") &&
                         !isTRUE(force_unlock_health())
      shiny::updateActionButton(session, "run",
                                disabled = fast_running || fordead_running)
    })

    # v0.70.1 — Filet de sécurité du toast `fast_prewarm_progress`
    # (persistent, `duration = NULL`). Si le cœur n'émet pas
    # `fast_prewarm:complete` à la fin (cas pathologique : hang
    # silencieux, exception non capturée côté prewarm, etc.), le
    # toast restait collé en bas à droite après que le worker s'est
    # déjà arrêté. Cet observer watch la transition de
    # `fast_task$status()` hors de "running" et nettoie le toast de
    # force — y compris si l'utilisateur a force-unlocked via le
    # bouton « Annuler le diagnostic ».
    shiny::observe({
      status <- fast_task$status()
      if (!identical(status, "running")) {
        shiny::removeNotification(session$ns("fast_prewarm_progress"))
      }
    })

    # Render the cancel/reset button only while a real task is in
    # flight AND the user hasn't already force-unlocked. The button is
    # secondary-styled to make clear it's a recovery action, not the
    # primary CTA.
    output$run_cancel_panel <- shiny::renderUI({
      ns <- session$ns
      i18n <- i18n_r()
      if (!identical(fast_task$status(), "running")) return(NULL)
      if (isTRUE(force_unlock_quick())) return(NULL)
      shiny::actionButton(
        ns("run_cancel"),
        i18n$t("monitoring_run_cancel_btn"),
        icon  = bsicons::bs_icon("x-circle"),
        class = "btn-outline-secondary btn-sm w-100 mt-1"
      )
    })

    shiny::observeEvent(input$run_cancel, {
      i18n <- i18n_r()
      # v0.52.0 — Vrai cancel coopératif. On écrit le flag AVANT le
      # force-unlock UI : le worker poll ce fichier entre chaque tuile
      # (nemeton@v0.53.0+) et sort proprement à la prochaine itération
      # avec un résumé `status = "cancelled"` (commit partiel des
      # tuiles déjà ingérées). Le force-unlock libère l'UI sans
      # attendre cette sortie.
      cancel_path <- .resolve_progress_path(
        app_state$current_project, "fast_cancel.flag")
      if (!is.null(cancel_path)) {
        tryCatch(file.create(cancel_path),
                 error = function(e) NULL)
      }
      # Force-unlock the button immediately so the user can relaunch.
      force_unlock_quick(TRUE)
      # Wipe every in-flight ingestion toast so the screen stops being
      # polluted by the old worker's stream of 403/404 warnings.
      for (nid in c("ingest_progress", "ingest_band_failed",
                    "ingest_pc_token", "ingest_error",
                    "ingest_warns", "ingest_zero", "ingest_success",
                    "ingest_cache_lookup")) {
        shiny::removeNotification(session$ns(nid))
      }
      # Cleanup local progress + log files so the next run starts on a
      # blank slate (poll observers will treat NULL as "no run").
      .cleanup_progress_file(ingest_progress_path())
      ingest_progress_path(NULL)
      .cleanup_progress_file(ingest_log_path())
      ingest_log_path(NULL)
      ingest_log_offset(0L)
      shiny::showNotification(
        i18n$t("monitoring_run_cancel_done"),
        id       = session$ns("run_cancel_info"),
        type     = "default",
        duration = 6
      )
    })

    # Click handler: validate state, build window dates, fire the task.
    shiny::observeEvent(input$run, {
      i18n <- i18n_r()
      # New manual launch: discard any prior force-unlock so the
      # button correctly greys out for the new worker.
      force_unlock_quick(FALSE)
      # v0.70.4 — Reset le flag idempotence : le prochain
      # `fast_task$result()` doit être traité (toast success /
      # error émis).
      fast_result_consumed(FALSE)
      # v0.52.0 — Purge un éventuel cancel_path résiduel d'un run
      # précédent (sinon, le worker abandonne dès la 1re tuile !).
      # Idempotent : `unlink` sur fichier absent retourne 0 sans erreur.
      .fast_cancel_flag <- .resolve_progress_path(
        app_state$current_project, "fast_cancel.flag")
      if (!is.null(.fast_cancel_flag) && file.exists(.fast_cancel_flag)) {
        tryCatch(unlink(.fast_cancel_flag, force = TRUE),
                 error = function(e) NULL)
      }
      # Cross-lock: refuse to start FAST while a FORDEAD run is in
      # flight (shared Sentinel-2 cache — cf. the run-button observer).
      # An abandoned FORDEAD run (cancel button → force-unlock) does
      # not block.
      if (identical(fordead_task$status(), "running") &&
          !isTRUE(force_unlock_health())) {
        shiny::showNotification(i18n$t("monitoring_busy_fordead"),
                                type = "warning", duration = 6)
        return()
      }
      if (identical(fast_task$status(), "running")) {
        shiny::showNotification(i18n$t("monitoring_ingest_starting"),
                                type = "message", duration = 4)
        return()
      }
      if (!isTRUE(nzchar(input$zone_id))) {
        shiny::showNotification(i18n$t("monitoring_validate_zone"),
                                type = "warning", duration = 4)
        return()
      }
      # v0.61.0 — `bands` est désormais câblé en dur (NDVI + NBR
      # systématiquement) lors du `fast_task$invoke()` plus bas.
      # La validation `length(input$bands) == 0L` est retirée.
      dr <- input$date_range
      if (is.null(dr) || length(dr) != 2L || any(is.na(dr))) {
        shiny::showNotification(i18n$t("monitoring_validate_dates"),
                                type = "warning", duration = 4)
        return()
      }

      # Initial persistent toast: replaced as soon as the worker emits
      # its first progress event (typically <1 s after invoke).
      shiny::showNotification(
        i18n$t("monitoring_ingest_starting"),
        id          = session$ns("ingest_progress"),
        type        = "message",
        duration    = NULL,
        closeButton = FALSE
      )

      # Resolve where the worker will write progress.json. NULL when no
      # project is open (PG-only setup) — in that case the progress
      # bandeau just stays on "Téléchargement en cours...".
      ppath <- .resolve_progress_path(app_state$current_project,
                                      "ingest_progress.json")
      if (!is.null(ppath) && file.exists(ppath)) {
        tryCatch(unlink(ppath), error = function(e) NULL)
      }
      ingest_progress_path(ppath)

      # Sentinel-2 band cache: nemeton@v0.21.3+ accepts `cache_dir` and
      # reuses already-downloaded bands across runs. Drop it under the
      # project so it follows the project (and so the user can wipe it
      # by clearing the project data folder). NULL = no cache (in-memory
      # only, the legacy behavior).
      cache_dir <- .resolve_s2_cache_dir(app_state$current_project)

      # `reprime_cache` checkbox semantics (inverted in v0.30.1):
      #   ☐ default → skip_cached = FALSE always (DB INSERTs are
      #              ON CONFLICT DO NOTHING, so re-running is safe).
      #              nemeton checks the disk cache, only fetches
      #              missing bands. This is what FORDEAD needs —
      #              the DB-short-circuit default of v0.30.0 and
      #              before left users with a populated DB but an
      #              empty disk cache, and FORDEAD had to re-fetch
      #              everything.
      #   ☑ checked → wipe <cache_dir>/* then re-download every
      #              scene + band from scratch. Use to recover from
      #              a corrupted cache.
      #
      # The skip_cached value passed to nemeton::ingest_sentinel2_timeseries
      # is now FALSE in BOTH branches (see the $invoke call below).
      # The branch below only differs by the wipe step.
      if (isTRUE(input$reprime_cache) && !is.null(cache_dir) &&
          dir.exists(cache_dir)) {
        # v0.51.4 — réamorçage RESTREINT à la fenêtre de dates FAST. Le
        # cache S2 est PARTAGÉ avec FORDEAD, dont la période
        # d'apprentissage déborde souvent la fenêtre FAST : on ne supprime
        # donc QUE les dossiers de scènes dont la date d'acquisition tombe
        # dans [dr[1], dr[2]], et on PRÉSERVE celles hors fenêtre (dates
        # d'apprentissage FORDEAD). Une scène dont la date n'est pas
        # parsable est conservée (prudence — on ne supprime que ce qu'on
        # peut dater avec certitude).
        d_from <- suppressWarnings(as.Date(dr[1]))
        d_to   <- suppressWarnings(as.Date(dr[2]))
        scene_dirs <- list.dirs(cache_dir, recursive = FALSE,
                                full.names = TRUE)
        if (length(scene_dirs) && !is.na(d_from) && !is.na(d_to)) {
          in_window <- vapply(scene_dirs, function(d) {
            sd <- .s2_scene_date_from_id(basename(d))
            !is.na(sd) && sd >= d_from && sd <= d_to
          }, logical(1))
          victims <- scene_dirs[in_window]
          if (length(victims)) {
            tryCatch(
              unlink(victims, recursive = TRUE, force = TRUE),
              error = function(e) NULL
            )
          }
          cli::cli_alert_info(
            "Cache S2 réamorcé sur la fenêtre FAST [{d_from} — {d_to}] : {length(victims)} scène(s) supprimée(s), {length(scene_dirs) - length(victims)} conservée(s) hors fenêtre (FORDEAD préservé)."
          )
        }
      }

      # Console log channel: truncate the file before invoke and reset
      # the tail offset so the parent picks the worker stream up from
      # byte 0. NULL when no project is open — in that case the worker
      # stays silent (its stdout is still captured by future).
      lpath <- .resolve_progress_path(app_state$current_project,
                                      "ingest_console.log")
      if (!is.null(lpath)) {
        tryCatch(writeLines(character(0), lpath, useBytes = TRUE),
                 error = function(e) NULL)
      }
      ingest_log_path(lpath)
      ingest_log_offset(0L)

      fast_task$invoke(
        zone_id       = as.integer(input$zone_id),
        start         = as.Date(dr[1]),
        end           = as.Date(dr[2]),
        # v0.61.0 — `bands` câblé en dur. Cf. commentaire dans la
        # sidebar (le checkboxGroupInput a été retiré).
        # NDMI ajouté (nemeton >= 0.64.0) : l'ingestion cache la bande
        # B11 nécessaire et le cœur pré-chauffe aussi les masques
        # d'alerte NDMI (combinaisons index × mode de prewarm_alerts).
        bands         = c("NDVI", "NBR", "NDMI"),
        max_cloud     = 20,
        # Pre-resolve here (the future worker can't see app_state) and
        # pass the URL explicitly. The fallback to a local SQLite file
        # under <project>/data/monitoring.sqlite is selected when no
        # PG env var is set — that's the single-user path.
        db_url        = .resolve_monitoring_db_url(app_state$current_project),
        progress_path = ppath,
        cache_dir     = cache_dir,
        # v0.30.1: always FALSE — nemeton checks disk cache + DB
        # INSERTs are idempotent. See the comment block above the
        # wipe branch for the full semantics.
        skip_cached   = FALSE,
        log_path      = lpath,
        # v0.42.1 — forwarded so the worker builds its ntfy push
        # messages in the user's language (the worker has no access
        # to app_state). Symétrique avec fordead_task$invoke.
        lang          = app_state$language %||% "fr",
        # v0.52.0 — chemin du cancel-flag polled par le worker entre
        # chaque tuile (nemeton@v0.53.0). Le clic « Annuler le
        # diagnostic » écrit ce fichier ; nemeton renvoie alors un
        # résumé status="cancelled" + commit partiel.
        cancel_path   = .fast_cancel_flag,
        # v0.54.0 → v0.55.0 — Le pré-calcul des 4 cartes FAST est
        # désormais fait PAR LE CŒUR (spec 018 nemeton@v0.61.0) via
        # les params natifs `prewarm_alerts` + `prewarm_mask_cache_dir`.
        # L'app les active systématiquement (feature désirable par
        # défaut). Le chemin doit être EXACTEMENT le même que celui
        # utilisé par `mod_monitoring_fast_alerts.R` /
        # `mod_validation_sampling.R` à la lecture (sinon cache D6
        # raté car hash calculé/lu à des endroits différents) — d'où
        # le helper `.fast_alert_cache_dir()` factorisé.
        prewarm_alerts         = TRUE,
        prewarm_mask_cache_dir = .fast_alert_cache_dir(
          app_state$current_project$path
        )
      )
    })

    # Result handler: re-runs whenever the task transitions to a
    # terminal state. While "running", task$result() throws
    # shiny.silent.error which we re-raise to keep the observer paused.
    shiny::observe({
      i18n <- i18n_r()
      result <- tryCatch(
        fast_task$result(),
        error = function(e) {
          if (inherits(e, "shiny.silent.error")) stop(e)
          # v0.70.4 — Garde idempotence (cf. fast_result_consumed)
          # appliquée AUSSI au branchement erreur : sans elle, un
          # re-fire de l'observer post-erreur ré-émet le toast
          # `ingest_error`.
          if (isTRUE(shiny::isolate(fast_result_consumed()))) {
            return(NULL)
          }
          fast_result_consumed(TRUE)
          shiny::removeNotification(session$ns("ingest_progress"))
          .cleanup_progress_file(ingest_progress_path())
          ingest_progress_path(NULL)
          .cleanup_progress_file(ingest_log_path())
          ingest_log_path(NULL)
          ingest_log_offset(0L)
          shiny::showNotification(
            sprintf("%s : %s", i18n$t("monitoring_ingest_error"),
                    e$message),
            id       = session$ns("ingest_error"),
            type     = "error", duration = 8
          )
          NULL
        }
      )
      if (!is.null(result)) {
        # v0.70.4 — Garde idempotence : `fast_task$result()` peut
        # refire plusieurs fois pour le MÊME result (cascade
        # reactive, transition status, etc.). Sans cette garde, le
        # toast `monitoring_ingest_success` (duration = 6 s) était
        # ré-émis plusieurs fois → clignotement visible côté UX.
        # Le flag est reset à FALSE dans observeEvent(input$run).
        if (isTRUE(shiny::isolate(fast_result_consumed()))) {
          return()
        }
        fast_result_consumed(TRUE)
        shiny::removeNotification(session$ns("ingest_progress"))
        .cleanup_progress_file(ingest_progress_path())
        ingest_progress_path(NULL)
        .cleanup_progress_file(ingest_log_path())
        ingest_log_path(NULL)
        ingest_log_offset(0L)
        n_scenes <- as.integer(result$summary$n_scenes %||% 0L)
        # v0.53.1 — `n_obs_inserted` toujours 0 depuis nemeton@v0.58.0
        # (drop obs_pixel insertion). Plus consommé dans le toast.
        warns    <- result$warnings %||% character(0)
        if (n_scenes == 0L) {
          # 0 scènes peut signifier soit "vraiment rien dans la période"
          # soit "STAC en panne" (HTTP 504, timeout réseau...). On
          # surface les warnings capturés (résumés pour rester
          # lisibles — cf. .summarize_backend_warnings) pour que
          # l'utilisateur sache si c'est une vraie absence ou un échec
          # backend. `id` explicite pour qu'un re-clic remplace le
          # toast au lieu de l'empiler.
          detail <- if (length(warns) > 0L) {
            paste(.summarize_backend_warnings(warns), collapse = " ; ")
          } else {
            i18n$t("monitoring_ingest_zero_default")
          }
          shiny::showNotification(
            sprintf(i18n$t("monitoring_ingest_zero_fmt"), detail),
            id       = session$ns("ingest_zero"),
            type     = "warning", duration = 12
          )
        } else {
          shiny::showNotification(
            sprintf(i18n$t("monitoring_ingest_success"), n_scenes),
            id       = session$ns("ingest_success"),
            type     = "message", duration = 6
          )
          # Si malgré le succès on a recolté des warnings non bloquants,
          # on les montre en plus dans un toast secondaire (id distinct
          # pour cohabiter avec le toast success). Les warnings sont
          # nettoyés (URLs pré-signées Azure / STAC strippées, cap à
          # ~200 chars par entrée) pour éviter le mur de texte du SAS
          # token quand un crop échoue en HTTP 403/404.
          if (length(warns) > 0L) {
            shiny::showNotification(
              sprintf(i18n$t("monitoring_ingest_warns_fmt"),
                      paste(.summarize_backend_warnings(warns),
                            collapse = " ; ")),
              id       = session$ns("ingest_warns"),
              type     = "warning", duration = 10
            )
          }
        }
        zones_refresh(zones_refresh() + 1L)  # force re-fetch in case
                                             # the ingestion changed state
        # v0.52.16 — bump `obs_refresh` retiré : `obs_pixel_data()`
        # n'existe plus, plus rien à invalider. `fast_reload` ci-dessous
        # suffit pour signaler aux modules raster (Alertes FAST + Carte
        # FAST) qu'ils doivent re-scanner le cache COG.
        # v0.42.0 — propagate to Alertes FAST + Carte FAST so they pick
        # up the new DB rows / new cache files. Before this bump, the
        # toast "ingestion success" would show but those two tabs
        # remained frozen on their pre-ingest empty state until the
        # user touched a slider — cf. bug report 2026-05-23 sur villards.
        fast_reload(fast_reload() + 1L)
      }
    })

    # ----- Async FORDEAD diagnosis (E6.c.5 — health mode) -----------

    fordead_task <- run_fordead_async()

    # v0.36.5 — snapshot du dernier résultat FORDEAD résolu. Permet
    # à l'onglet « Alertes FORDEAD » d'afficher une card « Zone saine »
    # avec la durée du run quand le diagnostic se termine avec 0
    # alertes insérées (réponse à un cas réel reporté : run réussi en
    # 142 s, n_alerts_inserted = 0, mais l'UI restait muette). NULL
    # tant qu'aucun diagnostic n'a abouti dans la session.
    fordead_last_result <- shiny::reactiveVal(NULL)

    # Same per-task progress plumbing as ingestion. FORDEAD emits phase
    # events (training / forest mask / dieback / export) — the toast
    # surfaces whichever phase the worker is currently on.
    fordead_progress_path <- shiny::reactiveVal(NULL)

    fordead_progress <- shiny::reactivePoll(
      intervalMillis = 500L, session,
      checkFunc = function() {
        p <- fordead_progress_path()
        if (is.null(p) || !file.exists(p)) return("0")
        as.character(file.info(p)$mtime)
      },
      valueFunc = function() {
        p <- fordead_progress_path()
        if (is.null(p) || !file.exists(p)) return(NULL)
        tryCatch(
          jsonlite::read_json(p, simplifyVector = TRUE),
          error = function(e) NULL
        )
      }
    )

    # FORDEAD progress dispatcher (v0.32.0). The cœur emits structured
    # events via progress_callback; we route each `ev$current` value
    # to its own toast strategy. Toasts position themselves in
    # bottom-right via the .shiny-notification-panel CSS override
    # in inst/app/www/css/custom.css (no per-call positioning API in
    # Shiny).
    #
    # Event → behavior:
    #   fordead:start       silent (button state is enough signal)
    #   fordead:phase       persistent spinner toast
    #                       "Phase {n}/{total} — {label}"
    #   fordead:phase_done  brief check toast "✓ {label}" (1.5 s)
    #   fordead:complete    8 s success toast with alert count + duration
    #   fordead:error       persistent error toast (user closes)
    #
    # Phase labels are looked up via i18n with humanized fallback —
    # see `.fordead_phase_label` — so a new phase name added in a
    # future nemeton release auto-shows as a Title-Cased version of
    # its raw key (no app release strictly required to consume it,
    # though we should add proper i18n strings in the next patch).
    #
    # The legacy status-based branch is kept as a safety net for
    # any pre-v0.22.5 nemeton that might still be in someone's env.
    shiny::observe({
      ev <- fordead_progress()
      if (is.null(ev)) return()
      .fordead_handle_progress_event(ev, session, i18n_r())
    })

    # User-side override that force-unlocks the run_health button —
    # same purpose as force_unlock_quick (cf. above). FORDEAD runs can
    # take 10-30 minutes and have no built-in cancellation, so a stuck
    # phase (network outage on STAC, reticulate Python OOM, etc.)
    # would otherwise block the user from relaunching.
    force_unlock_health <- shiny::reactiveVal(FALSE)

    # Button state for run_health: same logic as `run` — grey out
    # during an active FORDEAD run (long task) AND no user override,
    # preconditions are validated in the click observer for explicit
    # toast feedback.
    #
    # Cross-lock (v0.39.2): also greyed while a FAST ingestion is in
    # flight (shared Sentinel-2 cache — cf. the run-button observer).
    # Respects FAST's force-unlock symmetrically.
    shiny::observe({
      fordead_running <- identical(fordead_task$status(), "running") &&
                         !isTRUE(force_unlock_health())
      fast_running    <- identical(fast_task$status(), "running") &&
                         !isTRUE(force_unlock_quick())
      shiny::updateActionButton(session, "run_health",
                                disabled = fordead_running || fast_running)
    })

    output$run_health_cancel_panel <- shiny::renderUI({
      ns <- session$ns
      i18n <- i18n_r()
      if (!identical(fordead_task$status(), "running")) return(NULL)
      if (isTRUE(force_unlock_health())) return(NULL)
      shiny::actionButton(
        ns("run_health_cancel"),
        i18n$t("monitoring_run_cancel_btn"),
        icon  = bsicons::bs_icon("x-circle"),
        class = "btn-outline-secondary btn-sm w-100 mt-1"
      )
    })

    shiny::observeEvent(input$run_health_cancel, {
      i18n <- i18n_r()
      # v0.52.0 — Vrai cancel coopératif. Symétrique au FAST :
      # écrit le flag AVANT le force-unlock UI. Le worker FORDEAD
      # poll ce fichier entre phases reticulate (nemeton@v0.53.0+) et
      # sort proprement au prochain checkpoint.
      cancel_path <- .resolve_progress_path(
        app_state$current_project, "fordead_cancel.flag")
      if (!is.null(cancel_path)) {
        tryCatch(file.create(cancel_path),
                 error = function(e) NULL)
      }
      force_unlock_health(TRUE)
      for (nid in c("fordead_progress", "fordead_error",
                    "fordead_success", "fordead_warns")) {
        shiny::removeNotification(session$ns(nid))
      }
      .cleanup_progress_file(fordead_progress_path())
      fordead_progress_path(NULL)
      shiny::showNotification(
        i18n$t("monitoring_run_cancel_done"),
        id       = session$ns("run_health_cancel_info"),
        type     = "default",
        duration = 6
      )
    })

    # Helper — kicks off the FORDEAD task with the current sidebar
    # values. Called both from the direct-click path (validity OK) and
    # from the modal "force anyway" path (G3 garde-fou).
    #
    # v0.33.0 migration: nemeton@v0.24.0 derives the AOI from
    # (con, zone_id) internally — no need to fabricate `aoi` here
    # or open a throwaway DB connection. The worker now takes
    # cache_dir so its new phase-0 `ingest` can fetch missing bands
    # (B02 / B05 / B8A / B11) on top of what FAST already cached
    # (B04 / B12). zone_id presence is validated up front; the
    # actual existence check happens core-side.
    .invoke_fordead <- function() {
      i18n <- i18n_r()
      if (!isTRUE(nzchar(input$zone_id))) {
        shiny::showNotification(i18n$t("monitoring_validate_zone"),
                                type = "warning", duration = 4)
        return(invisible(FALSE))
      }
      shiny::showNotification(
        i18n$t("monitoring_health_starting"),
        id          = session$ns("fordead_progress"),
        type        = "message",
        duration    = NULL,
        closeButton = FALSE
      )
      ppath <- .resolve_progress_path(app_state$current_project,
                                      "fordead_progress.json")
      if (!is.null(ppath) && file.exists(ppath)) {
        tryCatch(unlink(ppath), error = function(e) NULL)
      }
      fordead_progress_path(ppath)

      # v0.52.0 — Purge un éventuel cancel_path résiduel avant un
      # nouveau lancement (sinon le worker abandonne dès l'entrée).
      .fordead_cancel_flag <- .resolve_progress_path(
        app_state$current_project, "fordead_cancel.flag")
      if (!is.null(.fordead_cancel_flag) &&
          file.exists(.fordead_cancel_flag)) {
        tryCatch(unlink(.fordead_cancel_flag, force = TRUE),
                 error = function(e) NULL)
      }

      fordead_task$invoke(
        dates_training    = as.character(input$dates_training),
        dates_monitoring  = as.character(input$date_range),
        threshold_anomaly = as.numeric(input$threshold_anomaly),
        vegetation_index  = input$vegetation_index,
        zone_id           = as.integer(input$zone_id),
        cache_dir         = .resolve_s2_cache_dir(app_state$current_project),
        db_url            = .resolve_monitoring_db_url(app_state$current_project),
        progress_path     = ppath,
        # v0.71.1 — Force l'output FORDEAD dans le cache projet
        # (sortait par défaut dans /tmp via tempfile("fordead_")).
        # `keep_output = TRUE` pour préserver les outputs (training,
        # masks bruts) — inspectables. Le dossier est per-zone et
        # écrasé à chaque relance, taille bornée.
        output_dir        = .resolve_fordead_output_dir(
                              app_state$current_project, input$zone_id),
        keep_output       = TRUE,
        # Forwarded so the worker builds its ntfy push messages in the
        # user's language (the worker has no access to app_state).
        lang              = app_state$language %||% "fr",
        # v0.52.0 — cancel coopératif (polled entre phases reticulate
        # par nemeton@v0.53.0+).
        cancel_path       = .fordead_cancel_flag
      )
      .persist_monitoring_metadata()
      invisible(TRUE)
    }

    # Persist sidebar state in the project metadata.json so a reopen
    # restores the user's last FORDEAD configuration. Best-effort —
    # silent no-op if the project has no on-disk path or metadata.
    .persist_monitoring_metadata <- function() {
      project_id <- app_state$current_project$id
      if (is.null(project_id)) return(invisible(FALSE))
      v <- validity()
      tryCatch(
        update_project_metadata(project_id, list(
          monitoring_mode               = input$mode,
          monitoring_threshold_anomaly  = as.numeric(input$threshold_anomaly),
          monitoring_vegetation_index   = input$vegetation_index,
          monitoring_dates_training     = as.character(input$dates_training),
          monitoring_validity_geo_pct   = v$geo_intersection_pct %||% NA_real_,
          monitoring_validity_species_pct = v$species_resineux_pct %||% NA_real_
        )),
        error = function(e) {
          cli::cli_warn("Could not persist monitoring metadata: {conditionMessage(e)}")
          invisible(FALSE)
        }
      )
    }

    # Click handler with G3 garde-fou: when the zone is out of validity
    # domain (geographic OR species), pop a confirmation modal before
    # invoking. The user can still proceed but knows the calibration
    # warranty doesn't apply.
    shiny::observeEvent(input$run_health, {
      i18n <- i18n_r()
      # New manual launch: discard any prior force-unlock so the
      # button correctly greys out for the new worker.
      force_unlock_health(FALSE)
      # v0.71.1 — Reset le flag idempotence (parité FAST v0.70.4).
      # Le prochain `fordead_task$result()` doit être traité (toast
      # success / error émis).
      fordead_result_consumed(FALSE)
      # Cross-lock: refuse to start FORDEAD while a FAST ingestion is
      # in flight (shared Sentinel-2 cache — cf. the run_health button
      # observer). An abandoned FAST run does not block.
      if (identical(fast_task$status(), "running") &&
          !isTRUE(force_unlock_quick())) {
        shiny::showNotification(i18n$t("monitoring_busy_fast"),
                                type = "warning", duration = 6)
        return()
      }
      if (identical(fordead_task$status(), "running")) {
        shiny::showNotification(i18n$t("monitoring_health_starting"),
                                type = "message", duration = 4)
        return()
      }
      if (!isTRUE(nzchar(input$zone_id))) {
        shiny::showNotification(i18n$t("monitoring_validate_zone"),
                                type = "warning", duration = 4)
        return()
      }
      dr <- input$date_range
      if (is.null(dr) || length(dr) != 2L || any(is.na(dr))) {
        shiny::showNotification(i18n$t("monitoring_validate_dates"),
                                type = "warning", duration = 4)
        return()
      }
      dt <- input$dates_training
      if (is.null(dt) || length(dt) != 2L || any(is.na(dt))) {
        shiny::showNotification(i18n$t("monitoring_validate_dates"),
                                type = "warning", duration = 4)
        return()
      }

      v <- validity()
      if (!is.null(v) && isFALSE(v$overall_valid)) {
        shiny::showModal(shiny::modalDialog(
          title = i18n$t("monitoring_confirm_invalid_title"),
          i18n$t("monitoring_confirm_invalid_body"),
          easyClose = FALSE,
          footer = htmltools::tagList(
            shiny::modalButton(i18n$t("monitoring_confirm_no")),
            shiny::actionButton(session$ns("confirm_invalid_run"),
                                i18n$t("monitoring_confirm_yes"),
                                class = "btn-warning")
          )
        ))
        return()
      }
      .invoke_fordead()
    })

    # G3 modal "Run anyway" path.
    shiny::observeEvent(input$confirm_invalid_run, {
      shiny::removeModal()
      .invoke_fordead()
    })

    # FORDEAD result handler.
    #
    # v0.36.5 — trois fixes pour le bug UX reporté (run réussi en 142 s
    # avec 0 alertes mais bouton « Lancer » resté grisé + sous-onglets
    # muets) :
    #
    #   1. Belt-and-suspenders re-enable du bouton via
    #      updateActionButton(disabled = FALSE) en plus de l'observer
    #      basé sur fordead_task$status() (au cas où une transition de
    #      statut serait manquée par Shiny pour une raison quelconque).
    #   2. Reset explicite de force_unlock_health(FALSE) pour rester
    #      cohérent avec l'observer click qui le pose à TRUE puis FALSE.
    #   3. Snapshot du résultat dans fordead_last_result() pour que
    #      l'onglet « Alertes FORDEAD » puisse afficher la card
    #      « Zone saine » avec la durée du run.
    shiny::observe({
      i18n <- i18n_r()
      result <- tryCatch(
        fordead_task$result(),
        error = function(e) {
          if (inherits(e, "shiny.silent.error")) stop(e)
          # v0.71.1 — Garde d'idempotence (symétrique FAST v0.70.4).
          # Sans elle, un re-fire de l'observer post-erreur ré-émet
          # le toast `fordead_error`.
          if (isTRUE(shiny::isolate(fordead_result_consumed()))) {
            return(NULL)
          }
          fordead_result_consumed(TRUE)
          shiny::removeNotification(session$ns("fordead_progress"))
          .cleanup_progress_file(fordead_progress_path())
          fordead_progress_path(NULL)
          shiny::showNotification(
            sprintf("%s : %s", i18n$t("monitoring_health_error"),
                    e$message),
            id       = session$ns("fordead_error"),
            type     = "error", duration = 10
          )
          # Belt-and-suspenders : re-enable + reset force-unlock pour
          # ne pas laisser le bouton grisé en cas de timing race.
          shiny::updateActionButton(session, "run_health",
                                    disabled = FALSE)
          force_unlock_health(FALSE)
          # Mémoriser le statut "error" — l'UI Alertes FORDEAD distingue
          # « pas encore lancé » de « run terminé en erreur ».
          fordead_last_result(list(
            status  = "error",
            message = conditionMessage(e),
            zone_id = suppressWarnings(as.integer(input$zone_id))
          ))
          NULL
        }
      )
      if (!is.null(result)) {
        # v0.71.1 — Garde d'idempotence : `fordead_task$result()` peut
        # refire plusieurs fois pour le MÊME result (cascade reactive,
        # transition status, etc.). Sans cette garde, le toast
        # `fordead_success` (duration = 8 s) était ré-émis →
        # clignotement + bouton « Lancer » perçu grisé.
        # Reset à FALSE dans observeEvent(input$run_health).
        if (isTRUE(shiny::isolate(fordead_result_consumed()))) {
          return()
        }
        fordead_result_consumed(TRUE)
        shiny::removeNotification(session$ns("fordead_progress"))
        .cleanup_progress_file(fordead_progress_path())
        fordead_progress_path(NULL)
        # Stamp the zone so the disk-reconciliation observer can tell
        # this in-session result apart from a synthetic reconciled one
        # and never clobbers it (cf. .reconcile_fordead_state).
        result$zone_id <- suppressWarnings(as.integer(input$zone_id))
        fordead_last_result(result)
        if (identical(result$status, "error")) {
          shiny::showNotification(
            sprintf("%s : %s", i18n$t("monitoring_health_error"),
                    result$message %||% ""),
            id       = session$ns("fordead_error"),
            type     = "error", duration = 10
          )
        } else {
          shiny::showNotification(
            sprintf(i18n$t("monitoring_health_success"),
                    result$n_alerts_inserted %||% 0L,
                    result$duration_sec      %||% 0),
            id       = session$ns("fordead_success"),
            type     = "message", duration = 8
          )
          alerts_refresh(alerts_refresh() + 1L)
        }
        # Belt-and-suspenders : re-enable explicite du bouton après
        # toute résolution (success ou error) en plus du status-based
        # observe. Même reset de force_unlock_health pour cohérence.
        shiny::updateActionButton(session, "run_health",
                                  disabled = FALSE)
        force_unlock_health(FALSE)
      }
    })

    # ----- Piste 1 — re-read disk + DB on FORDEAD sub-tab open -------
    #
    # `alerts_refresh` is normally bumped only by the run-completion
    # observer above. That observer never fires when a FORDEAD run
    # outlives its Shiny session (long run + browser disconnect): the
    # `future` worker still finishes, persists the dieback mask and
    # inserts alerts, but the orphaned `$result()` is never delivered.
    # Bumping `alerts_refresh` whenever the user opens "Alertes
    # FORDEAD" or "Carte FORDEAD" makes both `alerts()` and the Carte
    # FORDEAD `mask_r` re-query the DB / re-read the mask, so a
    # completed-but-undelivered run surfaces on the next tab visit.
    shiny::observeEvent(input$subtab, {
      if (isTRUE(input$subtab %in% c("alerts_fordead",
                                     "pixel_map_fordead"))) {
        alerts_refresh(alerts_refresh() + 1L)
      }
      # spec 021 — same for the RECONFORT map sub-tab.
      if (isTRUE(input$subtab == "pixel_map_reconfort")) {
        reconfort_refresh(reconfort_refresh() + 1L)
      }
    }, ignoreInit = FALSE)

    # ----- Piste 2 — reconcile FORDEAD result state from disk --------
    #
    # `fordead_last_result()` is an in-session reactiveVal: a reload
    # loses it. A run that completed out-of-session therefore showed
    # the generic "not run yet" placeholder on the Alertes FORDEAD
    # tab instead of the "Zone saine" card. This observer rebuilds a
    # synthetic success result from the persisted dieback mask
    # whenever the project / zone changes — unless a genuine
    # in-session result for that exact zone already exists (richer:
    # it carries the real run duration). The mask timestamp drives
    # the card's meta line (`monitoring_fordead_no_alerts_meta_date`).
    shiny::observe({
      proj <- app_state$current_project
      zone <- input$zone_id
      if (!identical(input$mode, "health")) return()
      if (!isTRUE(nzchar(zone))) return()
      if (identical(fordead_task$status(), "running")) return()
      zone_int <- suppressWarnings(as.integer(zone))
      cur <- shiny::isolate(fordead_last_result())
      if (!is.null(cur) && !isTRUE(cur$reconciled) &&
          identical(cur$zone_id, zone_int)) {
        return()
      }
      fordead_last_result(.reconcile_fordead_state(proj, zone_int))
    })

    # Spec 010 — Carte pixel sub-tab.
    #
    # v0.52.16 — paramètre `obs_pixel_data` retiré. Le module dérive
    # désormais `scenes_df` directement du cache COG disque (parse de
    # la date depuis le nom de scène Sentinel-2). FAST = pure raster
    # per-pixel, plus de SQL roundtrip sur `obs_pixel`.
    #
    # v0.42.0 — `refresh_r = fast_reload` allows cache_dir_r() to
    # re-evaluate after an ingestion creates the cache directory
    # (the dir.exists() check is not a Shiny dep on its own).
    # `thresholds_r` feeds Livrable 3 — horizontal reference lines on
    # the per-pixel modal plot drawn at the current sidebar NDVI / NBR
    # threshold values.
    pixel_map_ret <- mod_monitoring_pixel_map_server(
      "pixel_map",
      app_state      = app_state,
      mode_input     = shiny::reactive(input$mode),
      refresh_r      = shiny::reactive(fast_reload()),
      # v0.52.14 — pixel_map a son propre `input$index` (radio dans son
      # sidebar droit) ; le `thresholds_r` parent ne porte plus
      # `index` (relicat de v0.52.13).
      thresholds_r   = shiny::reactive(list(
        ndvi = input$threshold_ndvi,
        nbr  = input$threshold_nbr,
        ndmi = input$threshold_ndmi
      ))
    )

    # v0.42.0 — Alertes FAST sub-tab. Spec 013 raster wiring : the
    # module now consumes `nemeton::read_fast_alert_raster()` (count
    # / rolling modes, pixel resolution S2 10 m). Sidebar widgets
    # (zone_id, date_range, threshold_ndvi, threshold_nbr,
    # window_days) are forwarded as before, plus the new `mode_r`
    # toggle owned by the sub-module itself.
    # `refresh_r = fast_reload` so the raster reactive picks up the
    # post-ingestion DB state without a manual slider wiggle.
    fast_alerts_ret <- mod_monitoring_fast_alerts_server(
      "fast_alerts",
      app_state    = app_state,
      zone_id_r    = shiny::reactive(input$zone_id),
      date_range_r = shiny::reactive(input$date_range),
      # v0.52.14 — fast_alerts a son propre `input$index` (radio dans
      # son sidebar droit) ; le `thresholds_r` parent ne porte plus
      # `index`.
      thresholds_r = shiny::reactive(list(
        ndvi        = input$threshold_ndvi,
        nbr         = input$threshold_nbr,
        ndmi        = input$threshold_ndmi,
        window_days = input$window_days
      )),
      refresh_r    = shiny::reactive(fast_reload())
    )

    # v0.36.0 — Carte FORDEAD sub-tab. nemeton@v0.41.0 ships the mask
    # persist hook, so a completed FORDEAD run drops the categorical
    # 0-4 raster into <project>/cache/layers/fordead/ and the module
    # renders it.
    #
    # v0.38.6 — `refresh_r = alerts_refresh` is passed so the module's
    # mask_r reactive re-reads the cache after each FORDEAD run.
    # alerts_refresh is bumped by the FORDEAD result observer on
    # success; without threading it in, the Carte FORDEAD panel
    # stayed frozen on its pre-run empty-state even though the mask
    # had just been written to disk.
    fordead_map_ret <- mod_monitoring_fordead_map_server(
      "fordead_map",
      app_state = app_state,
      zone_id_r = shiny::reactive(input$zone_id),
      refresh_r = shiny::reactive(alerts_refresh())
    )

    # spec 021 (L6) — Carte RECONFORT : alertes feuillus + diagnostic
    # pixel CRSWIR/CRre. Lazy comme FORDEAD ; `reconfort_refresh` est
    # bumpé à la fin d'un run RECONFORT (et à l'ouverture du sous-onglet)
    # pour ré-invalider la couche d'alertes.
    reconfort_map_ret <- mod_monitoring_reconfort_map_server(
      "reconfort_map",
      app_state = app_state,
      zone_id_r = shiny::reactive(input$zone_id),
      refresh_r = shiny::reactive(reconfort_refresh())
    )

    # spec 021 (L6) — Lancement d'un run RECONFORT (ExtendedTask).
    #
    # Miroir de la machinerie FORDEAD (task + reactivePoll progress +
    # dispatcher + result observer + force-unlock). Le run cœur
    # `nemeton::run_reconfort_dieback()` est LOURD/opt-in (conda IOTA² +
    # GEODES + OTB/Shark) : sur un déploiement sans ce bundle, le worker
    # échoue et l'erreur est surfacée via le toast d'erreur — la carte et
    # le diagnostic restent fonctionnels sur les runs déjà produits
    # (Limite #1 spec 021). Pas de cancel coopératif côté cœur (pas de
    # `cancel_path`) : seul le force-unlock réarme le bouton.
    reconfort_task <- run_reconfort_async()
    reconfort_progress_path <- shiny::reactiveVal(NULL)
    reconfort_result_consumed <- shiny::reactiveVal(FALSE)
    force_unlock_reconfort <- shiny::reactiveVal(FALSE)

    reconfort_progress <- shiny::reactivePoll(
      intervalMillis = 500L, session,
      checkFunc = function() {
        p <- reconfort_progress_path()
        if (is.null(p) || !file.exists(p)) return("0")
        as.character(file.info(p)$mtime)
      },
      valueFunc = function() {
        p <- reconfort_progress_path()
        if (is.null(p) || !file.exists(p)) return(NULL)
        tryCatch(jsonlite::read_json(p, simplifyVector = TRUE),
                 error = function(e) NULL)
      }
    )

    shiny::observe({
      ev <- reconfort_progress()
      if (is.null(ev)) return()
      .reconfort_handle_progress_event(ev, session, i18n_r())
    })

    # Resolve / create the RECONFORT cache dir for the active project.
    .reconfort_cache_dir <- function(proj) {
      if (is.null(proj) || is.null(proj$path)) return(NULL)
      cd <- file.path(proj$path, "cache", "layers", "reconfort")
      if (!dir.exists(cd))
        tryCatch(dir.create(cd, recursive = TRUE, showWarnings = FALSE),
                 error = function(e) NULL)
      cd
    }

    # Button state : grey out while a RECONFORT run is in flight (and no
    # force-unlock) OR while a FAST / FORDEAD run holds the shared
    # Sentinel-2 cache.
    shiny::observe({
      reconfort_running <- identical(reconfort_task$status(), "running") &&
                           !isTRUE(force_unlock_reconfort())
      fast_running     <- identical(fast_task$status(), "running") &&
                          !isTRUE(force_unlock_quick())
      fordead_running  <- identical(fordead_task$status(), "running") &&
                          !isTRUE(force_unlock_health())
      shiny::updateActionButton(
        session, "run_reconfort",
        disabled = reconfort_running || fast_running || fordead_running)
    })

    output$run_reconfort_cancel_panel <- shiny::renderUI({
      ns <- session$ns
      i18n <- i18n_r()
      if (!identical(reconfort_task$status(), "running")) return(NULL)
      if (isTRUE(force_unlock_reconfort())) return(NULL)
      shiny::actionButton(
        ns("run_reconfort_cancel"),
        i18n$t("monitoring_run_cancel_btn"),
        icon  = bsicons::bs_icon("x-circle"),
        class = "btn-outline-secondary btn-sm w-100 mt-1"
      )
    })

    # Force-unlock : no cooperative cancel in the core, so this only
    # re-arms the button (the orphaned worker keeps running to completion
    # in the background).
    shiny::observeEvent(input$run_reconfort_cancel, {
      force_unlock_reconfort(TRUE)
      for (nid in c("reconfort_progress", "reconfort_error",
                    "reconfort_complete")) {
        shiny::removeNotification(session$ns(nid))
      }
    })

    .invoke_reconfort <- function() {
      i18n <- i18n_r()
      if (!isTRUE(nzchar(input$zone_id))) {
        shiny::showNotification(i18n$t("monitoring_validate_zone"),
                                type = "warning", duration = 4)
        return(invisible(FALSE))
      }
      proj <- app_state$current_project
      cd <- .reconfort_cache_dir(proj)
      if (is.null(cd)) {
        shiny::showNotification(i18n$t("monitoring_validate_zone"),
                                type = "warning", duration = 4)
        return(invisible(FALSE))
      }
      reconfort_result_consumed(FALSE)
      force_unlock_reconfort(FALSE)
      shiny::showNotification(
        i18n$t("monitoring_reconfort_starting"),
        id          = session$ns("reconfort_progress"),
        type        = "message",
        duration    = NULL,
        closeButton = FALSE
      )
      ppath <- .resolve_progress_path(proj, "reconfort_progress.json")
      if (!is.null(ppath) && file.exists(ppath)) {
        tryCatch(unlink(ppath), error = function(e) NULL)
      }
      reconfort_progress_path(ppath)

      zid <- as.integer(input$zone_id)
      out <- if (!is.null(cd)) file.path(cd, paste0("output_zone_", zid)) else NULL
      reconfort_task$invoke(
        zone_id       = zid,
        cache_dir     = cd,
        s2_year       = as.integer(input$reconfort_s2_year %||%
                                     format(Sys.Date(), "%Y")),
        db_url        = .resolve_monitoring_db_url(proj),
        progress_path = ppath,
        lang          = app_state$language %||% "fr",
        output_dir    = out
      )
      invisible(TRUE)
    }

    shiny::observeEvent(input$run_reconfort, {
      .invoke_reconfort()
    })

    # RECONFORT result handler (mirror of the FORDEAD one) : idempotent
    # toast + reconfort_refresh bump on success, error toast otherwise,
    # belt-and-suspenders button re-enable.
    shiny::observe({
      i18n <- i18n_r()
      result <- tryCatch(
        reconfort_task$result(),
        error = function(e) {
          if (inherits(e, "shiny.silent.error")) stop(e)
          if (isTRUE(shiny::isolate(reconfort_result_consumed()))) return(NULL)
          reconfort_result_consumed(TRUE)
          shiny::removeNotification(session$ns("reconfort_progress"))
          .cleanup_progress_file(reconfort_progress_path())
          reconfort_progress_path(NULL)
          shiny::showNotification(
            sprintf("%s : %s", i18n$t("monitoring_reconfort_error"),
                    conditionMessage(e)),
            id = session$ns("reconfort_error"), type = "error", duration = 10
          )
          shiny::updateActionButton(session, "run_reconfort", disabled = FALSE)
          force_unlock_reconfort(FALSE)
          NULL
        }
      )
      if (!is.null(result)) {
        if (isTRUE(shiny::isolate(reconfort_result_consumed()))) return()
        reconfort_result_consumed(TRUE)
        shiny::removeNotification(session$ns("reconfort_progress"))
        .cleanup_progress_file(reconfort_progress_path())
        reconfort_progress_path(NULL)
        shiny::showNotification(
          sprintf(i18n$t("monitoring_reconfort_success"),
                  result$n_alerts %||% result$n_alerts_inserted %||% 0L,
                  result$duration_sec %||% 0),
          id = session$ns("reconfort_success"), type = "message", duration = 8
        )
        reconfort_refresh(reconfort_refresh() + 1L)
        shiny::updateActionButton(session, "run_reconfort", disabled = FALSE)
        force_unlock_reconfort(FALSE)
      }
    })

    # v0.43.3 — Plan de validation : 2 instances mode-driven (FAST +
    # FORDEAD), source figée par instance. Symétrique avec les couples
    # Alertes/Carte FAST/FORDEAD. Threading des reactives sidebar (zone,
    # mode, thresholds, dates) identique pour les deux ; chaque instance
    # ne consomme que ce dont elle a besoin (source FAST → thresholds +
    # dates pour compute_fast_alert_mask ; source FORDEAD → ignore les
    # thresholds car le mask FORDEAD est déjà sur disque).
    validation_sampling_fast_ret <- mod_validation_sampling_server(
      "validation_sampling_fast",
      app_state    = app_state,
      zone_id_r    = shiny::reactive(input$zone_id),
      mode_r       = shiny::reactive(input$mode),
      # v0.52.14 — `index` suit le choix utilisateur côté Alertes FAST
      # (via le reactive exporté `fast_alerts_ret$index_r`), pour que la
      # prévisualisation validation_sampling FAST utilise le même
      # indice que ce que l'utilisateur voit dans l'onglet Alertes FAST.
      thresholds_r = shiny::reactive(list(
        index       = fast_alerts_ret$index_r() %||% "NDVI",
        ndvi        = input$threshold_ndvi,
        nbr         = input$threshold_nbr,
        ndmi        = input$threshold_ndmi,
        window_days = input$window_days
      )),
      date_range_r = shiny::reactive(input$date_range),
      source_fixed = "FAST"
    )

    validation_sampling_fordead_ret <- mod_validation_sampling_server(
      "validation_sampling_fordead",
      app_state    = app_state,
      zone_id_r    = shiny::reactive(input$zone_id),
      mode_r       = shiny::reactive(input$mode),
      # v0.52.14 — FORDEAD validation_sampling n'utilise pas
      # `read_fast_alert_raster` (sa source de mask est FORDEAD sur
      # disque, pas FAST). On laisse `index = "NDVI"` en valeur par
      # défaut au cas où le code aval y accède.
      thresholds_r = shiny::reactive(list(
        index       = "NDVI",
        ndvi        = input$threshold_ndvi,
        nbr         = input$threshold_nbr,
        ndmi        = input$threshold_ndmi,
        window_days = input$window_days
      )),
      date_range_r = shiny::reactive(input$date_range),
      source_fixed = "FORDEAD"
    )

    list(
      zones                       = zones,
      fast_task                   = fast_task,
      fordead_task                = fordead_task,
      validity                    = validity,
      pixel_map                   = pixel_map_ret,
      fast_alerts                 = fast_alerts_ret,
      fordead_map                 = fordead_map_ret,
      validation_sampling_fast    = validation_sampling_fast_ret,
      validation_sampling_fordead = validation_sampling_fordead_ret
    )
  })
}


# ---- Internal --------------------------------------------------------

# Resolve the absolute path where the async worker will write its
# `progress.json`. Returns NULL when no project is loaded — in that
# case the persistent "Ingestion en cours..." toast just stays put
# without per-scene refresh (the worker still completes successfully,
# we just have no progress channel).
.resolve_progress_path <- function(project, filename) {
  if (is.null(project) || is.null(project$path)) return(NULL)
  data_dir <- file.path(project$path, "data")
  if (!dir.exists(data_dir)) {
    tryCatch(
      dir.create(data_dir, recursive = TRUE, showWarnings = FALSE),
      error = function(e) NULL
    )
  }
  normalizePath(file.path(data_dir, filename),
                winslash = "/", mustWork = FALSE)
}

# v0.55.0 — Helper unique pour le chemin du cache D6 FAST alert
# (spec 017 + spec 018 nemeton). Utilisé à 3 endroits :
# (1) `mod_monitoring.R::fast_task$invoke()` pour `prewarm_mask_cache_dir`
# (2) `mod_monitoring_fast_alerts.R::raster_r()` pour `result_cache_dir`
# (3) `mod_validation_sampling.R` pour la prévisualisation
# Il est CRITIQUE que les 3 utilisent le même chemin, sinon le hash D6
# est calculé/lu à des endroits différents et le cache rate.
.fast_alert_cache_dir <- function(project_path) {
  if (is.null(project_path) || !nzchar(project_path)) return(NULL)
  file.path(project_path, "cache", "layers", "fast_alert")
}

# v0.57.0 — Helper parallèle pour le cache des MASKS d'alerte
# discrétisés en quartiles 0-4 (spec 017 D1-D2 nemeton@v0.55.0+).
# `nemeton::compute_fast_alert_mask(..., mask_cache_dir = ...)` persiste
# un TIF catégoriel 0-4 sous ce chemin. Distinct du cache du raster
# continu (`.fast_alert_cache_dir()`) car ce sont 2 produits différents :
#   * continu = score d'intensité du déficit NDVI/NBR (read_fast_alert_raster)
#   * mask    = classe de sévérité quartilée 0-4 (compute_fast_alert_mask)
.fast_alert_mask_cache_dir <- function(project_path) {
  if (is.null(project_path) || !nzchar(project_path)) return(NULL)
  file.path(project_path, "cache", "layers", "fast_alert_mask")
}

# Best-effort acquisition date (Date) parsed from a Sentinel-2 scene id
# (the cache subdir name). S2 product ids embed the datetime as
# YYYYMMDDTHHMMSS, so the first 8-digit run is the acquisition date.
# Returns NA when no plausible date token is found. Used by the
# date-windowed COG reprime to spare FORDEAD's out-of-window scenes.
.s2_scene_date_from_id <- function(scene_id) {
  m <- regmatches(scene_id, regexpr("[0-9]{8}", scene_id))
  if (!length(m) || !nzchar(m)) return(as.Date(NA))
  as.Date(m, format = "%Y%m%d")
}

# Resolve the Sentinel-2 band cache directory for the current project.
# Returns NULL when no project is loaded (no path = no cache, nemeton
# falls back to its in-memory legacy path).
#
# Layout (NMT cache convention, aligned with lidar_mnh / lidar_mnt /
# opencanopy/ already used by mod_sampling):
#
#   <project>/cache/layers/sentinel2/
#     S2A_MSIL2A_20240515.../
#       B04.tif
#       B08.tif
#       B12.tif
#
# Worker also re-creates the directory if missing, but we do it here
# too so the path lookup is consistent on subsequent calls.
.resolve_s2_cache_dir <- function(project) {
  if (is.null(project) || is.null(project$path)) return(NULL)
  cache_dir <- file.path(project$path, "cache", "layers", "sentinel2")
  if (!dir.exists(cache_dir)) {
    tryCatch(
      dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE),
      error = function(e) NULL
    )
  }
  normalizePath(cache_dir, winslash = "/", mustWork = FALSE)
}

# v0.71.1 — Helper pour résoudre l'output_dir des runs FORDEAD.
# Avant ce bump, `nemeton::run_fordead_dieback()` utilisait son
# défaut `tempfile(\"fordead_\")` → outputs intermédiaires (training,
# masks bruts, calibration) écrits dans /tmp puis supprimés à la
# fin (`keep_output = FALSE`). Désormais, on force l'output sous
# `<projet>/cache/layers/fordead/output_zone_<id>` :
#   - Plus de pollution /tmp.
#   - Outputs préservés (`keep_output = TRUE`) → inspection possible.
#   - **Per-zone**, écrasé à chaque relance → taille bornée
#     (50-200 Mo par zone, pas multiplié par run).
.resolve_fordead_output_dir <- function(project, zone_id) {
  if (is.null(project) || is.null(project$path)) return(NULL)
  zid <- suppressWarnings(as.integer(zone_id))
  if (length(zid) != 1L || is.na(zid)) return(NULL)
  out_dir <- file.path(project$path, "cache", "layers", "fordead",
                       sprintf("output_zone_%d", zid))
  if (!dir.exists(out_dir)) {
    tryCatch(
      dir.create(out_dir, recursive = TRUE, showWarnings = FALSE),
      error = function(e) NULL
    )
  }
  normalizePath(out_dir, winslash = "/", mustWork = FALSE)
}

# Load the project's BD Forêt V2 cache for the G3 validity check
# (v0.37.0). nemeton@v0.26.0 accepts `bdforet =` on
# `check_fordead_validity()` and derives the dominant species per
# parcel via `enrich_parcels_bdforet()` when the UGFs don't carry an
# essence column. The cache file is written by `download_ign_bdforet()`
# during the indicator compute phase
# (cf. `R/service_compute.R:1308`).
#
# Returns NULL when the project isn't loaded, no path, or the cache
# isn't built yet — caller treats NULL as "skip species check",
# matching nemeton@v0.25.9 behavior.
.load_project_bdforet <- function(project) {
  if (is.null(project) || is.null(project$path)) return(NULL)
  gpkg <- file.path(project$path, "cache", "layers", "bdforet.gpkg")
  if (!file.exists(gpkg)) return(NULL)
  tryCatch(
    sf::st_read(gpkg, quiet = TRUE),
    error = function(e) {
      cli::cli_warn(
        ".load_project_bdforet failed for {.path {gpkg}}: {conditionMessage(e)}"
      )
      NULL
    }
  )
}

# Best-effort removal of the progress.json file (and its .tmp sibling
# if the worker was interrupted mid-write). Errors are swallowed —
# leaving a stale file behind is harmless because the next invoke
# clears it before firing.
#
# v0.70.0 — Étend aussi au NDJSON append-only (brief logs FAST
# propres). Le path NDJSON est dérivé du JSON (cf. writer côté
# `service_monitoring.R::.build_progress_writer`).
.cleanup_progress_file <- function(path) {
  if (is.null(path)) return(invisible(NULL))
  ndjson <- if (grepl("\\.json$", path)) {
    sub("\\.json$", ".ndjson", path)
  } else {
    paste0(path, ".ndjson")
  }
  tryCatch(
    unlink(c(path, paste0(path, ".tmp"), ndjson)),
    error = function(e) invisible(NULL)
  )
  invisible(NULL)
}

# Make backend warning strings readable in a Shiny toast (v0.36.4).
#
# Sentinel-2 STAC + Microsoft Planetary Computer warnings can carry
# huge presigned SAS-token URLs (~400+ chars of `?st=…&se=…&sp=…&
# sv=…&sr=…&skoid=…&sktid=…&skt=…&ske=…&sks=…&skv=…&sig=…`) which
# turn the toast into a wall of text and obscure the useful part
# (HTTP status, scene id, reason). We strip those URLs to a short
# placeholder and hard-cap each warning at ~200 chars.
#
# Returns a character vector of the same length as `warns` (we
# don't dedupe/aggregate at this layer — the caller can collapse
# with " ; " to keep one entry per scene).
.summarize_backend_warnings <- function(warns, max_per_warning = 200L) {
  if (!length(warns)) return(character(0))
  vapply(as.character(warns), function(w) {
    if (is.na(w) || !nzchar(w)) return("")
    # Collapse the long Azure / STAC URL to a tiny placeholder.
    w <- gsub("https?://\\S+", "<URL>", w, perl = TRUE)
    # Whitespace normalize so multi-line warnings fit one line.
    w <- gsub("\\s+", " ", w, perl = TRUE)
    w <- trimws(w)
    if (nchar(w) > max_per_warning) {
      w <- paste0(substr(w, 1L, max_per_warning - 1L), "…")
    }
    w
  }, character(1), USE.NAMES = FALSE)
}

# Console mirror for an ingestion progress event. Called once per
# event from the reactivePoll observer (which itself only fires on
# mtime change), so we get exactly one line per scene without
# duplicates. We don't reuse the toast text verbatim because the
# console has more room for the extra fields nemeton emits
# (obs_date, cloud_pct).
.log_ingest_event <- function(ev, status, i_val, n_val, scene) {
  if (identical(status, "starting")) {
    cli::cli_alert_info("Sentinel-2 download starting ({n_val} scene(s) to process).")
    return(invisible(NULL))
  }
  # No scene_id + 0 completed = we're between the STAC search and the
  # first actual tile. Don't pollute the console with a misleading
  # "Tuile Sentinel-2 (scene_id missing) (0/N)".
  # NOTE : cette garde teste `i_val == 0L` BRUT (sémantique cœur :
  # `completed` = scènes terminées AVANT celle-ci). Le décalage 1-based
  # n'est appliqué qu'à l'affichage de la tuile en cours.
  if (!nzchar(scene) && i_val == 0L) {
    if (n_val > 0L) {
      cli::cli_alert_info("Sentinel-2 STAC search done: {n_val} scene(s) found.")
    } else {
      cli::cli_alert_info("Sentinel-2 STAC search in progress…")
    }
    return(invisible(NULL))
  }
  # v0.70.2 — Compteur 1-based pour l'affichage (brief
  # `BRIEF-nemetonshiny-console-FAST.md`, Partie B). Le cœur émet
  # `completed = i - 1` (« scènes terminées avant celle-ci »,
  # convention spec ingest pour calculer une fraction de
  # progression). Affiché tel quel, ça donnait « Tuile 0/120 » sur
  # la 1ʳᵉ scène. On affiche désormais `i_val + 1` = numéro de la
  # tuile EN COURS (1..total). La garde STAC ci-dessus continue à
  # utiliser `i_val` brut (0).
  tile_no <- i_val + 1L
  extras <- c(
    if (!is.null(ev$obs_date) && nzchar(as.character(ev$obs_date)))
      paste0(as.character(ev$obs_date)),
    if (!is.null(ev$cloud_pct) && is.finite(as.numeric(ev$cloud_pct)))
      sprintf("%.1f%% nuages", as.numeric(ev$cloud_pct)),
    if (!is.null(ev$source) && nzchar(as.character(ev$source)))
      paste0("source=", as.character(ev$source))
  )
  suffix <- if (length(extras)) sprintf(" — %s", paste(extras, collapse = ", ")) else ""
  if (identical(status, "scene_error")) {
    cli::cli_alert_warning(
      "Tuile Sentinel-2 {scene} ({tile_no}/{n_val}) — erreur{suffix}"
    )
  } else {
    cli::cli_alert_info(
      "Tuile Sentinel-2 {scene} ({tile_no}/{n_val}){suffix}"
    )
  }
  invisible(NULL)
}

# Console mirror for a Sentinel-2 band-level event
# (`s2:band_cached` / `s2:band_fetched` emitted by nemeton@v0.21.3+).
# Stays out of the toast — the scene-level event drives the UI — but
# we log each band so a developer following along in the terminal
# sees cache hits vs fresh downloads, which is the most useful info
# for diagnosing why a run is slow or large on disk.
.log_band_event <- function(ev, current_phase) {
  band  <- as.character(ev$band %||% "?")
  scene <- as.character(ev$scene_id %||% "")
  scene_short <- if (nzchar(scene)) substr(scene, 1L, 24L) else "(?)"
  if (identical(current_phase, "s2:band_cached")) {
    cli::cli_alert_info(
      "  ⤷ Bande {band} (cache) — scène {scene_short}…"
    )
  } else {
    cli::cli_alert_info(
      "  ⤷ Bande {band} (téléchargement) — scène {scene_short}…"
    )
  }
  invisible(NULL)
}

# i18n label for a FORDEAD phase name. Looks up the
# `monitoring_fordead_phase_<name>` key; if missing, returns a
# humanized Title-Cased version of the raw name so a new phase
# emitted by a future nemeton release still shows readably without
# requiring an app update.
.fordead_phase_label <- function(phase_name, i18n) {
  if (!nzchar(phase_name)) return("")
  key <- paste0("monitoring_fordead_phase_", phase_name)
  if (i18n$has(key)) {
    return(i18n$t(key))
  }
  tools::toTitleCase(gsub("_", " ", phase_name, fixed = TRUE))
}

# Dispatcher for the FORDEAD progress event stream emitted by
# nemeton@v0.22.5+. Extracted from the observe() body in
# mod_monitoring_server so it can be unit-tested directly with a
# fake session + i18n object (no reactivePoll plumbing needed).
#
# `ev`      : list parsed from the worker's JSON progress file.
# `session` : the moduleServer session (used for ns() + side-effect
#             notifications). Tests pass a stub list(ns = identity).
# `i18n`    : the get_i18n() translator.
#
# Side effects only (showNotification / removeNotification). Returns
# invisible NULL.
.fordead_handle_progress_event <- function(ev, session, i18n) {
  current <- as.character(ev$current %||% "")

  if (identical(current, "fordead:start")) {
    # Silent — the disabled "Lancer" button is enough feedback.
    return(invisible(NULL))
  }

  if (identical(current, "fordead:phase")) {
    phase_name <- as.character(ev$phase_name %||% "")
    completed  <- as.integer(ev$completed   %||% 0L)
    total      <- as.integer(ev$total       %||% 0L)
    label <- .fordead_phase_label(phase_name, i18n)
    msg <- i18n$t("monitoring_fordead_phase_progress",
                  n = completed + 1L, total = total, label = label)
    .log_fordead_event(ev, "running", completed, total, phase_name)
    shiny::showNotification(
      .monitoring_spinning_msg(msg),
      id          = session$ns("fordead_progress"),
      type        = "message",
      duration    = NULL,
      closeButton = FALSE
    )
    return(invisible(NULL))
  }

  if (identical(current, "fordead:phase_done")) {
    phase_name <- as.character(ev$phase_name %||% "")
    label <- .fordead_phase_label(phase_name, i18n)
    msg <- i18n$t("monitoring_fordead_phase_done", label = label)
    shiny::showNotification(
      msg,
      id       = session$ns("fordead_phase_done"),
      type     = "default",
      duration = 1.5
    )
    return(invisible(NULL))
  }

  if (identical(current, "fordead:complete")) {
    n_alerts <- as.integer(ev$n_alerts_inserted %||% 0L)
    duration <- as.numeric(ev$duration_sec     %||% 0)
    msg <- i18n$t("monitoring_fordead_complete",
                  n = n_alerts, sec = round(duration, 1))
    shiny::removeNotification(session$ns("fordead_progress"))
    shiny::showNotification(
      msg,
      id       = session$ns("fordead_complete"),
      type     = "message",
      duration = 8
    )
    return(invisible(NULL))
  }

  if (identical(current, "fordead:error")) {
    phase_name <- as.character(ev$phase_name    %||% "")
    err_msg    <- as.character(ev$error_message %||% "")
    msg <- i18n$t("monitoring_fordead_error",
                  phase = phase_name, msg = err_msg)
    shiny::removeNotification(session$ns("fordead_progress"))
    shiny::showNotification(
      msg,
      id          = session$ns("fordead_error"),
      type        = "error",
      duration    = NULL,
      closeButton = TRUE
    )
    return(invisible(NULL))
  }

  # ----- Legacy fallback (pre-v0.22.5 nemeton payloads) -------------
  # Same logic as before v0.32.0 — keeps the UI alive if the worker
  # emits the older status/phase shape.
  status <- ev$status %||% "running"
  if (identical(status, "done")) return(invisible(NULL))
  phase <- as.character(ev$current %||% ev$phase %||% ev$scene_id %||% "")
  i_val <- as.integer(ev$completed %||% ev$i %||% 0L)
  n_val <- as.integer(ev$total     %||% ev$n %||% 0L)
  msg <- if (identical(status, "starting") || !nzchar(phase)) {
    i18n$t("monitoring_health_starting")
  } else if (n_val > 0L) {
    sprintf(i18n$t("monitoring_health_phase_fmt"),
            phase, i_val, n_val)
  } else {
    sprintf(i18n$t("monitoring_health_phase_simple_fmt"), phase)
  }
  .log_fordead_event(ev, status, i_val, n_val, phase)
  shiny::showNotification(
    .monitoring_spinning_msg(msg),
    id          = session$ns("fordead_progress"),
    type        = if (identical(status, "phase_error")) "warning" else "message",
    duration    = NULL,
    closeButton = FALSE
  )
  invisible(NULL)
}

# Per-phase label for RECONFORT (mirror of .fordead_phase_label). The
# core emits phase_name ∈ {env, model, mask, tiles, ingest, stage,
# mapprod, collect, postprocess, persist} — each has an i18n key
# `monitoring_reconfort_phase_<name>` ; an unknown name falls back to a
# Title-Cased version of the raw key.
.reconfort_phase_label <- function(phase_name, i18n) {
  if (!nzchar(phase_name)) return("")
  key <- paste0("monitoring_reconfort_phase_", phase_name)
  if (i18n$has(key)) return(i18n$t(key))
  tools::toTitleCase(gsub("_", " ", phase_name, fixed = TRUE))
}

# Dispatcher for the RECONFORT progress event stream emitted by
# nemeton::run_reconfort_dieback() (events `reconfort:start|phase|
# complete|error`). Mirror of .fordead_handle_progress_event ; testable
# directly with a fake session + i18n. Side effects only.
.reconfort_handle_progress_event <- function(ev, session, i18n) {
  current <- as.character(ev$current %||% "")

  if (identical(current, "reconfort:start")) {
    return(invisible(NULL))  # silent — disabled button is enough
  }

  if (identical(current, "reconfort:phase")) {
    phase_name <- as.character(ev$phase_name %||% "")
    completed  <- as.integer(ev$completed   %||% 0L)
    total      <- as.integer(ev$total       %||% 0L)
    label <- .reconfort_phase_label(phase_name, i18n)
    msg <- i18n$t("monitoring_reconfort_phase_progress",
                  n = completed + 1L, total = total, label = label)
    shiny::showNotification(
      .monitoring_spinning_msg(msg),
      id          = session$ns("reconfort_progress"),
      type        = "message",
      duration    = NULL,
      closeButton = FALSE
    )
    return(invisible(NULL))
  }

  if (identical(current, "reconfort:complete")) {
    n_alerts <- as.integer(ev$n_alerts     %||% ev$n_alerts_inserted %||% 0L)
    duration <- as.numeric(ev$duration_sec %||% 0)
    shiny::removeNotification(session$ns("reconfort_progress"))
    shiny::showNotification(
      i18n$t("monitoring_reconfort_complete",
             n = n_alerts, sec = round(duration, 1)),
      id       = session$ns("reconfort_complete"),
      type     = "message",
      duration = 8
    )
    return(invisible(NULL))
  }

  if (identical(current, "reconfort:error")) {
    err_msg <- as.character(ev$error_message %||% ev$message %||% "")
    shiny::removeNotification(session$ns("reconfort_progress"))
    shiny::showNotification(
      sprintf("%s : %s", i18n$t("monitoring_reconfort_error"), err_msg),
      id          = session$ns("reconfort_error"),
      type        = "error",
      duration    = NULL,
      closeButton = TRUE
    )
    return(invisible(NULL))
  }

  invisible(NULL)
}

# Console mirror for a FORDEAD phase event. The payload is simpler
# than ingestion (no per-scene extras), so the line is just the
# phase name and the X/N counter when nemeton emits one.
.log_fordead_event <- function(ev, status, i_val, n_val, phase) {
  if (identical(status, "starting") || !nzchar(phase)) {
    cli::cli_alert_info("FORDEAD diagnosis starting.")
    return(invisible(NULL))
  }
  if (identical(status, "phase_error")) {
    cli::cli_alert_warning("FORDEAD phase {phase} — erreur")
    return(invisible(NULL))
  }
  if (n_val > 0L) {
    cli::cli_alert_info("FORDEAD phase {phase} ({i_val}/{n_val})")
  } else {
    cli::cli_alert_info("FORDEAD phase {phase}")
  }
  invisible(NULL)
}

# Wrap a message body with a spinning gear icon so the persistent
# toast (`duration = NULL`) shows the user that something is still
# happening. The `.nmt-spin` CSS keyframe is defined in
# `inst/app/www/css/custom.css` and is also re-used by the DB probe
# loading card (see `.monitoring_loading_card`).
.monitoring_spinning_msg <- function(text) {
  htmltools::tagList(
    htmltools::tags$span(
      class      = "nmt-spin me-2 text-secondary",
      style      = "display:inline-block;vertical-align:middle;",
      `aria-hidden` = "true",
      bsicons::bs_icon("gear-fill")
    ),
    htmltools::tags$span(
      style = "vertical-align:middle;",
      text
    )
  )
}

# Persistent "connecting…" / "creating local SQLite…" card shown while
# the async DB probe (ExtendedTask) is running. The spinning gear icon
# uses the existing `.nmt-spin` CSS keyframe from inst/app/www/css. The
# card replaces the previous toast-based feedback (which was easy to
# miss because Shiny notifications are top-right and auto-dismiss).
.monitoring_loading_card <- function(icon = "gear-fill", title, body = NULL) {
  htmltools::tags$div(
    class = "card mb-3 border-secondary",
    htmltools::tags$div(
      class = "card-body py-3",
      htmltools::div(
        class = "d-flex align-items-center",
        htmltools::tags$span(
          class = "me-2 fs-4 nmt-spin text-secondary",
          bsicons::bs_icon(icon)
        ),
        htmltools::tags$div(
          htmltools::tags$strong(title),
          if (!is.null(body)) htmltools::tags$div(class = "text-muted small", body)
        )
      )
    )
  )
}


.monitoring_status_card <- function(icon, class, title, body = NULL) {
  htmltools::tags$div(
    class = paste("card mb-3", class),
    htmltools::tags$div(
      class = "card-body py-3",
      htmltools::div(
        class = "d-flex align-items-center",
        bsicons::bs_icon(icon, class = "me-2 fs-4"),
        htmltools::tags$div(
          htmltools::tags$strong(title),
          if (!is.null(body)) htmltools::tags$div(class = "text-muted small", body)
        )
      )
    )
  )
}

# G3 banner — used when zone is outside FORDEAD validity domain or when
# the user opts into low/medium classes (>50% FP per ONF/DSF 2024).
.monitoring_validity_banner <- function(icon, title, body = NULL) {
  htmltools::tags$div(
    class = "card border-warning mb-3",
    htmltools::tags$div(
      class = "card-body py-2",
      htmltools::div(
        class = "d-flex align-items-start",
        bsicons::bs_icon(icon, class = "me-2 fs-4 text-warning"),
        htmltools::tags$div(
          htmltools::tags$strong(title),
          if (!is.null(body)) htmltools::tags$div(class = "small", body)
        )
      )
    )
  )
}


# v0.77.0 — Surfaces (ha) + part (%) des 4 strates projet `_tot/_feu/
# _res/_mix` pour le bandeau FAST. Lit le polygone de chaque zone via
# `get_monitoring_zone_aoi()` (EPSG:2154) et calcule l'aire avec
# `sf::st_area`. Le pourcentage est relatif à la strate `_tot` (union
# de toutes les essences, surface de référence du projet).
#
# @param con DBIConnection vers la base monitoring.
# @param zones_df data.frame `(id, name)` des zones du projet courant
#   (sortie de `nemeton::find_zones_by_project`).
# @return data.frame `(strata, label_key, ha, pct)` ordonné
#   tot → feu → res → mix, limité aux strates réellement présentes.
#   Lignes vides (aire incalculable) écartées. NULL/0-row si rien.
.compute_zone_surfaces <- function(con, zones_df) {
  if (is.null(con) || is.null(zones_df) || !nrow(zones_df)) return(NULL)
  # Mappe le suffixe du nom de zone → (strate, clé i18n du label).
  strata_map <- list(
    tot = "zone_tot", feu = "zone_feu",
    res = "zone_res", mix = "zone_mix"
  )
  rows <- lapply(names(strata_map), function(st) {
    idx <- grep(sprintf("_%s$", st), as.character(zones_df$name))
    if (!length(idx)) return(NULL)
    zid <- as.integer(zones_df$id[idx[1]])
    aoi <- tryCatch(get_monitoring_zone_aoi(con, zid),
                    error = function(e) NULL)
    if (is.null(aoi)) return(NULL)
    ha <- tryCatch(
      as.numeric(sum(sf::st_area(aoi))) / 1e4,
      error = function(e) NA_real_
    )
    if (!is.finite(ha)) return(NULL)
    data.frame(strata = st, label_key = strata_map[[st]],
               ha = ha, pct = NA_real_, stringsAsFactors = FALSE)
  })
  rows <- Filter(Negate(is.null), rows)
  if (!length(rows)) return(NULL)
  df <- do.call(rbind, rows)
  # Pourcentage relatif à `_tot` (toutes essences).
  tot_ha <- df$ha[df$strata == "tot"]
  if (length(tot_ha) == 1L && is.finite(tot_ha) && tot_ha > 0) {
    df$pct <- ifelse(df$strata == "tot", NA_real_, df$ha / tot_ha * 100)
  }
  df
}

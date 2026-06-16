# mod_validation_sampling.R — Spec 014 phase B (sous-onglet UI).
#
# Cinquième sous-onglet de Suivi sanitaire : « Plan de validation ».
# Toujours visible quand une `monitoring_zone_id` est posée (pas
# mode-driven comme les couples FAST / FORDEAD). L'utilisateur choisit
# la source d'alerte (FORDEAD ou FAST) et génère un plan
# d'échantillonnage de validation terrain via le cœur
# `nemeton::create_validation_sampling_plan()`, encapsulé côté app par
# `generate_validation_plan()`.
#
# Sortie : carte Leaflet (raster d'alerte + markers Validation / Témoin),
# table DT du plan, bouton « Persister dans samples.gpkg » et bouton
# « Exporter pour QGIS » qui produit un .qgz dans <project>/exports/.


#' Plan de validation sub-tab UI
#'
#' @param id Module namespace id.
#' @param source Optional character `"FAST"` or `"FORDEAD"`. When
#'   provided, the radio button « Source d'alerte » is omitted from
#'   the sidebar — the source is **fixed** for this instance. This is
#'   the mode used by `mod_monitoring`, which mounts two instances of
#'   the module (one per source, mode-driven visibility like the
#'   FAST / FORDEAD alerts and pixel map couples). v0.43.3.
#'   When `NULL`, the radio is shown (legacy behaviour).
#' @noRd
mod_validation_sampling_ui <- function(id, source = NULL) {
  ns <- shiny::NS(id)
  i18n <- get_i18n("fr")
  radio_ui <- if (is.null(source)) {
    shiny::radioButtons(
      ns("source"),
      label  = i18n$t("validation_source_label"),
      inline = TRUE,
      choices = c("FORDEAD" = "FORDEAD", "FAST" = "FAST"),
      selected = "FORDEAD"
    )
  } else {
    # Fixed source — surface it as a static badge so the user knows
    # *which* source this sub-tab targets without exposing a control.
    htmltools::div(
      class = "mb-2",
      htmltools::tags$small(class = "text-muted",
                            i18n$t("validation_source_label"), ":"),
      htmltools::tags$strong(
        class = "ms-1",
        if (identical(source, "FAST")) i18n$t("validation_source_fast_trend")
        else as.character(source)
      )
    )
  }
  is_fast <- identical(source, "FAST")
  # ----- Paramètres : trend (FAST, spec 025) vs catégoriel (FORDEAD/RECONFORT)
  param_ui <- if (is_fast) {
    # Plan sanitaire branché sur le trend (déclin pluriannuel) — pas de
    # classes/témoins/tampon : la pondération est continue (|pente|).
    htmltools::tagList(
      shiny::selectInput(
        ns("trend_index"), i18n$t("validation_trend_index_label"),
        choices = c("NDRE" = "NDRE", "NDMI" = "NDMI"), selected = "NDRE"
      ),
      shiny::dateRangeInput(
        ns("trend_win"), i18n$t("validation_trend_window_label"),
        start = "2017-01-01", end = Sys.Date(),
        weekstart = 1, separator = " → "
      ),
      shiny::numericInput(
        ns("n_validation"), i18n$t("validation_trend_n_plots_label"),
        value = 20L, min = 1L, max = 200L, step = 1L
      ),
      shiny::numericInput(
        ns("n_control"), i18n$t("validation_trend_n_control_label"),
        value = 5L, min = 0L, max = 100L, step = 1L
      ),
      shiny::numericInput(
        ns("seed"), i18n$t("validation_seed_label"),
        value = 42L, min = 0L, max = 1e6L, step = 1L
      ),
      bslib::accordion(
        open = FALSE,
        bslib::accordion_panel(
          i18n$t("validation_trend_advanced_label"),
          shiny::sliderInput(
            ns("trend_months"), i18n$t("monitoring_trend_months"),
            min = 1, max = 12, value = c(6, 9), step = 1
          ),
          shiny::numericInput(
            ns("trend_min_years"), i18n$t("monitoring_trend_min_years"),
            value = 4, min = 2, max = 20, step = 1
          ),
          shiny::numericInput(
            ns("trend_min_obs"), i18n$t("validation_trend_min_obs_label"),
            value = 2, min = 1, max = 10, step = 1
          ),
          shiny::numericInput(
            ns("trend_alpha"), i18n$t("monitoring_trend_alpha"),
            value = 0.05, min = 0.001, max = 0.2, step = 0.005
          )
        )
      )
    )
  } else {
    htmltools::tagList(
      shiny::numericInput(
        ns("n_validation"),
        label = i18n$t("validation_n_validation_label"),
        value = 20L, min = 1L, max = 200L, step = 1L
      ),
      shiny::numericInput(
        ns("n_control"),
        label = i18n$t("validation_n_control_label"),
        value = 5L, min = 0L, max = 100L, step = 1L
      ),
      # spec 021 — RECONFORT : masque catégoriel 1/2/3, alertes c(2,3),
      # témoins c(1). FORDEAD : échelle 0-4.
      if (identical(source, "RECONFORT")) {
        shiny::checkboxGroupInput(
          ns("classes"),
          label   = i18n$t("validation_classes_label"),
          choices = stats::setNames(
            c("2", "3", "1"),
            c(i18n$t("reconfort_class_label_2"),
              i18n$t("reconfort_class_label_3"),
              i18n$t("reconfort_class_label_1"))
          ),
          selected = c("2", "3"),
          inline = FALSE
        )
      } else {
        shiny::checkboxGroupInput(
          ns("classes"),
          label   = i18n$t("validation_classes_label"),
          # Ordre décroissant de sévérité : 4, 3, 2, 1.
          choices = stats::setNames(
            c("4", "3", "2", "1"),
            c(i18n$t("validation_class_fordead_4"),
              i18n$t("validation_class_fordead_3"),
              i18n$t("validation_class_fordead_2"),
              i18n$t("validation_class_fordead_1"))
          ),
          selected = c("3", "4"),
          inline = FALSE
        )
      },
      if (identical(source, "RECONFORT")) {
        shiny::checkboxGroupInput(
          ns("control_classes"),
          label   = i18n$t("validation_control_classes_label"),
          choices = c("1", "2", "3"),
          selected = "1",
          inline = TRUE
        )
      } else {
        shiny::checkboxGroupInput(
          ns("control_classes"),
          label   = i18n$t("validation_control_classes_label"),
          choices = c("0", "1", "2", "3", "4"),
          selected = "0",
          inline = TRUE
        )
      },
      shiny::uiOutput(ns("class_distribution")),
      shiny::numericInput(
        ns("buffer_m"),
        label = i18n$t("validation_buffer_label"),
        value = 0, min = 0, max = 100, step = 5
      ),
      shiny::numericInput(
        ns("seed"),
        label = i18n$t("validation_seed_label"),
        value = 42L, min = 0L, max = 1e6L, step = 1L
      )
    )
  }

  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      width = 320,
      title = i18n$t("validation_sampling_title"),

      radio_ui,
      param_ui,
      shiny::actionButton(
        ns("generate"),
        label = if (is_fast) i18n$t("validation_trend_generate_btn")
                else i18n$t("validation_generate_btn"),
        icon  = shiny::icon("play"),
        class = "btn-primary mt-2"
      ),
      shiny::hr(),
      shiny::actionButton(
        ns("persist"),
        label = i18n$t("validation_persist_btn"),
        icon  = shiny::icon("save"),
        class = "btn-outline-secondary",
        disabled = NA
      ),
      shiny::downloadButton(
        ns("export_qgis"),
        label = i18n$t("validation_export_qgis_btn"),
        icon  = shiny::icon("file-export"),
        class = "btn-outline-secondary",
        disabled = NA
      )
    ),
    shiny::uiOutput(ns("panel")),
    shiny::uiOutput(ns("table_panel"))
  )
}


#' Plan de validation sub-tab server
#'
#' @param id Module namespace id.
#' @param app_state Parent reactiveValues with `language`,
#'   `current_project`.
#' @param zone_id_r Reactive returning the active monitoring zone id.
#' @param mode_r Reactive returning the parent sidebar mode
#'   (`"quick"` / `"health"`). When `source_fixed` is NULL, drives
#'   the default of the inner `source` radio. Unused when the source
#'   is fixed.
#' @param thresholds_r Reactive returning the FAST sidebar thresholds
#'   `list(ndvi, nbr, window_days)`.
#' @param date_range_r Reactive returning the parent date range.
#' @param source_fixed Optional character `"FAST"` or `"FORDEAD"`.
#'   When non-NULL, the module ignores `input$source` (which the UI
#'   omits) and uses this value verbatim. Pair with
#'   [mod_validation_sampling_ui()] called with the same `source`.
#'   v0.43.3.
#' @noRd
mod_validation_sampling_server <- function(id, app_state,
                                           zone_id_r,
                                           mode_r,
                                           thresholds_r,
                                           date_range_r,
                                           source_fixed = NULL) {
  shiny::moduleServer(id, function(input, output, session) {

    i18n_r <- shiny::reactive({
      get_i18n(app_state$language %||% "fr")
    })

    # Reactive that returns the active source for this module instance.
    # Either the fixed value passed at construction, or whatever the
    # user picked in the radio.
    current_source <- shiny::reactive({
      if (!is.null(source_fixed)) return(as.character(source_fixed))
      as.character(input$source %||% "FORDEAD")
    })

    # Default the source radio to whatever the parent sidebar mode is
    # at first sub-tab open. After that the user owns the choice — the
    # observer only fires once per parent-mode flip.
    # No-op when source is fixed (the radio doesn't exist).
    if (is.null(source_fixed)) {
      last_mode <- shiny::reactiveVal(NULL)
      shiny::observe({
        m <- mode_r()
        if (is.null(m) || identical(m, last_mode())) return()
        last_mode(m)
        target <- if (identical(m, "quick")) "FAST" else "FORDEAD"
        if (!identical(input$source, target)) {
          shiny::updateRadioButtons(session, "source", selected = target)
        }
      })
    }

    # v0.45.0 — label unification : pour FAST, charge best-effort le
    # raster d'alerte continu et calcule les labels quartile via
    # `.fast_class_labels()`, partagé avec Alertes FAST. Pour FORDEAD,
    # labels biologiques statiques. Le résultat alimente le checkbox
    # `classes` via updateCheckboxGroupInput (préserve la sélection
    # courante).
    preview_raster_r <- shiny::reactive({
      if (!identical(current_source(), "FAST")) return(NULL)
      proj <- app_state$current_project
      if (is.null(proj) || is.null(proj$path)) return(NULL)
      zone_id <- suppressWarnings(as.integer(
        proj$metadata$monitoring_zone_id))
      # v0.48.1 — guard longueur nulle : un projet sans zone
      # enregistrée a `monitoring_zone_id == NULL`, donc
      # `as.integer(NULL)` retourne `integer(0)` et `is.na(integer(0))`
      # vaut `logical(0)` → `if` plantait « argument de longueur
      # nulle ». On vérifie length avant is.na.
      if (length(zone_id) != 1L || is.na(zone_id)) return(NULL)
      th <- thresholds_r()
      dr <- date_range_r()
      if (is.null(th$ndvi) || is.null(th$nbr) ||
          length(dr) != 2L || any(is.na(dr))) return(NULL)
      cd <- file.path(proj$path, "cache", "layers", "sentinel2")
      if (!dir.exists(cd)) return(NULL)
      con <- get_monitoring_db_connection(project = proj, read_only = TRUE)
      if (is.null(con)) return(NULL)
      on.exit(close_monitoring_db_connection(con), add = TRUE)
      # v0.52.13 — API mono-index (nemeton@v0.55.0, spec 017).
      # v0.52.15 — cache D6 (nemeton@v0.57.0) sur la prévisualisation
      # FAST : un clic sur l'aperçu après changement de seuil/index est
      # désormais instantané si le COG résultat est déjà sur disque.
      idx <- th$index %||% "NDVI"
      # NDMI / NDRE use their own thresholds (th$ndmi / th$ndre),
      # falling back to NDVI's when absent.
      thr <- switch(idx,
                    NBR  = th$nbr,
                    NDMI = th$ndmi %||% th$ndvi,
                    NDRE = th$ndre %||% th$ndmi %||% th$ndvi,
                    th$ndvi)
      # v0.55.0 — helper unique partagé (cohérence hash D6).
      result_cache <- .fast_alert_cache_dir(proj$path)
      tryCatch(
        nemeton::read_fast_alert_raster(
          con,
          zone_id          = zone_id,
          index            = idx,
          threshold        = as.numeric(thr),
          date_from        = dr[1],
          date_to          = dr[2],
          mode             = "count",
          window_days      = as.integer(th$window_days %||% 30L),
          cache_dir        = cd,
          cache_result     = TRUE,
          result_cache_dir = result_cache
        ),
        error = function(e) NULL
      )
    })

    shiny::observe({
      i18n <- i18n_r()
      src  <- current_source()
      # spec 021 — RECONFORT a une échelle catégorielle 1/2/3 (≠ 0-4
      # FORDEAD/FAST) et ses libellés sont fixés statiquement à l'UI.
      # `.fast_class_labels()` ne connaît que FAST/FORDEAD (match.arg) et
      # réécrirait les choix sur c("3","4","1","2") — on saute donc cet
      # observer pour RECONFORT. FAST utilise désormais le trend (spec 025)
      # sans cases « classes » → on saute aussi.
      if (identical(src, "RECONFORT") || identical(src, "FAST")) return()
      r    <- preview_raster_r()
      labels <- .fast_class_labels(r, source = src,
                                   mode = "count", i18n = i18n)
      # Ordre décroissant de sévérité : 4, 3, 2, 1.
      shiny::updateCheckboxGroupInput(
        session, "classes",
        choices = stats::setNames(
          c("4", "3", "2", "1"),
          c(labels[["4"]], labels[["3"]],
            labels[["2"]], labels[["1"]])
        ),
        selected = input$classes %||% c("3", "4")
      )
    })

    # Classified alert mask (0-4) for the current source/zone, read from
    # the project cache (no compute — best-effort). This is the raster
    # whose class values the `classes` / `control_classes` filters act on,
    # so its distribution drives both the on-screen hint and the
    # control-class auto-relax. Returns NULL when no cached mask exists.
    alert_mask_r <- shiny::reactive({
      # PERF/UX — ne toucher la base monitoring + lire le raster d'alerte
      # QUE lorsque l'onglet Santé est réellement actif. Sinon cette
      # reactive se ré-évalue à chaque changement de `current_project` (donc
      # à chaque chargement de projet depuis l'Accueil), ouvrant une
      # connexion DB + lisant un raster hors du chemin de chargement, et
      # l'observateur auto-relax ci-dessous fait fuiter son toast « Aucun
      # pixel sain » par-dessus la carte. Même garde que `pixel_stack_r`
      # (v0.75.2) : `active_main_tab` est exposé par app_server.
      shiny::req(identical(app_state$active_main_tab, "monitoring"))
      proj <- app_state$current_project
      if (is.null(proj) || is.null(proj$path)) return(NULL)
      zid <- suppressWarnings(as.integer(proj$metadata$monitoring_zone_id))
      if (length(zid) != 1L || is.na(zid)) return(NULL)
      src <- current_source()
      cd <- file.path(proj$path, "cache", "layers",
                      switch(src,
                             FORDEAD   = "fordead",
                             RECONFORT = "reconfort",
                             "fast_sampling"))
      if (!dir.exists(cd)) return(NULL)
      con <- get_monitoring_db_connection(project = proj, read_only = TRUE)
      if (is.null(con)) return(NULL)
      on.exit(close_monitoring_db_connection(con), add = TRUE)
      tryCatch(
        if (identical(src, "FORDEAD"))
          nemeton::read_fordead_dieback_mask(con, zid, cache_dir = cd)
        else if (identical(src, "RECONFORT"))
          nemeton::read_reconfort_alert_mask(con, zid, cache_dir = cd)
        else
          nemeton::read_fast_alert_mask(con, zid, cache_dir = cd),
        error = function(e) NULL
      )
    })

    class_distribution_r <- shiny::reactive({
      .validation_class_distribution(alert_mask_r())
    })

    # Distribution hint : "Distribution du raster : 0=0, 1=0, …, 4=8471".
    # Plus a help note when no healthy (class 0) pixel exists.
    output$class_distribution <- shiny::renderUI({
      i18n <- i18n_r()
      dist <- class_distribution_r()
      if (is.null(dist)) return(NULL)
      body <- paste(sprintf("%s=%d", names(dist), dist), collapse = ", ")
      note <- if (isTRUE((dist[["0"]] %||% 0L) == 0L)) {
        htmltools::tags$div(
          class = "text-warning small mt-1",
          i18n$t("validation_no_healthy_pixel_hint"))
      } else NULL
      htmltools::div(
        class = "small text-muted mt-1 mb-2",
        htmltools::tags$em(sprintf(
          i18n$t("validation_class_distribution_fmt"), body)),
        note
      )
    })

    # Auto-relax : when the alert mask has no class-0 cell and the user
    # has not touched `control_classes` (still the c("0") default), switch
    # the selection to the lowest present class so control plots can be
    # drawn at all. Fires once.
    auto_relax_done <- shiny::reactiveVal(FALSE)
    shiny::observe({
      # FAST = trend (pas de masque catégoriel ni de cases témoins).
      if (identical(current_source(), "FAST")) return()
      if (isTRUE(auto_relax_done())) return()
      dist <- class_distribution_r()
      if (is.null(dist)) return()
      present <- as.integer(names(dist))[dist > 0L]
      if (!length(present)) return()
      cur <- input$control_classes %||% "0"
      if (!(0L %in% present) && identical(sort(cur), "0")) {
        relaxed <- as.character(min(present))
        shiny::updateCheckboxGroupInput(session, "control_classes",
                                        selected = relaxed)
        auto_relax_done(TRUE)
        shiny::showNotification(
          sprintf(i18n_r()$t("validation_control_auto_relaxed"), relaxed),
          type = "warning", duration = 6
        )
      }
    })

    # The plan reactive : only fires on the Generate button. eventReactive
    # makes input changes invalidate the value silently — the user must
    # re-click Generate to get a fresh plan.
    plan_rv <- shiny::reactiveVal(NULL)
    plan_error <- shiny::reactiveVal(NULL)

    shiny::observeEvent(input$generate, {
      plan_rv(NULL)
      plan_error(NULL)
      proj <- app_state$current_project
      if (is.null(proj)) return()
      con <- get_monitoring_db_connection(project = proj)
      if (is.null(con)) {
        plan_error(list(class = "validation_no_db",
                        message = "Monitoring DB not configured"))
        return()
      }
      on.exit(close_monitoring_db_connection(con), add = TRUE)

      # ----- FAST = plan sanitaire branché sur le trend (spec 025) -------
      # Pondération continue par |pente| ; pas de classes/témoins/tampon.
      if (identical(current_source(), "FAST")) {
        win <- input$trend_win
        tm  <- input$trend_months %||% c(6L, 9L)
        res <- tryCatch(
          generate_trend_sanitary_plan(
            con              = con,
            project          = proj,
            index            = input$trend_index %||% "NDRE",
            n_plots          = as.integer(input$n_validation %||% 20L),
            n_control        = as.integer(input$n_control %||% 5L),
            date_from        = if (length(win) == 2L) win[1] else NULL,
            date_to          = if (length(win) == 2L) win[2] else NULL,
            months           = seq.int(as.integer(tm[1]), as.integer(tm[2])),
            min_years        = as.integer(input$trend_min_years %||% 4L),
            min_obs_per_year = as.integer(input$trend_min_obs %||% 2L),
            alpha            = as.numeric(input$trend_alpha %||% 0.05),
            seed             = as.integer(input$seed %||% 42L)
          ),
          error = function(e) e
        )
        if (inherits(res, "error")) {
          plan_error(list(class = setdiff(class(res),
                                          c("error", "condition")),
                          message = conditionMessage(res)))
          return()
        }
        plan_rv(res)
        n_temoin <- tryCatch(
          sum(as.character(res$type) == "Temoin", na.rm = TRUE),
          error = function(e) 0L
        )
        if (as.integer(input$n_control %||% 5L) > 0L && n_temoin == 0L) {
          shiny::showNotification(
            i18n_r()$t("validation_no_control_warning"),
            type = "warning", duration = 8
          )
        }
        return()
      }

      # spec 021 — RECONFORT : classes catégorielles 1/2/3 (alertes 2/3,
      # témoin 1) au lieu de l'échelle 0-4 FORDEAD/FAST. Les défauts
      # ci-dessous ne servent que si la sélection est vidée (input NULL).
      is_reconfort <- identical(current_source(), "RECONFORT")
      classes_int <- as.integer(
        input$classes %||% (if (is_reconfort) c("2", "3") else c("3", "4")))
      control_int <- as.integer(
        input$control_classes %||% (if (is_reconfort) "1" else "0"))
      n_control_req <- as.integer(input$n_control %||% 5L)
      th <- thresholds_r()
      dr <- date_range_r()
      res <- tryCatch(
        generate_validation_plan(
          con             = con,
          project         = proj,
          source          = current_source(),
          n_validation    = as.integer(input$n_validation %||% 20L),
          n_control       = n_control_req,
          classes         = classes_int,
          control_classes = control_int,
          buffer_m        = as.numeric(input$buffer_m %||% 0),
          seed            = as.integer(input$seed %||% 42L),
          ndvi_threshold  = th$ndvi,
          nbr_threshold   = th$nbr,
          window_days     = th$window_days %||% 30L,
          date_from       = if (length(dr) == 2L) dr[1] else NULL,
          date_to         = if (length(dr) == 2L) dr[2] else NULL,
          # v0.52.15 — index mono-indice (spec 017). `th$index` est
          # alimenté par le retour `index_r` de mod_monitoring_fast_alerts
          # (cf. v0.52.14) pour le source FAST. Source FORDEAD est
          # `index`-agnostic mais le param est inoffensif.
          index           = th$index
        ),
        error = function(e) e
      )
      if (inherits(res, "error")) {
        plan_error(list(class = setdiff(class(res), c("error",
                                                      "condition")),
                        message = conditionMessage(res)))
        return()
      }
      plan_rv(res)

      # Deliverable 5 — the cœur warns (cli) "No cell matching
      # control_classes" and returns 0 témoins silently. Surface a clean
      # UI toast when control plots were requested but none came back.
      n_temoin <- tryCatch(
        sum(as.character(res$type) == "Temoin", na.rm = TRUE),
        error = function(e) 0L
      )
      if (n_control_req > 0L && n_temoin == 0L) {
        shiny::showNotification(
          i18n_r()$t("validation_no_control_warning"),
          type = "warning", duration = 8
        )
      }
    })

    # Invalidate the result silently if the user touches any input
    # after the plan was generated — the toast hint is rendered in the
    # panel state.
    shiny::observe({
      # Catégoriel (FORDEAD/RECONFORT) + trend (FAST) : on dépend des deux
      # jeux d'inputs ; ceux absents dans un mode valent NULL (sans effet).
      input$source; input$n_validation; input$n_control;
      input$classes; input$control_classes; input$buffer_m; input$seed
      input$trend_index; input$trend_win; input$trend_months
      input$trend_min_years; input$trend_min_obs; input$trend_alpha
      # Only invalidate if a plan exists already.
      if (!is.null(plan_rv())) {
        plan_rv(NULL)
        plan_error(NULL)
      }
    }) |> shiny::bindEvent(
      input$source, input$n_validation, input$n_control,
      input$classes, input$control_classes, input$buffer_m, input$seed,
      input$trend_index, input$trend_win, input$trend_months,
      input$trend_min_years, input$trend_min_obs, input$trend_alpha,
      ignoreInit = TRUE
    )

    # Cache_dir resolver for the alert raster overlay on the map.
    alert_raster_r <- shiny::reactive({
      plan <- plan_rv()
      if (is.null(plan)) return(NULL)
      proj <- app_state$current_project
      if (is.null(proj) || is.null(proj$path)) return(NULL)
      zone_id <- as.integer(plan$zone_id[1])
      src <- as.character(plan$source[1])
      cd <- file.path(proj$path, "cache", "layers",
                      switch(src,
                             FORDEAD   = "fordead",
                             RECONFORT = "reconfort",
                             "fast_sampling"))
      con <- get_monitoring_db_connection(project = proj, read_only = TRUE)
      if (is.null(con)) return(NULL)
      on.exit(close_monitoring_db_connection(con), add = TRUE)
      tryCatch(
        if (identical(src, "FORDEAD")) {
          nemeton::read_fordead_dieback_mask(con, zone_id, cache_dir = cd)
        } else if (identical(src, "RECONFORT")) {
          nemeton::read_reconfort_alert_mask(con, zone_id, cache_dir = cd)
        } else {
          nemeton::read_fast_alert_mask(con, zone_id, cache_dir = cd)
        },
        error = function(e) NULL
      )
    })

    # Panel : empty state, error state, generating state, or the map.
    output$panel <- shiny::renderUI({
      i18n <- i18n_r()
      err  <- plan_error()
      plan <- plan_rv()

      if (!is.null(err)) {
        # FAST trend : aucun déclin significatif sur la fenêtre → message
        # dédié (≠ « zone saine » du masque catégoriel count/rolling).
        empty_trend <- "validation_empty_trend" %in% err$class
        empty_mask  <- "validation_empty_mask" %in% err$class
        title_key <- if (empty_trend) {
          "validation_empty_trend_title"
        } else if (empty_mask) {
          "validation_empty_mask_title"
        } else {
          "validation_no_mask_title"
        }
        body_key <- if (empty_trend) {
          "validation_empty_trend_body"
        } else if (empty_mask) {
          "validation_empty_mask_body"
        } else {
          "validation_no_mask_body"
        }
        css <- if (empty_trend || empty_mask)
          "alert alert-success" else "alert alert-warning"
        return(htmltools::div(
          class = paste("p-4 m-3", css),
          htmltools::h5(i18n$t(title_key)),
          htmltools::p(i18n$t(body_key)),
          htmltools::tags$small(class = "text-muted", err$message)
        ))
      }
      if (is.null(plan)) {
        return(htmltools::div(
          class = "p-4 text-center text-muted",
          bsicons::bs_icon("compass",
                           class = "fs-1 d-block mx-auto mb-3"),
          htmltools::p(i18n$t("validation_idle_hint"))
        ))
      }
      leaflet::leafletOutput(session$ns("map"), height = "55vh")
    })

    output$map <- leaflet::renderLeaflet({
      plan <- plan_rv()
      if (is.null(plan) || !nrow(plan)) return(NULL)
      i18n <- i18n_r()

      plan_ll <- sf::st_transform(plan, 4326)
      coords <- sf::st_coordinates(plan_ll)
      types  <- as.character(plan_ll$type)
      # FAST trend : plan sanitaire (source "FAST_TREND"), couleur continue
      # par sévérité (|pente|) ; FORDEAD/RECONFORT : 2 couleurs catégorielles.
      is_trend <- identical(as.character(plan_ll$source[1] %||% ""),
                            "FAST_TREND")

      sev_pal <- NULL
      if (is_trend) {
        av <- suppressWarnings(as.numeric(plan_ll$alert_value))
        av[is.na(av)] <- 0
        sev_max <- max(av, 0, na.rm = TRUE)
        sev_pal <- leaflet::colorNumeric(
          c("#2CA02C", "#FFD27F", "#FF9933", "#D62728"),
          domain = c(0, if (sev_max > 0) sev_max else 1)
        )
        is_temoin <- types == "Temoin"
        fill <- ifelse(is_temoin, "#7F7F7F", sev_pal(av))
        idx_lbl <- as.character(plan_ll$index[1] %||% "NDRE")
        popups <- vapply(seq_len(nrow(plan_ll)), function(i) {
          sprintf(
            "<strong>%s</strong><br/>%s<br/>%s : %s",
            htmltools::htmlEscape(as.character(plan_ll$plot_id[i])),
            htmltools::htmlEscape(types[i]),
            htmltools::htmlEscape(idx_lbl),
            if (is_temoin[i]) "0" else sprintf("%+.4f /an", av[i])
          )
        }, character(1))
      } else {
        colors <- c(Validation = "#2CA02C", Temoin = "#7F7F7F")
        types[!types %in% names(colors)] <- "Validation"
        fill <- unname(colors[types])
        popups <- vapply(seq_len(nrow(plan_ll)), function(i) {
          sprintf("<strong>%s</strong><br/>%s — class %s<br/>visit_order: %d",
                  htmltools::htmlEscape(as.character(plan_ll$plot_id[i])),
                  htmltools::htmlEscape(types[i]),
                  htmltools::htmlEscape(as.character(
                    plan_ll$alert_class[i] %||% NA)),
                  as.integer(plan_ll$visit_order[i] %||% NA))
        }, character(1))
      }

      # v0.45.0 — overlay UGF (polygones bleu vif) pour le repère spatial.
      ugf_4326 <- .ugf_for_overlay(app_state$current_project)

      m <- leaflet::leaflet() |>
        leaflet::addProviderTiles("OpenStreetMap",     group = "OSM") |>
        leaflet::addProviderTiles("Esri.WorldImagery", group = "Satellite") |>
        leaflet::addLayersControl(
          baseGroups    = c("OSM", "Satellite"),
          overlayGroups = if (!is.null(ugf_4326)) "UGF" else NULL,
          options       = leaflet::layersControlOptions(collapsed = TRUE)
        )

      # Overlay du masque catégoriel sous les marqueurs — uniquement
      # FORDEAD/RECONFORT (le plan trend ne repose pas sur un masque 0-4).
      if (!is_trend) {
        r <- alert_raster_r()
        if (!is.null(r) && inherits(r, "SpatRaster") &&
            terra::ncell(r) > 0L) {
          r_show <- terra::ifel(r == 0, NA, r)
          cats <- 0:4
          cols <- c("#2CA02C", "#FFD27F", "#FF9933", "#D62728", "#222222")
          pal <- leaflet::colorFactor(cols, levels = cats,
                                      na.color = "transparent")
          m <- m |>
            leaflet::addRasterImage(r_show, colors = pal, opacity = 0.55,
                                    method = "ngb")
        }
      }

      if (!is.null(ugf_4326)) {
        m <- m |>
          leaflet::addPolygons(
            data        = ugf_4326,
            group       = "UGF",
            color       = "#1f78b4",
            weight      = 2,
            opacity     = 0.9,
            fillOpacity = 0
          )
      }

      m <- m |>
        leaflet::addCircleMarkers(
          lng = coords[, "X"], lat = coords[, "Y"],
          radius = 7, weight = 1, color = "#000",
          fillColor = fill, fillOpacity = 0.95,
          popup = popups
        )

      if (is_trend) {
        m |>
          leaflet::addLegend(
            position = "bottomright", pal = sev_pal, values = av,
            title = sprintf(i18n$t("validation_trend_legend_title"), idx_lbl),
            opacity = 0.95
          ) |>
          leaflet::addLegend(
            position = "bottomright", colors = "#7F7F7F",
            labels = i18n$t("validation_legend_temoin"), opacity = 0.95
          )
      } else {
        m |>
          leaflet::addLegend(
            position = "bottomright",
            colors   = unname(colors),
            labels   = c(i18n$t("validation_legend_validation"),
                         i18n$t("validation_legend_temoin")),
            title    = NULL,
            opacity  = 0.95
          )
      }
    })

    output$table_panel <- shiny::renderUI({
      if (is.null(plan_rv())) return(NULL)
      htmltools::div(
        class = "p-3",
        DT::DTOutput(session$ns("table"))
      )
    })

    output$table <- DT::renderDT({
      plan <- plan_rv()
      if (is.null(plan)) return(NULL)
      df <- sf::st_drop_geometry(plan)
      keep <- intersect(
        c("plot_id", "type", "alert_value", "index", "alert_class",
          "visit_order", "source", "source_run_id", "generated_at"),
        names(df)
      )
      DT::datatable(df[, keep, drop = FALSE], rownames = FALSE,
                    options = list(pageLength = 10, dom = "tip"))
    })

    # Persist + Export QGIS buttons : disable state mirrors plan_rv().
    shiny::observe({
      enabled <- !is.null(plan_rv())
      shiny::updateActionButton(session, "persist",
                                disabled = !enabled)
      session$sendCustomMessage(
        "shiny:disabled",
        list(id = session$ns("export_qgis"), disabled = !enabled)
      )
    })

    shiny::observeEvent(input$persist, {
      i18n <- i18n_r()
      plan <- plan_rv()
      proj <- app_state$current_project
      if (is.null(plan) || is.null(proj) || is.null(proj$path)) return()
      n <- tryCatch(
        persist_validation_plan(plan, proj$path),
        error = function(e) e
      )
      if (inherits(n, "error")) {
        shiny::showNotification(
          sprintf("%s: %s", i18n$t("validation_persist_btn"),
                  conditionMessage(n)),
          type = "error", duration = 8
        )
        return()
      }
      shiny::showNotification(
        sprintf(i18n$t("validation_persisted_toast"), as.integer(n)),
        type = "message", duration = 5
      )
    })

    output$export_qgis <- shiny::downloadHandler(
      filename = function() {
        sprintf("validation_%s.qgz", format(Sys.time(), "%Y%m%dT%H%M%S"))
      },
      content = function(file) {
        i18n <- i18n_r()
        plan <- plan_rv()
        proj <- app_state$current_project
        if (is.null(plan) || is.null(proj)) {
          file.create(file)
          return(invisible())
        }
        out_dir <- tempfile("validation-qgis-")
        dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
        zone_etude <- if (!is.null(proj$indicators_sf)) {
          tryCatch(sf::st_transform(proj$indicators_sf, 2154),
                   error = function(e) NULL)
        } else NULL
        qgz <- tryCatch(
          nemeton::create_qgis_project(
            placettes    = plan,
            zone_etude   = zone_etude,
            parcours_tsp = NULL,
            output_dir   = out_dir,
            project_name = sprintf("validation_%s",
                                   format(Sys.time(), "%Y%m%dT%H%M%S")),
            crs          = 2154,
            region       = "BFC",
            lang         = app_state$language %||% "fr",
            overwrite    = TRUE
          ),
          error = function(e) {
            cli::cli_alert_warning(
              "create_qgis_project failed: {e$message}"
            )
            NULL
          }
        )
        if (is.null(qgz) || !file.exists(qgz)) {
          file.create(file)
          return(invisible())
        }
        file.copy(qgz, file, overwrite = TRUE)
        shiny::showNotification(
          sprintf(i18n$t("validation_qgis_exported_toast"),
                  basename(qgz)),
          type = "message", duration = 6
        )
      }
    )

    # Force the panel renderUI to evaluate while the sub-tab is
    # hidden, same defensive pattern as the other Suivi sanitaire
    # sub-modules (bslib nav_show/hide is unreliable for output
    # suspension).
    shiny::outputOptions(output, "panel",       suspendWhenHidden = FALSE)
    shiny::outputOptions(output, "table_panel", suspendWhenHidden = FALSE)

    invisible(list(plan = plan_rv))
  })
}


# Class frequency (0-4) of an alert mask SpatRaster. Returns a named
# integer vector ("0".."4") with NA cells dropped, or NULL when the
# raster is unusable. Used by the distribution hint and the
# control-class auto-relax.
.validation_class_distribution <- function(r) {
  if (is.null(r) || !inherits(r, "SpatRaster") ||
      terra::ncell(r) == 0L) {
    return(NULL)
  }
  vals <- tryCatch(terra::values(r, mat = FALSE), error = function(e) NULL)
  if (is.null(vals)) return(NULL)
  vals <- vals[!is.na(vals)]
  if (!length(vals)) return(NULL)
  tab <- table(factor(as.integer(vals), levels = 0:4))
  stats::setNames(as.integer(tab), names(tab))
}

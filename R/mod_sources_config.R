#' Optional Theia sources configuration module
#'
#' @description
#' The two **opt-in Theia sources** of the application, grouped in their own
#' tab of the settings (gear) modal:
#'
#'   * **Coupes rases (SUFOSAT)** - national Sentinel-1 clear-cut detection
#'     feeding the T3 indicator (spec 030): toggle + `window_years` /
#'     `min_proba`.
#'   * **Rafraichissement urbain (LST)** - Theia/Thermocity surface coolness
#'     feeding the A5 indicator (spec 032): toggle + `buffer_m`.
#'
#' Both blocks used to live in the project card (`mod_project`), where they
#' stretched an already long form and were easy to miss. They belong with the
#' other external-service settings, next to the Theia credentials they depend
#' on - hence this module, mounted as a tab of `mod_theia_config`'s modal.
#'
#' Both sources are **enabled by default** (see `project_sufosat_enabled()` /
#' `project_lst_enabled()`): a project that never visited this tab still gets
#' T3 and A5. The Theia fetch stays gated on credentials being configured, and
#' a failed / out-of-coverage fetch degrades to `NA` per unit - never an error.
#'
#' @name mod_sources_config
#' @keywords internal
NULL


#' Render an applicability verdict as a one-line badge
#'
#' @description
#' Three levels, same vocabulary as the source statuses: green when the
#' indicator applies, neutral grey when it legitimately will not (or will with
#' extrapolated confidence), amber when the question could not be answered.
#'
#' A legitimate non-applicability is **not** a warning: a forest outside
#' Thermocity coverage has nothing to fix.
#'
#' @param msg List from [applicabilite_message()], or `NULL`.
#'
#' @return A `div`, or `NULL` when there is nothing to say.
#'
#' @noRd
.applicabilite_badge <- function(msg) {
  if (is.null(msg)) return(NULL)

  spec <- switch(
    msg$level,
    ok    = list(cls = "small mb-2", icon = "check-circle-fill",
                 icls = "text-success me-1"),
    error = list(cls = "small text-warning mb-2",
                 icon = "exclamation-triangle-fill", icls = "me-1"),
    list(cls = "small text-muted mb-2", icon = "info-circle-fill",
         icls = "text-info me-1")
  )

  htmltools::div(
    class = spec$cls,
    bsicons::bs_icon(spec$icon, class = spec$icls),
    msg$text
  )
}


#' Applicability verdict for R5 on the current project
#'
#' @description
#' Best-effort: without loaded units, or with a core that does not expose the
#' accessor, we say nothing rather than guess. `NULL` means "unknown", never
#' "not applicable".
#'
#' @param app_state reactiveValues.
#' @param i18n Translator object.
#'
#' @return List from [applicabilite_message()], or `NULL`.
#'
#' @noRd
.applicabilite_msg_r5 <- function(app_state, i18n) {
  units <- app_state$current_project$indicators_sf
  if (is.null(units) || !inherits(units, "sf") || nrow(units) == 0L) return(NULL)

  bd <- tryCatch({
    pth <- file.path(app_state$current_project$path,
                     "cache", "layers", "bdforet.gpkg")
    if (file.exists(pth)) sf::st_read(pth, quiet = TRUE) else NULL
  }, error = function(e) NULL)

  v <- applicabilite_safe("r5_applicabilite", units = units, bdforet = bd)
  applicabilite_message("r5", v, i18n)
}


#' Applicability verdict for A5 on the current project
#'
#' @description
#' The cached LST raster is passed when it exists: without it the core answers
#' at the scale of the AOI - a STAC query knows bounding boxes, not pixels - and
#' `eligible_partial` becomes unreachable.
#'
#' @param app_state reactiveValues.
#' @param i18n Translator object.
#'
#' @return List from [applicabilite_message()], or `NULL`.
#'
#' @noRd
.applicabilite_msg_a5 <- function(app_state, i18n) {
  units <- app_state$current_project$indicators_sf
  if (is.null(units) || !inherits(units, "sf") || nrow(units) == 0L) return(NULL)

  lst <- tryCatch({
    dir <- file.path(app_state$current_project$path, "cache", "layers", "lst")
    tifs <- if (dir.exists(dir)) list.files(dir, "\\.tif$", full.names = TRUE)
            else character(0)
    if (length(tifs)) terra::rast(tifs[1]) else NULL
  }, error = function(e) NULL)

  buffer <- app_state$current_project$metadata$lst_urbain$buffer_m %||% 500
  v <- applicabilite_safe("a5_applicabilite", units = units, lst = lst,
                          buffer_m = buffer)
  applicabilite_message("a5", v, i18n)
}


#' Optional sources configuration UI
#'
#' @param id Character. Module namespace ID.
#'
#' @return A `div` holding the two source blocks.
#' @noRd
mod_sources_config_ui <- function(id) {
  ns <- shiny::NS(id)

  htmltools::div(
    class = "pt-3",
    shiny::uiOutput(ns("intro")),
    bslib::layout_columns(
      col_widths = c(6, 6),
      shiny::uiOutput(ns("sufosat_block")),
      shiny::uiOutput(ns("lst_block"))
    ),
    # Calibrages du Suivi sanitaire (spec 013). Ce ne sont pas des sources,
    # d'ou un bloc distinct sur toute la largeur plutot qu'une troisieme
    # colonne : le lecteur ne doit pas croire qu'il active une donnee.
    shiny::uiOutput(ns("fast_block"))
  )
}


#' Optional sources configuration server
#'
#' @param id Character. Module namespace ID.
#' @param app_state reactiveValues. Shared application state (uses
#'   `$language`, `$current_project`, `$project_id`).
#'
#' @return Invisible NULL.
#' @noRd
mod_sources_config_server <- function(id, app_state) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # La langue peut changer en cours de session : i18n en reactive, pas en
    # valeur figee a l'instanciation du module.
    i18n_r <- shiny::reactive(get_i18n(app_state$language %||% "fr"))

    # Bumpe apres un enregistrement pour re-rendre les deux blocs.
    refresh <- shiny::reactiveVal(0)

    # Le projet courant porte son id ; `app_state$project_id` sert de repli.
    .pid <- function() {
      proj <- app_state$current_project
      pid <- proj$id %||% app_state$project_id
      if (is.null(pid) || !nzchar(as.character(pid))) NULL else as.character(pid)
    }

    # Recharge le projet apres ecriture des metadonnees, pour que le reste de
    # l'app (calcul, radar) voie la nouvelle configuration sans rouvrir.
    .refresh_project <- function(pid) {
      refreshed <- tryCatch(load_project(pid), error = function(e) NULL)
      if (!is.null(refreshed)) app_state$current_project <- refreshed
      refresh(refresh() + 1)
    }

    output$intro <- shiny::renderUI({
      shiny::p(class = "text-muted small", i18n_r()$t("sources_config_intro"))
    })

    # ========================================
    # Coupes rases -> T3 (SUFOSAT, spec 030)
    # ========================================

    output$sufosat_block <- shiny::renderUI({
      i18n <- i18n_r()
      refresh()
      header <- htmltools::tags$label(
        class = "form-label fw-semibold", i18n$t("sufosat_section"))
      hint <- htmltools::tags$small(
        class = "text-muted d-block mb-2", i18n$t("sufosat_hint"))

      pid <- .pid()
      if (is.null(pid)) {
        return(htmltools::div(
          class = "mb-3 p-2 border rounded", header, hint,
          htmltools::div(class = "text-muted small fst-italic",
                         i18n$t("sources_need_project"))))
      }

      # T3 needs the SUFOSAT rasters from Theia - gate on S3 credentials.
      theia_ok <- isTRUE(tryCatch(theia_api_key_configured(),
                                  error = function(e) FALSE))
      if (!theia_ok) {
        return(htmltools::div(
          class = "mb-3 p-2 border rounded", header, hint,
          htmltools::div(
            class = "small text-warning fst-italic",
            bsicons::bs_icon("exclamation-triangle", class = "me-1"),
            i18n$t("sufosat_need_theia"))))
      }

      proj    <- app_state$current_project
      sc      <- proj$metadata$sufosat
      enabled <- project_sufosat_enabled(proj$metadata)
      status  <- if (enabled) {
        htmltools::div(
          class = "small mb-2",
          bsicons::bs_icon("check-circle-fill", class = "text-success me-1"),
          i18n$t("sufosat_active"))
      } else {
        htmltools::div(class = "small text-muted mb-2 fst-italic",
                       i18n$t("sufosat_none"))
      }

      htmltools::div(
        class = "mb-3 p-2 border rounded h-100",
        header, hint, status,
        shiny::checkboxInput(ns("sufosat_enabled"), i18n$t("sufosat_enable"),
                             value = enabled),
        shiny::sliderInput(
          ns("sufosat_window"), i18n$t("sufosat_window"),
          min = 1, max = 8, value = sc$window_years %||% 5, step = 1,
          width = "100%"),
        shiny::sliderInput(
          ns("sufosat_min_proba"), i18n$t("sufosat_min_proba"),
          min = 0.5, max = 1.0, value = sc$min_proba %||% 0.9, step = 0.05,
          width = "100%"),
        shiny::actionButton(
          ns("sufosat_save"), i18n$t("sufosat_save"),
          class = "btn-primary btn-sm", icon = bsicons::bs_icon("save"))
      )
    })

    shiny::observeEvent(input$sufosat_save, {
      i18n <- i18n_r()
      if (deny_if_readonly(app_state)) return()
      pid <- .pid()
      if (is.null(pid)) {
        shiny::showNotification(i18n$t("sources_need_project"), type = "warning")
        return()
      }
      tryCatch({
        set_project_sufosat(
          pid,
          enabled      = isTRUE(input$sufosat_enabled),
          window_years = input$sufosat_window %||% 5,
          min_proba    = input$sufosat_min_proba %||% 0.9)
        .refresh_project(pid)
        shiny::showNotification(i18n$t("sufosat_saved"), type = "message")
      }, error = function(e) {
        shiny::showNotification(paste(i18n$t("error"), conditionMessage(e)),
                                type = "error")
      })
    })

    # ========================================
    # Rafraichissement urbain -> A5 (LST, spec 032)
    # ========================================

    output$lst_block <- shiny::renderUI({
      i18n <- i18n_r()
      refresh()
      header <- htmltools::tags$label(
        class = "form-label fw-semibold", i18n$t("lst_section"))
      hint <- htmltools::tags$small(
        class = "text-muted d-block mb-2", i18n$t("lst_hint"))

      pid <- .pid()
      if (is.null(pid)) {
        return(htmltools::div(
          class = "mb-3 p-2 border rounded", header, hint,
          htmltools::div(class = "text-muted small fst-italic",
                         i18n$t("sources_need_project"))))
      }

      # A5 needs the LST raster from Theia - gate on S3 credentials.
      theia_ok <- isTRUE(tryCatch(theia_api_key_configured(),
                                  error = function(e) FALSE))
      if (!theia_ok) {
        return(htmltools::div(
          class = "mb-3 p-2 border rounded", header, hint,
          htmltools::div(
            class = "small text-warning fst-italic",
            bsicons::bs_icon("exclamation-triangle", class = "me-1"),
            i18n$t("lst_need_theia"))))
      }

      proj    <- app_state$current_project
      lc      <- proj$metadata$lst_urbain
      enabled <- project_lst_enabled(proj$metadata)

      # " Active " disait l'INTENTION, pas l'ETAT : sur un projet rural, la
      # source restait annoncee active alors qu'aucune scene Thermocity ne
      # couvre l'emprise et que A5 sortait vide. Quand un statut a ete
      # enregistre a l'acquisition, il prime - c'est lui qui sait.
      src_status <- tryCatch(
        load_source_status(get_project_path(proj$id), "theia_lst"),
        error = function(e) NULL)

      # Verdict d'applicabilite A5, complementaire du statut de source : le
      # statut dit si le catalogue repond, le verdict dit si l'indicateur a un
      # sens sur CES unites.
      a5_msg <- .applicabilite_msg_a5(app_state, i18n)
      msg <- if (enabled) source_status_message(src_status, i18n) else NULL

      status <- if (!enabled) {
        htmltools::div(class = "small text-muted mb-2 fst-italic",
                       i18n$t("lst_none"))
      } else if (is.null(msg)) {
        # Rien d'enregistre (projet jamais calcule, ou coeur sans
        # `theia_source_status`) : on garde le libelle d'avant plutot que
        # d'affirmer une couverture qu'on n'a pas verifiee.
        htmltools::div(
          class = "small mb-2",
          bsicons::bs_icon("check-circle-fill", class = "text-success me-1"),
          i18n$t("lst_active"))
      } else if (identical(msg$level, "ok")) {
        htmltools::div(
          class = "small mb-2",
          bsicons::bs_icon("check-circle-fill", class = "text-success me-1"),
          msg$text)
      } else if (identical(msg$level, "info")) {
        # Ton neutre et non alarmant : hors couverture n'est pas une panne.
        htmltools::div(
          class = "small text-muted mb-2",
          bsicons::bs_icon("info-circle-fill", class = "text-info me-1"),
          msg$text)
      } else {
        htmltools::div(
          class = "small text-warning mb-2",
          bsicons::bs_icon("exclamation-triangle-fill", class = "me-1"),
          msg$text)
      }

      htmltools::div(
        class = "mb-3 p-2 border rounded h-100",
        header, hint, status,
        .applicabilite_badge(a5_msg),
        shiny::checkboxInput(ns("lst_enabled"), i18n$t("lst_enable"),
                             value = enabled),
        shiny::sliderInput(
          ns("lst_buffer"), i18n$t("lst_buffer"),
          min = 100, max = 2000, value = lc$buffer_m %||% 500, step = 100,
          width = "100%"),
        shiny::actionButton(
          ns("lst_save"), i18n$t("lst_save"),
          class = "btn-primary btn-sm", icon = bsicons::bs_icon("save"))
      )
    })

    shiny::observeEvent(input$lst_save, {
      i18n <- i18n_r()
      if (deny_if_readonly(app_state)) return()
      pid <- .pid()
      if (is.null(pid)) {
        shiny::showNotification(i18n$t("sources_need_project"), type = "warning")
        return()
      }
      tryCatch({
        set_project_lst_urbain(
          pid,
          enabled  = isTRUE(input$lst_enabled),
          buffer_m = input$lst_buffer %||% 500)
        .refresh_project(pid)
        shiny::showNotification(i18n$t("lst_saved"), type = "message")
      }, error = function(e) {
        shiny::showNotification(paste(i18n$t("error"), conditionMessage(e)),
                                type = "error")
      })
    })

    # ========================================
    # Calibrages du Suivi sanitaire -> FAST (spec 013)
    # ========================================
    #
    # Ces quatre reglages etaient des sliders du sidebar Suivi sanitaire. Ce
    # sont des CALIBRAGES, qu'on regle une fois par massif et non a chaque
    # lancement : leur place est ici, persistes par projet. La periode
    # d'observation, elle, est restee dans le sidebar - c'est le geste courant
    # d'un diagnostic, et l'enfouir aurait rallonge la boucle d'exploration.

    output$fast_block <- shiny::renderUI({
      i18n <- i18n_r()
      refresh()
      header <- htmltools::tags$label(
        class = "form-label fw-semibold", i18n$t("fast_params_section"))
      hint <- htmltools::tags$small(
        class = "text-muted d-block mb-2", i18n$t("fast_params_hint"))

      pid <- .pid()
      if (is.null(pid)) {
        return(htmltools::div(
          class = "mt-3 p-2 border rounded", header, hint,
          htmltools::div(class = "text-muted small fst-italic",
                         i18n$t("sources_need_project"))))
      }

      fp <- project_fast_params(app_state$current_project$metadata)

      # Verdict d'applicabilite de R5, AVANT calcul : c'est ici qu'il sert.
      # Decouvrir apres coup qu'un peuplement n'est pas evaluable, c'est avoir
      # lance FORDEAD pour rien.
      r5_msg <- .applicabilite_msg_r5(app_state, i18n)

      htmltools::div(
        class = "mt-3 p-2 border rounded",
        header, hint,
        .applicabilite_badge(r5_msg),
        bslib::layout_columns(
          col_widths = c(3, 3, 3, 3),
          shiny::sliderInput(
            ns("fast_threshold_ndvi"), i18n$t("monitoring_threshold_ndvi"),
            min = 0.10, max = 0.80, value = fp$threshold_ndvi, step = 0.01,
            width = "100%"),
          shiny::sliderInput(
            ns("fast_threshold_nbr"), i18n$t("monitoring_threshold_nbr"),
            min = 0.10, max = 0.80, value = fp$threshold_nbr, step = 0.01,
            width = "100%"),
          shiny::sliderInput(
            ns("fast_threshold_ndmi"), i18n$t("monitoring_threshold_ndmi"),
            min = 0.10, max = 0.80, value = fp$threshold_ndmi, step = 0.01,
            width = "100%"),
          shiny::numericInput(
            ns("fast_window_days"), i18n$t("monitoring_window_days"),
            value = fp$window_days, min = 7L, max = 90L, step = 1L,
            width = "100%")
        ),
        shiny::actionButton(
          ns("fast_save"), i18n$t("fast_params_save"),
          class = "btn-primary btn-sm", icon = bsicons::bs_icon("save"))
      )
    })

    shiny::observeEvent(input$fast_save, {
      i18n <- i18n_r()
      if (deny_if_readonly(app_state)) return()
      pid <- .pid()
      if (is.null(pid)) {
        shiny::showNotification(i18n$t("sources_need_project"), type = "warning")
        return()
      }
      tryCatch({
        set_project_fast_params(
          pid,
          threshold_ndvi = input$fast_threshold_ndvi,
          threshold_nbr  = input$fast_threshold_nbr,
          threshold_ndmi = input$fast_threshold_ndmi,
          window_days    = input$fast_window_days)
        .refresh_project(pid)
        shiny::showNotification(i18n$t("fast_params_saved"), type = "message")
      }, error = function(e) {
        shiny::showNotification(paste(i18n$t("error"), conditionMessage(e)),
                                type = "error")
      })
    })

    invisible(NULL)
  })
}

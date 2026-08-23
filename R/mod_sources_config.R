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
#' The tab then grew four **calibration** blocks that are not sources at all -
#' Suivi sanitaire (FAST thresholds + FORDEAD anomaly threshold), Accessibilite
#' (buffer), Desserte (buffer, skidding, max slope, slope pricing) and
#' reGeneration (phenology, expert overrides, forcing, resolution). They share
#' one property: they are set once per massif and then left alone, whereas the
#' sidebars they came from are where one varies a run. Each persists on the
#' project metadata and each block owns its save button.
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
    shiny::uiOutput(ns("fast_block")),
    # Meme raison pour la zone tampon de l'Accessibilite : un calibrage
    # d'emprise, regle une fois par massif, pas une source a activer.
    shiny::uiOutput(ns("acc_block")),
    # Idem pour les calibrages de la Desserte (emprise, portee machine, pente
    # constructible, tarification de la pente).
    shiny::uiOutput(ns("desserte_block")),
    # Idem pour la reGeneration (phenologie, overrides experts, forcage,
    # resolution microclimat).
    shiny::uiOutput(ns("regen_block")),
    # Idem pour le croisement ONF : domanialite retenue, purge des parcelles
    # peu forestieres et decoupe du parcellaire sur le cadastre. Trois
    # calibrages qui vivaient dans la barre d'actions de Carte UGF, a cote du
    # bouton qui les consomme - donc regles a chaque essai au lieu d'une fois
    # par massif.
    shiny::uiOutput(ns("onf_block"))
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

      fp  <- project_fast_params(app_state$current_project$metadata)
      fdp <- project_fordead_params(app_state$current_project$metadata)

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
        # Seuil d'anomalie FORDEAD (CRSWIR) : meme nature de calibrage que les
        # trois seuils FAST ci-dessus - il etait le dernier slider du sidebar
        # Suivi sanitaire. Semantique INVERSE, d'ou sa propre ligne d'aide : un
        # pixel est en anomalie quand son CRSWIR depasse la valeur modelisee de
        # plus que ce seuil.
        htmltools::tags$small(
          class = "text-muted d-block mt-2 mb-2", i18n$t("fordead_params_hint")),
        bslib::layout_columns(
          col_widths = 3,
          shiny::sliderInput(
            ns("fordead_threshold_anomaly"), i18n$t("monitoring_threshold_anomaly"),
            min = 0.05, max = 0.50, value = fdp$threshold_anomaly, step = 0.01,
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
        # Un seul bouton pour toute la section " seuils de detection " : FAST et
        # FORDEAD se reglent d'un meme geste.
        set_project_fordead_params(
          pid, threshold_anomaly = input$fordead_threshold_anomaly)
        .refresh_project(pid)
        shiny::showNotification(i18n$t("fast_params_saved"), type = "message")
      }, error = function(e) {
        shiny::showNotification(paste(i18n$t("error"), conditionMessage(e)),
                                type = "error")
      })
    })

    # ========================================
    # Accessibilite - zone tampon d'emprise
    # ========================================
    #
    # La zone tampon etait un `numericInput` du sidebar droit de la Carte
    # d'accessibilite. Elle dimensionne l'emprise ACQUISE (MNT + desserte), donc
    # le cout et le cache du calcul : c'est un calibrage de massif, regle une
    # fois, pas un reglage d'affichage. Sa place est ici, persistee par projet.

    output$acc_block <- shiny::renderUI({
      i18n <- i18n_r()
      refresh()
      header <- htmltools::tags$label(
        class = "form-label fw-semibold", i18n$t("acc_params_section"))
      hint <- htmltools::tags$small(
        class = "text-muted d-block mb-2", i18n$t("acc_buffer_help"))

      pid <- .pid()
      if (is.null(pid)) {
        return(htmltools::div(
          class = "mt-3 p-2 border rounded", header, hint,
          htmltools::div(class = "text-muted small fst-italic",
                         i18n$t("sources_need_project"))))
      }

      ap <- project_accessibility_params(app_state$current_project$metadata)

      htmltools::div(
        class = "mt-3 p-2 border rounded",
        header, hint,
        bslib::layout_columns(
          col_widths = 3,
          shiny::numericInput(
            ns("acc_buffer_m"), i18n$t("acc_buffer"),
            value = ap$buffer_m, min = 0, max = 20000, step = 50,
            width = "100%")
        ),
        shiny::actionButton(
          ns("acc_save"), i18n$t("acc_params_save"),
          class = "btn-primary btn-sm", icon = bsicons::bs_icon("save"))
      )
    })

    shiny::observeEvent(input$acc_save, {
      i18n <- i18n_r()
      if (deny_if_readonly(app_state)) return()
      pid <- .pid()
      if (is.null(pid)) {
        shiny::showNotification(i18n$t("sources_need_project"), type = "warning")
        return()
      }
      tryCatch({
        set_project_accessibility_params(pid, buffer_m = input$acc_buffer_m)
        .refresh_project(pid)
        shiny::showNotification(i18n$t("acc_params_saved"), type = "message")
      }, error = function(e) {
        shiny::showNotification(paste(i18n$t("error"), conditionMessage(e)),
                                type = "error")
      })
    })

    # ========================================
    # Desserte - calibrages d'emprise et de cout
    # ========================================
    #
    # Ces cinq reglages etaient des inputs du sidebar gauche de la Carte de
    # desserte. Ils definissent CE QU'ON ACQUIERT (tampon), CE QU'ON CONSIDERE
    # desservi (distance de debardage), JUSQU'OU l'on construit (pente max) et
    # COMMENT on chiffre la pente (bareme / terrassement + largeur) : autant de
    # decisions de massif, prises une fois. Seul le choix du moteur
    # (glouton / Steiner) reste dans le sidebar - c'est lui qu'on fait varier
    # d'un essai a l'autre.
    #
    # `methode_pente` et `pente_max_pct` restent DEUX entrees distinctes, et ce
    # n'est pas un detail de presentation : avant que le coeur ne les separe,
    # choisir " terrassement " deplacait aussi le plafond de pente en silence,
    # de 60 % a 100 %, ouvrant 5 % du massif. Le terrassement est desormais le
    # defaut (il chiffre un volume de deblai/remblai, donc il tient compte de la
    # largeur de plateforme que le bareme ignore) - raison de plus pour que le
    # plafond se decide a part.

    output$desserte_block <- shiny::renderUI({
      i18n <- i18n_r()
      refresh()
      header <- htmltools::tags$label(
        class = "form-label fw-semibold", i18n$t("dess_params_section"))
      hint <- htmltools::tags$small(
        class = "text-muted d-block mb-2", i18n$t("dess_params_hint"))

      pid <- .pid()
      if (is.null(pid)) {
        return(htmltools::div(
          class = "mt-3 p-2 border rounded", header, hint,
          htmltools::div(class = "text-muted small fst-italic",
                         i18n$t("sources_need_project"))))
      }

      dp <- project_desserte_params(app_state$current_project$metadata)

      htmltools::div(
        class = "mt-3 p-2 border rounded",
        header, hint,
        bslib::layout_columns(
          col_widths = c(4, 4, 4),
          # Les explications de chaque reglage suivent le champ, portees par le
          # meme " i " que dans le sidebar d'origine - on deplace le widget, pas
          # son mode d'emploi.
          shiny::numericInput(
            ns("dess_buffer_km"),
            htmltools::tagList(i18n$t("dess_buffer"),
                               info_popover_in_label(i18n$t("dess_buffer_help"))),
            value = dp$buffer_km, min = 0, max = 20, step = 1, width = "100%"),
          shiny::numericInput(
            ns("dess_skidding_m"),
            htmltools::tagList(i18n$t("dess_skidding"),
                               info_popover_in_label(i18n$t("dess_skidding_help"))),
            value = dp$skidding_m, min = 0, max = 2000, step = 50,
            width = "100%"),
          shiny::numericInput(
            ns("dess_pente_max_pct"),
            htmltools::tagList(i18n$t("dess_pente_max"),
                               info_popover_in_label(i18n$t("dess_pente_max_help"))),
            value = dp$pente_max_pct, min = 0, max = 100, step = 5,
            width = "100%")
        ),
        htmltools::tags$small(
          class = "text-muted d-block mb-2", i18n$t("dess_methode_pente_help")),
        bslib::layout_columns(
          col_widths = c(8, 4),
          shiny::radioButtons(
            ns("dess_methode_pente_cfg"), i18n$t("dess_methode_pente"),
            choices = stats::setNames(
              c("bareme", "terrassement"),
              c(i18n$t("dess_methode_bareme"),
                i18n$t("dess_methode_terrassement"))),
            selected = dp$methode_pente, inline = TRUE, width = "100%"),
          # La largeur n'a d'effet QU'EN terrassement (le bareme y est aveugle) :
          # masquee autrement, comme dans le sidebar d'origine.
          shiny::conditionalPanel(
            condition = sprintf("input['%s'] == 'terrassement'",
                                ns("dess_methode_pente_cfg")),
            shiny::numericInput(
              ns("dess_largeur_m"),
              htmltools::tagList(i18n$t("dess_largeur"),
                                 info_popover_in_label(i18n$t("dess_largeur_help"))),
              value = dp$largeur_m, min = 2.5, max = 6, step = 0.5,
              width = "100%"))
        ),
        shiny::actionButton(
          ns("desserte_save"), i18n$t("dess_params_save"),
          class = "btn-primary btn-sm", icon = bsicons::bs_icon("save"))
      )
    })

    shiny::observeEvent(input$desserte_save, {
      i18n <- i18n_r()
      if (deny_if_readonly(app_state)) return()
      pid <- .pid()
      if (is.null(pid)) {
        shiny::showNotification(i18n$t("sources_need_project"), type = "warning")
        return()
      }
      tryCatch({
        set_project_desserte_params(
          pid,
          buffer_km     = input$dess_buffer_km,
          skidding_m    = input$dess_skidding_m,
          pente_max_pct = input$dess_pente_max_pct,
          methode_pente = input$dess_methode_pente_cfg,
          largeur_m     = input$dess_largeur_m)
        .refresh_project(pid)
        shiny::showNotification(i18n$t("dess_params_saved"), type = "message")
      }, error = function(e) {
        shiny::showNotification(paste(i18n$t("error"), conditionMessage(e)),
                                type = "error")
      })
    })

    # ========================================
    # Parcellaire ONF - domanialite, purge, decoupe
    # ========================================

    output$onf_block <- shiny::renderUI({
      i18n <- i18n_r()
      refresh()
      header <- htmltools::tags$label(
        class = "form-label fw-semibold", i18n$t("onf_params_section"))
      hint <- htmltools::tags$small(
        class = "text-muted d-block mb-2", i18n$t("onf_params_hint"))

      pid <- .pid()
      if (is.null(pid)) {
        return(htmltools::div(class = "mt-3", header, hint,
                              htmltools::tags$em(class = "text-muted small",
                                                 i18n$t("sources_need_project"))))
      }
      cfg <- project_onf_params(app_state$current_project$metadata)

      htmltools::div(
        class = "mt-3",
        header, hint,
        bslib::layout_columns(
          col_widths = c(4, 4, 4),
          shiny::checkboxGroupInput(
            ns("onf_domanialite_cfg"),
            label = i18n$t("onf_domanialite"),
            choices = stats::setNames(
              c("domaniale", "autre"),
              c(i18n$t("onf_domanialite_domaniale"),
                i18n$t("onf_domanialite_autre"))),
            selected = cfg$domanialite),
          htmltools::div(
            shiny::checkboxInput(ns("onf_purge_cfg"),
                                 i18n$t("onf_purge_hors"),
                                 value = isTRUE(cfg$purger)),
            info_popover_in_label(i18n$t("onf_purge_hors_tip")),
            shiny::numericInput(
              ns("onf_seuil_cfg"),
              label = htmltools::tagList(
                i18n$t("onf_seuil_foret"),
                info_popover_in_label(i18n$t("onf_seuil_foret_tip"))),
              value = round(100 * cfg$seuil_foret), min = 0, max = 100, step = 1)),
          htmltools::div(
            shiny::checkboxInput(ns("onf_clip_cfg"),
                                 i18n$t("onf_clip_cadastre"),
                                 value = isTRUE(cfg$clip_cadastre)),
            info_popover_in_label(i18n$t("onf_clip_cadastre_tip")))
        ),
        shiny::actionButton(ns("onf_save"), i18n$t("onf_params_save"),
                            class = "btn-primary btn-sm")
      )
    })

    shiny::observeEvent(input$onf_save, {
      i18n <- i18n_r()
      if (deny_if_readonly(app_state)) return()
      pid <- .pid()
      if (is.null(pid)) {
        shiny::showNotification(i18n$t("sources_need_project"), type = "warning")
        return()
      }
      tryCatch({
        # Le seuil se saisit en POUR CENT et se range en part : l'utilisateur
        # pense « 10 % », le coeur compare des parts.
        seuil <- suppressWarnings(as.numeric(input$onf_seuil_cfg))
        if (length(seuil) != 1L || is.na(seuil)) seuil <- 0
        set_project_onf_params(
          pid,
          domanialite   = input$onf_domanialite_cfg,
          purger        = isTRUE(input$onf_purge_cfg),
          seuil_foret   = max(0, min(1, seuil / 100)),
          clip_cadastre = isTRUE(input$onf_clip_cfg))
        .refresh_project(pid)
        shiny::showNotification(i18n$t("onf_params_saved"), type = "message")
      }, error = function(e) {
        shiny::showNotification(paste(i18n$t("error"), conditionMessage(e)),
                                type = "error")
      })
    })

    # ========================================
    # reGeneration - phenologie, experts, forcage
    # ========================================
    #
    # Le debourrement et la chute des feuilles decrivent le cycle foliaire du
    # massif ; `lai_max` et `ewm` sont des OVERRIDES (vides = derives de la
    # donnee) ; le forcage et la resolution decident du jeu meteo et de la
    # finesse du microclimat. Rien la-dedans ne se regle d'un run a l'autre :
    # dans le sidebar, `lai_max` et `ewm` invitaient meme au remplissage
    # reflexe - saisir `lai_max` annule un PAI LiDAR calcule en 57 min sans
    # aucun signal. Ils sont donc ici, persistes par projet.

    output$regen_block <- shiny::renderUI({
      i18n <- i18n_r()
      refresh()
      header <- htmltools::tags$label(
        class = "form-label fw-semibold", i18n$t("regen_params_section"))
      hint <- htmltools::tagList(
        htmltools::tags$small(
          class = "text-muted d-block", i18n$t("regen_params_hint")),
        # La semantique " vide = derive " des deux overrides est portee a part :
        # c'est la seule phrase du bloc dont l'oubli change un resultat.
        htmltools::tags$small(
          class = "text-muted d-block mb-2 fst-italic",
          i18n$t("regen_expert_hint")))

      pid <- .pid()
      if (is.null(pid)) {
        return(htmltools::div(
          class = "mt-3 p-2 border rounded", header, hint,
          htmltools::div(class = "text-muted small fst-italic",
                         i18n$t("sources_need_project"))))
      }

      rp <- project_regen_params(app_state$current_project$metadata)

      htmltools::div(
        class = "mt-3 p-2 border rounded",
        header, hint,
        bslib::layout_columns(
          col_widths = c(3, 3, 3, 3),
          shiny::numericInput(
            ns("regen_budburst"), i18n$t("regen_budburst"),
            value = rp$budburst, min = 1, max = 200, width = "100%"),
          shiny::numericInput(
            ns("regen_leaf_fall"), i18n$t("regen_leaf_fall"),
            value = rp$leaf_fall, min = 200, max = 366, width = "100%"),
          shiny::numericInput(
            ns("regen_lai_max"),
            htmltools::tagList(i18n$t("regen_lai_max"),
                               info_popover_in_label(i18n$t("regen_lai_tip"))),
            value = rp$lai_max, min = 0, max = 12, step = 0.1, width = "100%"),
          shiny::numericInput(
            ns("regen_ewm"),
            htmltools::tagList(i18n$t("regen_ewm"),
                               info_popover_in_label(i18n$t("regen_ewm_hint"))),
            value = rp$ewm, min = 10, max = 400, step = 5, width = "100%")
        ),
        bslib::layout_columns(
          col_widths = c(3, 4, 5),
          shiny::numericInput(
            ns("regen_rooting_depth_cm"),
            htmltools::tagList(i18n$t("regen_rooting_depth"),
                               info_popover_in_label(
                                 i18n$t("regen_rooting_depth_hint"))),
            value = rp$rooting_depth_cm, min = 20, max = 200, step = 10,
            width = "100%"),
          shiny::radioButtons(
            ns("regen_forcing"),
            htmltools::tagList(i18n$t("regen_forcing"),
                               info_popover_in_label(i18n$t("regen_forcing_tip"))),
            choices = stats::setNames(
              c("safran", "era5"),
              c(i18n$t("regen_forcing_safran"), i18n$t("regen_forcing_era5"))),
            selected = rp$forcing, inline = TRUE, width = "100%"),
          shiny::radioButtons(
            ns("regen_resolution"), i18n$t("regen_resolution"),
            choices = stats::setNames(
              c("2", "5"), c(i18n$t("regen_res_2m"), i18n$t("regen_res_5m"))),
            selected = rp$resolution, inline = TRUE, width = "100%")
        ),
        shiny::actionButton(
          ns("regen_save"), i18n$t("regen_params_save"),
          class = "btn-primary btn-sm", icon = bsicons::bs_icon("save"))
      )
    })

    shiny::observeEvent(input$regen_save, {
      i18n <- i18n_r()
      if (deny_if_readonly(app_state)) return()
      pid <- .pid()
      if (is.null(pid)) {
        shiny::showNotification(i18n$t("sources_need_project"), type = "warning")
        return()
      }
      tryCatch({
        set_project_regen_params(
          pid,
          budburst         = input$regen_budburst,
          leaf_fall        = input$regen_leaf_fall,
          # Un champ vide reste vide : c'est le signal " derive de la donnee ",
          # pas une valeur manquante a remplacer par un defaut.
          lai_max          = input$regen_lai_max,
          ewm              = input$regen_ewm,
          rooting_depth_cm = input$regen_rooting_depth_cm,
          forcing          = input$regen_forcing,
          resolution       = input$regen_resolution)
        .refresh_project(pid)
        shiny::showNotification(i18n$t("regen_params_saved"), type = "message")
      }, error = function(e) {
        shiny::showNotification(paste(i18n$t("error"), conditionMessage(e)),
                                type = "error")
      })
    })

    invisible(NULL)
  })
}

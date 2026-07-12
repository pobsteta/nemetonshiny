# reGénération — module Shiny (spec 027, L4)
#
# Onglet de lecture de vulnérabilité climatique : exposition microclimatique ×
# stress hydrique du sol, pour prioriser les interventions de régénération.
# Pattern golem : mod_regeneration_ui(id) + mod_regeneration_server(id, app_state).
# Aucune logique métier ici — tout passe par service_regeneration (→ nemeton).

# `.fmt_elapsed()` (chrono MM:SS des boutons/notifs async) est désormais partagé
# dans R/utils_notif.R avec le rendu unifié `.running_notif_content()`.

# --- Phase en cours du moteur reGénération (canal fichier, spec 027) ---------
# Le worker future écrit sa phase dans cache/regeneration/engine_status.json ;
# la session principale le poll (invalidateLater 1000) et rend la phase dans la
# notif bas-droite. Cf. brief engine-phase-status.

# Lit engine_status.json ; NULL si absent/illisible/périmé (> 2 min sans MAJ).
.regen_read_phase <- function(project_path) {
  if (is.null(project_path)) return(NULL)
  f <- file.path(project_path, "cache", "regeneration", "engine_status.json")
  if (!file.exists(f)) return(NULL)
  st <- tryCatch(jsonlite::fromJSON(f), error = function(e) NULL)
  if (is.null(st) || is.null(st$phase)) return(NULL)
  if (!is.null(st$ts) && as.integer(Sys.time()) - st$ts > 120L) return(NULL)
  st
}

# « base ({i}/{n}) » seulement si l'événement a fourni l'avancement.
.regen_step_lbl <- function(i18n, key, st) {
  base <- i18n$t(key)
  if (!is.null(st$i) && !is.null(st$n))
    sprintf("%s (%d/%d)", base, as.integer(st$i), as.integer(st$n))
  else base
}

# « base {year} ({i}/{n}) » seulement si l'événement era5 a fourni l'année.
.regen_micro_lbl <- function(i18n, key, st) {
  base <- i18n$t(key)
  if (!is.null(st$year) && !is.null(st$i) && !is.null(st$n))
    sprintf("%s %s (%d/%d)", base, st$year, as.integer(st$i), as.integer(st$n))
  else base
}

# Lit engine.log (JSONL, écrit en ajout par le worker — spec 035 B3.b). Renvoie
# un data.frame (ts, level, source, message), vide si absent/illisible. Contrairement
# à engine_status.json, ce fichier SURVIT à la mort du worker (OOM) : c'est le seul
# moyen de récupérer les diagnostics quand engine_task$result() est inaccessible.
.regen_read_log <- function(project_path) {
  empty <- data.frame(ts = integer(0), level = character(0),
                      source = character(0), message = character(0),
                      stringsAsFactors = FALSE)
  if (is.null(project_path)) return(empty)
  f <- file.path(project_path, "cache", "regeneration", "engine.log")
  if (!file.exists(f)) return(empty)
  lines <- tryCatch(readLines(f, warn = FALSE), error = function(e) character(0))
  lines <- lines[nzchar(trimws(lines))]
  if (!length(lines)) return(empty)
  rows <- lapply(lines, function(l) tryCatch(jsonlite::fromJSON(l), error = function(e) NULL))
  rows <- Filter(Negate(is.null), rows)
  if (!length(rows)) return(empty)
  do.call(rbind, lapply(rows, function(r) data.frame(
    ts = as.integer(r$ts %||% NA_integer_),
    level = as.character(r$level %||% "info"),
    source = as.character(r$source %||% ""),
    message = as.character(r$message %||% ""),
    stringsAsFactors = FALSE)))
}

# Relaie le journal du worker vers la console du processus PRINCIPAL (B3.c) —
# les cli_warn du worker `multisession` n'y arrivent jamais d'eux-mêmes.
.regen_relay_log <- function(log) {
  if (!is.data.frame(log) || !nrow(log)) return(invisible(NULL))
  for (i in seq_len(nrow(log))) {
    msg <- sprintf("[%s] %s", log$source[i], log$message[i])
    switch(log$level[i],
      "error"   = cli::cli_alert_danger(msg),
      "warning" = cli::cli_alert_warning(msg),
      invisible(NULL))   # `info` : jalons, pas de bruit console
  }
  invisible(NULL)
}

# Médiane + étendue d'un vecteur dérivé par UGF (spec 035 B4.a). NULL si le
# vecteur est scalaire ou absent : il n'y a alors rien de spatialisé à montrer.
.regen_derived_stats <- function(x, digits = 1) {
  x <- suppressWarnings(as.numeric(x))
  x <- x[is.finite(x)]
  if (length(x) < 2L) return(NULL)
  fmt <- function(v) format(round(v, digits), trim = TRUE, nsmall = digits)
  list(median = fmt(stats::median(x)), min = fmt(min(x)), max = fmt(max(x)), n = length(x))
}

# Entrées actionnables du journal (erreurs + avertissements), formatées « source: message ».
.regen_log_issues <- function(log) {
  if (!is.data.frame(log) || !nrow(log)) return(character(0))
  sel <- log$level %in% c("error", "warning")
  if (!any(sel)) return(character(0))
  unique(sprintf("%s: %s", log$source[sel], log$message[sel]))
}

# Supprime engine_status.json en fin de tâche (success/error). Le worker écrit
# `done` mais ne supprime PAS (course avec le poll) : c'est le module qui nettoie.
# NE TOUCHE PAS à engine.log : il doit survivre au run pour le post-mortem (B3.a).
.regen_cleanup_status <- function(app_state) {
  project_path <- tryCatch(app_state$current_project$path, error = function(e) NULL)
  if (!is.null(project_path))
    unlink(file.path(project_path, "cache", "regeneration", "engine_status.json"))
}

# Libellé i18n d'une phase (voir modèle 6 phases + états terminaux du brief).
.regen_phase_label <- function(i18n, st) {
  switch(st$phase %||% "",
    "grille"     = i18n$t("regen_phase_grille"),
    "pai"        = sprintf(i18n$t("regen_phase_pai"),
                           i18n$t(switch(st$source %||% "",
                             "cache" = "regen_phase_pai_cache",   # hit disque : phase éclair
                             "lidar" = "regen_phase_pai_lidar",   # dérivé du nuage COPC : long
                             "regen_phase_pai_raster"))),         # repli satellite S2/PROSAIL
    "microclimf_moyenne"  = .regen_micro_lbl(i18n, "regen_phase_micro_moy", st),
    "microclimf_canicule" = .regen_micro_lbl(i18n, "regen_phase_micro_can", st),
    "exposition" = i18n$t("regen_phase_exposition"),
    # Réserve utile SoilGrids : « (i/n) » compte les horizons de profondeur.
    "ewm"        = .regen_step_lbl(i18n, "regen_phase_ewm", st),
    "biljou"     = i18n$t("regen_phase_biljou"),
    "microclimf_skipped" = sprintf(i18n$t("regen_phase_micro_skip"),
                                   st$reason %||% ""),
    "done"       = "",
    "")
}

#' reGénération tab UI
#' @param id Module id.
#' @noRd
mod_regeneration_ui <- function(id) {
  ns <- shiny::NS(id)
  opts <- get_app_options()
  i18n <- get_i18n(opts$language %||% "fr")

  # Label + info tooltip (même pattern que mod_sampling / mod_synthesis).
  label_tt <- function(label, tooltip) {
    htmltools::tagList(label, " ",
      bslib::tooltip(bsicons::bs_icon("info-circle", class = "text-muted ms-1"),
                     tooltip, placement = "right"))
  }

  # Variante pour les entrées du radio « Couche affichée » : le tooltip décrit la
  # variable puis rappelle la lecture de l'échelle de couleurs, commune aux 4
  # couches (dégradé continu borné aux min/max des UG affichées).
  layer_tt <- function(label, tooltip) {
    htmltools::tagList(label, " ",
      bslib::tooltip(bsicons::bs_icon("info-circle", class = "text-muted ms-1"),
                     htmltools::tagList(
                       htmltools::tags$div(tooltip),
                       htmltools::tags$div(class = "mt-1 fst-italic",
                                           i18n$t("regen_map_legend_scale"))),
                     placement = "left"))
  }

  bslib::layout_sidebar(
    fillable = TRUE,
    sidebar = bslib::sidebar(
      id = ns("sidebar"), width = 360, open = TRUE,

      htmltools::tags$p(class = "text-muted small", i18n$t("regen_intro")),

      # --- Années de référence — placées en tête -------------------------
      # Le bloc année moyenne / caniculaire + le bouton Auto (E-OBS) + l'indice
      # E-OBS précèdent le moteur : l'utilisateur choisit d'abord les millésimes
      # de référence, puis lance le moteur microclimf qui les consomme.
      htmltools::tags$strong(i18n$t("regen_years_section")),
      shiny::fluidRow(
        shiny::column(6, shiny::numericInput(ns("year_moyenne"),
          i18n$t("regen_year_moyenne"), value = 2018, min = 1990, max = 2100, step = 1)),
        shiny::column(6, shiny::numericInput(ns("year_canicule"),
          i18n$t("regen_year_canicule"), value = 2022, min = 1990, max = 2100, step = 1))
      ),
      bslib::tooltip(
        bslib::input_task_button(ns("auto_years"), i18n$t("regen_year_auto"),
          icon = bsicons::bs_icon("magic"),
          label_busy = i18n$t("regen_auto_running_short"),
          type = "outline-secondary", class = "btn-sm mb-2"),
        i18n$t("regen_year_auto_tip"), placement = "right"),
      shiny::uiOutput(ns("eobs_status")),
      shiny::uiOutput(ns("eobs_index_display")),
      htmltools::tags$hr(class = "my-2"),

      # --- Moteur microclimf réel (opt-in, coûteux) ----------------------
      # Lance le vrai run microclimf (LiDAR HD + ERA5) via nemeton, en async.
      # Désactivé tant que les prérequis (grille LiDAR HD + identifiants CDS)
      # ne sont pas réunis ; le statut est affiché sous le bouton.
      htmltools::tags$small(class = "text-muted d-block mb-1", i18n$t("regen_engine_section")),
      bslib::tooltip(
        bslib::input_task_button(ns("run_engine"), i18n$t("regen_engine_run"),
          icon = bsicons::bs_icon("cpu"),
          label_busy = i18n$t("regen_engine_running_short"),
          type = "outline-primary", class = "btn-sm w-100"),
        i18n$t("regen_engine_tip"), placement = "right"),
      shiny::uiOutput(ns("engine_status")),
      # Journal du moteur (spec 035 B3.e) : replié par défaut, alimenté par
      # engine.log. Seul support durable des diagnostics du worker.
      shiny::uiOutput(ns("engine_log")),
      htmltools::tags$hr(class = "my-2"),

      # --- Moteur « risque de gel tardif » (R7, meteoland) ---------------
      # Opt-in, coûteux (interpolation Tmin journalière SAFRAN→MNT) → worker +
      # cache. Grisé si meteoland (Suggests) est absent, avec explication.
      htmltools::tags$small(class = "text-muted d-block mb-1", i18n$t("regen_frost_section")),
      if (regen_meteoland_available()) {
        bslib::tooltip(
          bslib::input_task_button(ns("run_frost"), i18n$t("regen_frost_run"),
            icon = bsicons::bs_icon("snow"),
            label_busy = i18n$t("regen_frost_running_short"),
            type = "outline-primary", class = "btn-sm w-100"),
          i18n$t("regen_frost_tip"), placement = "right")
      } else {
        htmltools::tagList(
          htmltools::tagAppendAttributes(
            bslib::input_task_button(ns("run_frost"), i18n$t("regen_frost_run"),
              icon = bsicons::bs_icon("snow"),
              type = "outline-secondary", class = "btn-sm w-100"),
            disabled = NA),
          htmltools::tags$small(class = "text-muted d-block mt-1",
            i18n$t("regen_frost_unavailable")))
      },
      htmltools::tags$hr(class = "my-2"),

      # --- Peuplement ----------------------------------------------------
      htmltools::tags$strong(i18n$t("regen_stand_section")),
      shiny::radioButtons(ns("forest_type"),
        label_tt(i18n$t("regen_forest_type"), i18n$t("regen_forest_type_tip")),
        choices = stats::setNames(c("feuillu", "resineux"),
          c(i18n$t("regen_forest_feuillu"), i18n$t("regen_forest_resineux"))),
        selected = "feuillu", inline = TRUE),
      # Débourrement / chute des feuilles = phénologie des feuillus caducs. Un
      # résineux sempervirent n'a pas de cycle foliaire saisonnier (BILJOU mode
      # coniferous, LAI permanent) : on masque ces deux champs hors « Feuillu ».
      shiny::conditionalPanel(
        condition = "input.forest_type == 'feuillu'", ns = ns,
        shiny::fluidRow(
          shiny::column(6, shiny::numericInput(ns("budburst"),
            i18n$t("regen_budburst"), value = 105, min = 1, max = 200)),
          shiny::column(6, shiny::numericInput(ns("leaf_fall"),
            i18n$t("regen_leaf_fall"), value = 300, min = 200, max = 366))
        )
      ),
      # --- Paramètres experts (spec 035 B4.b) ----------------------------
      # `lai_max` et `ewm` partagent la même sémantique : vide = dérivé de la
      # donnée (PAI LiDAR / SoilGrids), rempli = forcé. Isolés dans la sidebar,
      # ils invitaient au remplissage réflexe — saisir `lai_max` annule le
      # bénéfice d'un PAI calculé en 57 min, sans aucun signal. Repliés par défaut,
      # avec une seule phrase portant la sémantique commune.
      # `rooting_depth_cm` n'est pas un override : c'est la profondeur sur laquelle
      # SoilGrids intègre la réserve utile. Il vit ici parce qu'il n'a de sens que
      # pour qui touche à ces réglages, mais il garde une valeur par défaut.
      bslib::accordion(
        open = FALSE, class = "mb-2",
        bslib::accordion_panel(
          title = i18n$t("regen_expert_section"), icon = bsicons::bs_icon("sliders"),
          htmltools::tags$p(class = "text-muted small", i18n$t("regen_expert_hint")),
          shiny::numericInput(ns("lai_max"), label_tt(i18n$t("regen_lai_max"),
              i18n$t("regen_lai_tip")),
            value = NA, min = 0, max = 12, step = 0.1),
          shiny::numericInput(ns("ewm"), label_tt(i18n$t("regen_ewm"),
              i18n$t("regen_ewm_hint")),
            value = NA, min = 10, max = 400, step = 5),
          shiny::numericInput(ns("rooting_depth_cm"),
            label_tt(i18n$t("regen_rooting_depth"), i18n$t("regen_rooting_depth_hint")),
            value = 100, min = 20, max = 200, step = 10)
        )
      ),

      # --- Forçage / résolution -----------------------------------------
      # L'« Essence cible » vit désormais à droite de la carte (sous « Couche
      # affichée ») car elle met à jour la choroplèthe en direct ; le « Buffer
      # contexte régional » vit dans l'onglet carte « Contexte régional (E-OBS) ».
      shiny::radioButtons(ns("forcing"), i18n$t("regen_forcing"),
        choices = stats::setNames(c("safran", "era5"),
          c(i18n$t("regen_forcing_safran"), i18n$t("regen_forcing_era5"))),
        selected = "safran", inline = TRUE),
      shiny::radioButtons(ns("resolution"), i18n$t("regen_resolution"),
        choices = stats::setNames(c("2", "5"),
          c(i18n$t("regen_res_2m"), i18n$t("regen_res_5m"))),
        selected = "2", inline = TRUE),

      shiny::checkboxInput(ns("hydric_only"), i18n$t("regen_run_hydric_only"),
        value = FALSE),
      shiny::actionButton(ns("run"), i18n$t("regen_run"),
        class = "btn-primary w-100", icon = bsicons::bs_icon("play-fill")),

      # Provenance de la donnée canopée effectivement utilisée (spec 033 D5) :
      # LiDAR HD (PAI structural) ou repli satellite S2/PROSAIL (NDP 0). Lu
      # depuis nemeton::detect_ndp() ; ne s'affiche que si une canopée est utilisée.
      shiny::uiOutput(ns("canopy_provenance")),
      # Valeurs réellement utilisées par le moteur (spec 035 B4.a) : médiane +
      # étendue du lai_max et de la réserve utile par UGF, ou badge « forcé ».
      shiny::uiOutput(ns("derived_stats")),

      # --- Export / persistance (sous le bouton Lancer) ------------------
      htmltools::tags$hr(class = "my-2"),
      htmltools::tags$small(class = "text-muted d-block mb-1", i18n$t("regen_results_section")),
      shiny::downloadButton(ns("export_gpkg"), i18n$t("regen_export_gpkg"),
        class = "btn-outline-primary btn-sm w-100 mb-2"),
      shiny::actionButton(ns("persist_db"), i18n$t("regen_persist_db"),
        class = "btn-outline-secondary btn-sm w-100", icon = bsicons::bs_icon("database"))
    ),

    # --- Résultats -------------------------------------------------------
    shiny::uiOutput(ns("status")),
    bslib::navset_card_tab(
      bslib::nav_panel(
        i18n$t("regen_tab_map"),
        # Carte + sidebar DROITE dédiée (parité panneau droit des cartes FAST) :
        # le radio « Couche affichée » vit à droite de la carte ; les fonds
        # OSM/Satellite/UGF restent dans le contrôle natif Leaflet (coin carte).
        bslib::layout_sidebar(
          fillable = TRUE,
          sidebar = bslib::sidebar(
            position = "right", open = "always", width = 260,
            htmltools::tags$strong(i18n$t("regen_map_layer")),
            # Chaque couche porte un « i » qui explique la variable cartographiée
            # et comment lire sa légende. Tooltip à gauche : le sidebar est collé
            # au bord droit de la fenêtre.
            shiny::radioButtons(ns("map_layer"), NULL,
              choiceValues = c("indice_priorite_regen", "sensibilite", "njstress",
                               "d_tmax", "r7_gel_days"),
              choiceNames = list(
                layer_tt(i18n$t("regen_map_priorite"), i18n$t("regen_map_priorite_info")),
                layer_tt(i18n$t("regen_map_sensibilite"), i18n$t("regen_map_sensibilite_info")),
                layer_tt(i18n$t("regen_map_njstress"), i18n$t("regen_map_njstress_info")),
                layer_tt(i18n$t("regen_map_dtmax"), i18n$t("regen_map_dtmax_info")),
                layer_tt(i18n$t("regen_map_gel"), i18n$t("regen_map_gel_info"))),
              selected = "indice_priorite_regen"),
            # Essence cible : re-priorise la choroplèthe en direct (sans relancer
            # l'analyse). N'affecte QUE la couche « Indice de priorité » — donc
            # masquée pour les autres couches (conditionalPanel), où elle n'aurait
            # aucun effet visible.
            shiny::conditionalPanel(
              condition = "input.map_layer == 'indice_priorite_regen'", ns = ns,
              htmltools::tags$hr(class = "my-2"),
              shiny::selectInput(ns("species"), label_tt(i18n$t("regen_species_target"),
                  i18n$t("regen_species_tip")),
                choices = stats::setNames("", i18n$t("regen_species_generic"))))
          ),
          leaflet::leafletOutput(ns("map"), height = "70vh")
        )
      ),
      bslib::nav_panel(i18n$t("regen_map_context"),
        # La carte est bivariée : le cœur exige les DEUX séries E-OBS (tx + rr).
        # Seule `tx` est rapatriée par « Auto (E-OBS) », d'où une carte vide et
        # muette jusqu'ici. Le bandeau dit ce qui manque et propose de l'acquérir.
        # Sidebar DROITE dédiée (parité carte principale) : le rayon du contexte
        # régional ne concerne que cette carte, d'où sa place ici et non dans la
        # sidebar de configuration du moteur.
        bslib::layout_sidebar(
          fillable = TRUE,
          sidebar = bslib::sidebar(
            position = "right", open = "always", width = 260,
            htmltools::tags$strong(i18n$t("regen_buffer")),
            shiny::numericInput(ns("buffer_km"), NULL,
              value = 25, min = 5, max = 100, step = 5),
            htmltools::tags$strong(i18n$t("regen_context_opacity")),
            shiny::sliderInput(ns("context_opacity"), NULL,
              min = 0, max = 1, value = 0.8, step = 0.05, ticks = FALSE)
          ),
          shiny::uiOutput(ns("context_status")),
          leaflet::leafletOutput(ns("context_map"), height = "70vh")
        )),
      bslib::nav_panel(i18n$t("regen_table_section"),
        shiny::checkboxInput(ns("filter_coverage"), i18n$t("regen_filter_coverage"),
          value = TRUE),
        DT::dataTableOutput(ns("table"))),
      bslib::nav_panel(i18n$t("regen_parcel_sheet"),
        shiny::uiOutput(ns("parcel_sheet")))
    )
  )
}

#' reGénération tab server
#' @param id Module id.
#' @param app_state reactiveValues carrying `current_project` (whose
#'   `indicators_sf` holds the UGF units).
#' @noRd
mod_regeneration_server <- function(id, app_state) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    i18n <- get_i18n(get_app_options()$language %||% "fr")

    # Verrou projet (serveur multi-utilisateurs) : chaque action mutante est
    # gardée par `deny_if_readonly(app_state, i18n)` (helper partagé, R/service_lock.R)
    # qui refuse l'action et prévient l'utilisateur quand le projet est ouvert en
    # lecture seule (verrou d'autrui, ou anonyme).

    rv <- shiny::reactiveValues(
      result = NULL, years = NULL, warnings = character(0),
      running = FALSE, eobs = NULL, lai_source = NA_character_,
      engine_log = NULL,
      # Valeurs dérivées par le moteur (spec 035 B4.a) : rendent visible la
      # spatialisation du lai_max et de la réserve utile.
      lai_max = NULL, ewm = NULL, ewm_source = NA_character_,
      # Incrémenté après acquisition de la série `rr` : re-rend la carte contexte.
      context_refresh = 0L,
      # Chrono des boutons async (spec 027 feedback) : instant de départ, remis
      # à NULL à la fin de la tâche. NULL = pas de run en cours.
      engine_running = FALSE, eobs_running = FALSE,
      engine_start = NULL, eobs_start = NULL,
      # Chrono du téléchargement des précipitations E-OBS (~800 Mo) : notif
      # persistante « engrenage + MM:SS » tant que la tâche tourne.
      eobs_rr_running = FALSE, eobs_rr_start = NULL,
      # Chrono du moteur « risque de gel tardif » (R7, meteoland).
      frost_running = FALSE, frost_start = NULL,
      # Restauration différée à l'entrée dans l'onglet reGénération (et non à
      # l'ouverture du projet) : un changement de projet/UGF pose ce drapeau ;
      # l'observer de restauration ne le consomme (toast + relecture cache) que
      # lorsque l'onglet reGénération est actif.
      regen_needs_restore = FALSE
    )

    # UGF units of the current project (EPSG:2154), with the same geometry
    # fallback chain as mod_monitoring_pixel_map : indicators_sf → UGF →
    # parcelles. Sans ce repli, un projet Reconfort/monitoring qui n'a pas
    # (encore) calculé les 31 indicateurs n'a pas d'indicators_sf, et l'onglet
    # tombait sur le message trompeur « besoin d'un projet ».
    units_sf <- shiny::reactive({
      project <- app_state$current_project
      if (is.null(project)) return(NULL)
      to_2154 <- function(x) tryCatch(sf::st_transform(x, 2154), error = function(e) x)

      sfx <- project$indicators_sf
      if (inherits(sfx, "sf") && nrow(sfx) > 0L) return(to_2154(sfx))

      ugf <- tryCatch(if (has_ug_data(project)) ug_build_sf(project) else NULL,
                      error = function(e) NULL)
      if (inherits(ugf, "sf") && nrow(ugf) > 0L) return(to_2154(ugf))

      parc <- project$parcels
      if (inherits(parc, "sf") && nrow(parc) > 0L) return(to_2154(parc))
      NULL
    })

    # --- Restauration à l'ouverture d'un projet (spec 035 B2) ----------------
    # Sans ça, rouvrir un projet déjà analysé affichait les contours d'UGF nus :
    # `rv$result` n'était écrit que par « Lancer l'analyse » ou par la fin du
    # moteur. Ici on RELIT le cache disque via `restore_regeneration()`.
    #
    # NE PAS appeler `run_regeneration()` ici. Son étape R3
    # (`indicateur_r3_secheresse(dem = )`) re-dérive la topographie depuis la
    # mosaïque MNT LiDAR : 132 s mesurées sur un projet de 30 UGF, contre 0,3 s
    # pour tout le reste. Shiny étant mono-thread, cet appel synchrone gelait
    # TOUTE la session à l'ouverture — y compris l'onglet Sélection.
    #
    # Déclenché sur (projet, unités) : le projet est posé avant les géométries,
    # donc un observateur sur le seul `current_project` verrait `units_sf()` NULL
    # et n'aurait aucune occasion de réessayer.
    # (A) Observateur DONNÉES — tout onglet. Un changement de projet (ou d'UGF)
    # purge le résultat précédent (sinon le choroplèthe d'un projet resterait à
    # l'écran, et mod_synthesis lirait une reGénération périmée) et pose le
    # drapeau « à restaurer ». AUCUN toast, AUCUNE relecture ici : la
    # restauration est différée à l'entrée dans l'onglet reGénération (obs. B).
    shiny::observeEvent(list(app_state$current_project, units_sf()), {
      rv$result <- NULL
      rv$lai_source <- NA_character_
      app_state$regeneration_result <- NULL
      rv$regen_needs_restore <- TRUE
    }, ignoreNULL = FALSE)

    # (B) Observateur RESTAURATION — uniquement quand l'onglet reGénération est
    # actif. C'est ici — et pas à l'ouverture du projet — que le toast « en bas »
    # s'affiche et que le cache disque est relu. Déclenché par l'activation de
    # l'onglet OU par un nouveau drapeau (changement de projet/UGF pendant qu'on
    # est déjà sur l'onglet). Le drapeau est consommé (une seule tentative) pour
    # ne pas re-toaster à chaque aller-retour d'onglet.
    shiny::observeEvent(list(app_state$active_main_tab, rv$regen_needs_restore), {
      if (!identical(app_state$active_main_tab, "regeneration")) return()
      if (!isTRUE(rv$regen_needs_restore)) return()
      project_path <- tryCatch(app_state$current_project$path, error = function(e) NULL)
      if (is.null(project_path)) return()
      units <- units_sf()
      if (is.null(units)) return()

      rv$regen_needs_restore <- FALSE   # consommer : une seule tentative

      pc <- load_regeneration_precomputed(project_path)
      # Ne restaurer que si une sortie de MOTEUR existe : `dem` / `eobs_*` seuls
      # ne produisent aucun indice, et une restauration à vide n'affiche rien.
      if (is.null(pc$biljou) && is.null(pc$sensibilite)) return()

      # Retour visuel « en bas » (parité Suivi sanitaire) : la restauration +
      # le rendu des cartes prennent un temps perceptible. On peint le toast
      # MAINTENANT, puis on diffère le travail synchrone d'un tick (`later`) pour
      # qu'il s'affiche AVANT le calcul — sinon show + remove tomberaient dans le
      # même flush Shiny (invisibles).
      nid <- session$ns("regen_restore_notif")
      shiny::showNotification(i18n$t("regen_restore_loading"), id = nid,
                              duration = NULL, type = "message")
      later::later(function() {
        res <- tryCatch(restore_regeneration(units, precomputed = pc),
                        error = function(e) NULL)
        if (!is.null(res)) {
          rv$result <- res$units
          # Restaurer n'est pas analyser : ni avertissements, ni années détectées.
          rv$warnings <- character(0)
          rv$canopy_source <- regen_canopy_provenance(res$units)
          # mod_synthesis lit app_state$regeneration_result pour la perspective
          # IA : sans cette ligne, la synthèse d'un projet rouvert ignore la
          # reGénération.
          app_state$regeneration_result <- res$units
        }
        try(shiny::removeNotification(nid, session = session), silent = TRUE)
      }, delay = 0.05)
    }, ignoreNULL = FALSE)

    # Populate the optional target-species selector from the core
    # (nemeton::regen_species_choices). Réactif à l'AOI : les essences présentes
    # sur les parcelles (BD Forêt v2 → classe via map_tfv_to_species_class) sont
    # listées en tête (groupe « présentes »), puis les essences d'adaptation.
    shiny::observe({
      units <- tryCatch(units_sf(), error = function(e) NULL)
      df <- regeneration_species_choices(units = units, lang = i18n$language)
      generic_lbl <- i18n$t("regen_species_generic")
      if (is.null(df) || !is.data.frame(df) || !all(c("code", "label") %in% names(df))) {
        shiny::updateSelectInput(session, "species",
          choices = stats::setNames("", generic_lbl))
        return()
      }
      named <- function(sub) stats::setNames(sub$code, sub$label)
      choices <- list()
      choices[[generic_lbl]] <- ""   # option générique (toutes essences) en tête
      if ("groupe" %in% names(df)) {
        present <- df[df$groupe == "present", , drop = FALSE]
        others  <- df[df$groupe != "present", , drop = FALSE]
        if (nrow(present)) choices[[i18n$t("regen_species_group_present")]] <- named(present)
        if (nrow(others))  choices[[i18n$t("regen_species_group_adaptation")]] <- named(others)
      } else {
        choices[[i18n$t("regen_species_group_adaptation")]] <- named(df)
      }
      shiny::updateSelectInput(session, "species", choices = choices,
                               selected = shiny::isolate(input$species) %||% "")
    })

    # Auto-detect the average / heatwave years from E-OBS for this AOI.
    # Async : l'acquisition E-OBS (CDS) est déléguée à un worker future (cf.
    # eobs_task) ; le résultat met à jour les deux champs Année.
    shiny::observeEvent(input$auto_years, {
      units <- units_sf()
      if (is.null(units)) {
        # Le bouton passe « busy » dès le clic : le remettre prêt sur ce retour
        # anticipé, sinon il reste grisé.
        bslib::update_task_button("auto_years", state = "ready")
        shiny::showNotification(i18n$t("regen_need_project"), type = "warning")
        return()
      }
      project_path <- tryCatch(app_state$current_project$path, error = function(e) NULL)
      rv$eobs_running <- TRUE
      rv$eobs_start <- Sys.time()
      # Notif persistante en bas à droite (retirée en fin de tâche) — le run
      # E-OBS (CDS) peut durer plusieurs minutes.
      shiny::showNotification(i18n$t("regen_auto_running"), type = "message",
                              duration = NULL, id = session$ns("eobs_notif"))
      eobs_task$invoke(units, project_path, .dev_pkg_path, get_app_options())
    })

    shiny::observeEvent(eobs_task$status(), {
      st <- eobs_task$status()
      if (identical(st, "success")) {
        rv$eobs_running <- FALSE
        rv$eobs_start <- NULL
        shiny::removeNotification(session$ns("eobs_notif"))
        yrs <- tryCatch(eobs_task$result(), error = function(e) NULL)
        if (!is.null(yrs) && !is.null(yrs$year_moyenne)) {
          shiny::updateNumericInput(session, "year_moyenne", value = yrs$year_moyenne)
          shiny::updateNumericInput(session, "year_canicule", value = yrs$year_canicule)
          rv$eobs <- yrs
          shiny::showNotification(
            sprintf(i18n$t("regen_auto_done"), yrs$year_moyenne, yrs$year_canicule),
            type = "message", duration = 6)
        } else {
          shiny::showNotification(i18n$t("regen_auto_none"), type = "warning", duration = 6)
        }
      } else if (identical(st, "error")) {
        rv$eobs_running <- FALSE
        rv$eobs_start <- NULL
        shiny::removeNotification(session$ns("eobs_notif"))
        shiny::showNotification(i18n$t("regen_auto_none"), type = "warning", duration = 6)
      }
    })

    # Statut/chrono sous le bouton Auto (E-OBS) — tick chaque seconde tant que
    # la détection tourne (spec 027 feedback). Le sablier « en cours » vit
    # désormais ici (retiré de eobs_index_display pour ne pas doubler).
    output$eobs_status <- shiny::renderUI({
      if (!isTRUE(rv$eobs_running)) return(NULL)
      shiny::invalidateLater(1000)
      htmltools::div(class = "small text-info mt-1",
        bsicons::bs_icon("hourglass-split", class = "me-1"),
        i18n$t("regen_auto_running_short"),
        htmltools::tags$span(class = "ms-1 font-monospace",
                             .fmt_elapsed(rv$eobs_start)))
    })

    output$eobs_index_display <- shiny::renderUI({
      if (isTRUE(rv$eobs_running)) return(NULL)   # chrono affiché par eobs_status
      if (is.null(rv$eobs)) return(NULL)
      htmltools::tags$small(class = "text-muted d-block mb-2",
        sprintf("%s : %s / %s", i18n$t("regen_eobs_index"),
                rv$eobs$year_moyenne %||% "?", rv$eobs$year_canicule %||% "?"))
    })

    # --- Run --------------------------------------------------------------
    shiny::observeEvent(input$run, {
      if (deny_if_readonly(app_state, i18n)) return()
      units <- units_sf()
      if (is.null(units)) {
        shiny::showNotification(i18n$t("regen_need_project"), type = "warning")
        return()
      }
      rv$running <- TRUE
      project_path <- tryCatch(app_state$current_project$path, error = function(e) NULL)
      # NA/NULL numeric inputs → NULL (let the core auto-detect / default).
      na_null <- function(x) if (is.null(x) || (length(x) == 1 && is.na(x))) NULL else x
      cfg <- list(
        year_moyenne = na_null(input$year_moyenne),
        year_canicule = na_null(input$year_canicule),
        forest_type = input$forest_type %||% "feuillu",
        lai_max = na_null(input$lai_max),
        species = if (nzchar(input$species %||% "")) input$species else NULL,
        hydric_only = isTRUE(input$hydric_only)
      )

      # withProgress affiche un overlay immédiat (retour visible avant le calcul
      # synchrone) et empêche les reclics pendant le run.
      res <- shiny::withProgress(message = i18n$t("regen_running"), value = 0.2, {
        precomputed <- load_regeneration_precomputed(project_path)
        shiny::incProgress(0.3)
        out <- tryCatch(
          run_regeneration(units, cfg = cfg, precomputed = precomputed),
          error = function(e) {
            shiny::showNotification(
              sprintf("%s: %s", i18n$t("error"), conditionMessage(e)), type = "error")
            NULL
          })
        shiny::incProgress(0.5)
        out
      })
      rv$running <- FALSE
      if (!is.null(res)) {
        rv$result <- res$units
        rv$years <- res$years
        rv$warnings <- res$warnings %||% character(0)
        # Provenance canopée lue sur le résultat (repli satellite / LiDAR HD).
        rv$canopy_source <- regen_canopy_provenance(res$units)
        # Publier le résultat pour l'export PDF (section reGénération de mod_synthesis).
        app_state$regeneration_result <- res$units
        nw <- length(rv$warnings)
        if (nw > 0L) {
          shiny::showNotification(sprintf(i18n$t("regen_run_done_warn"), nw),
                                  type = "warning", duration = 8)
        } else {
          shiny::showNotification(i18n$t("regen_run_done"), type = "message", duration = 5)
        }
      }
    })

    # --- Essence cible : mise à jour LIVE de la choroplèthe ----------------
    # L'essence n'alimente que la dernière étape (indice de priorité). Sur un
    # résultat déjà analysé, on re-priorise en place — sans rejouer l'analyse
    # complète (donc sans R3/topographie LiDAR) — pour que la carte reflète
    # aussitôt l'essence choisie. `ignoreInit` : ne pas réagir au peuplement du
    # sélecteur. On s'efface pendant un run / le moteur (qui appliquent déjà
    # l'essence eux-mêmes).
    shiny::observeEvent(input$species, {
      if (is.null(rv$result)) return()
      if (isTRUE(rv$running) || isTRUE(rv$engine_running)) return()
      updated <- regen_reprioritize(rv$result, input$species)
      rv$result <- updated
      app_state$regeneration_result <- updated
    }, ignoreInit = TRUE)

    # --- Moteur microclimf réel (option B, async) -------------------------
    # Provenance identique à la session principale pour recharger le namespace
    # dans le worker future (cf. mod_home compute_task). Le run est lourd
    # (LiDAR HD + ERA5 + microclimf) : il tourne dans un process séparé, ne
    # renvoie qu'un sf, et la sortie est cachée (sensibilite.gpkg) pour être
    # consommée en fast-path par run_regeneration au run suivant.
    .dev_pkg_path <- tryCatch(
      if (isTRUE(pkgload::is_dev_package("nemetonshiny")))
        find.package("nemetonshiny") else NULL,
      error = function(e) NULL)

    engine_task <- shiny::ExtendedTask$new(
      function(units, project_path, cfg, dev_path, app_opts) {
        if (requireNamespace("future", quietly = TRUE)) {
          plan_classes <- class(future::plan())
          if (!any(c("multisession", "multicore", "cluster") %in% plan_classes)) {
            future::plan("multisession")
          }
        }
        promises::future_promise({
          if (!is.null(dev_path) && requireNamespace("pkgload", quietly = TRUE)) {
            pkgload::load_all(dev_path, quiet = TRUE)
          } else {
            loadNamespace("nemetonshiny")
          }
          options(nemeton.app_options = app_opts)
          fn <- getFromNamespace("run_regeneration_engine", "nemetonshiny")
          fn(units, project_path, cfg)
        }, seed = TRUE)
      })

    # Auto E-OBS async : l'acquisition E-OBS (CDS, même clé qu'ERA5) peut être
    # lente / mise en file → worker future, ne renvoie que la paire d'années.
    eobs_task <- shiny::ExtendedTask$new(
      function(units, project_path, dev_path, app_opts) {
        if (requireNamespace("future", quietly = TRUE)) {
          plan_classes <- class(future::plan())
          if (!any(c("multisession", "multicore", "cluster") %in% plan_classes)) {
            future::plan("multisession")
          }
        }
        promises::future_promise({
          if (!is.null(dev_path) && requireNamespace("pkgload", quietly = TRUE)) {
            pkgload::load_all(dev_path, quiet = TRUE)
          } else {
            loadNamespace("nemetonshiny")
          }
          options(nemeton.app_options = app_opts)
          fn <- getFromNamespace("run_regeneration_detect_years", "nemetonshiny")
          fn(units, project_path)
        }, seed = TRUE)
      })

    # Acquisition opt-in de la série précipitations E-OBS (~800 Mo, CDS). Jamais
    # déclenchée par un rendu : uniquement sur clic explicite, dans un worker.
    eobs_rr_task <- shiny::ExtendedTask$new(
      function(units, project_path, dev_path, app_opts) {
        if (requireNamespace("future", quietly = TRUE)) {
          plan_classes <- class(future::plan())
          if (!any(c("multisession", "multicore", "cluster") %in% plan_classes)) {
            future::plan("multisession")
          }
        }
        promises::future_promise({
          if (!is.null(dev_path) && requireNamespace("pkgload", quietly = TRUE)) {
            pkgload::load_all(dev_path, quiet = TRUE)
          } else {
            loadNamespace("nemetonshiny")
          }
          options(nemeton.app_options = app_opts)
          fn <- getFromNamespace("regen_fetch_eobs_rr", "nemetonshiny")
          fn(units, project_path)
        }, seed = TRUE)
      })

    # Moteur « risque de gel tardif » (R7, meteoland). Opt-in, worker future,
    # sortie cachée (tmin_*.tif) : interpolation Tmin journalière → indicateur R7.
    frost_task <- shiny::ExtendedTask$new(
      function(units, project_path, cfg, dev_path, app_opts) {
        if (requireNamespace("future", quietly = TRUE)) {
          plan_classes <- class(future::plan())
          if (!any(c("multisession", "multicore", "cluster") %in% plan_classes)) {
            future::plan("multisession")
          }
        }
        promises::future_promise({
          if (!is.null(dev_path) && requireNamespace("pkgload", quietly = TRUE)) {
            pkgload::load_all(dev_path, quiet = TRUE)
          } else {
            loadNamespace("nemetonshiny")
          }
          options(nemeton.app_options = app_opts)
          fn <- getFromNamespace("run_regeneration_frost", "nemetonshiny")
          fn(units, project_path, cfg)
        }, seed = TRUE)
      })

    # Lier chaque input_task_button à sa tâche : bslib désactive le bouton +
    # affiche le spinner tant que la tâche tourne, puis le réactive (succès OU
    # erreur). Empêche les runs concurrents (réécriture sensibilite/biljou.gpkg).
    # `bind_task_button()` prend l'id local au module (sans ns()).
    bslib::bind_task_button(engine_task, "run_engine")
    bslib::bind_task_button(eobs_task, "auto_years")
    bslib::bind_task_button(eobs_rr_task, "fetch_eobs_rr")
    bslib::bind_task_button(frost_task, "run_frost")

    shiny::observeEvent(input$fetch_eobs_rr, {
      if (deny_if_readonly(app_state, i18n)) {
        bslib::update_task_button("fetch_eobs_rr", state = "ready")
        return()
      }
      units <- units_sf()
      project_path <- tryCatch(app_state$current_project$path, error = function(e) NULL)
      if (is.null(units) || is.null(project_path)) {
        bslib::update_task_button("fetch_eobs_rr", state = "ready")
        shiny::showNotification(i18n$t("regen_need_project"), type = "warning")
        return()
      }
      # Notif persistante « engrenage qui tourne + chrono MM:SS » (cadre unifié
      # partagé avec les moteurs) : le téléchargement (~800 Mo, CDS) dure
      # plusieurs minutes. Retirée en fin de tâche par l'observer de statut.
      rv$eobs_rr_running <- TRUE
      rv$eobs_rr_start <- Sys.time()
      shiny::showNotification(
        .running_notif_content(i18n$t("regen_eobs_rr_running"), rv$eobs_rr_start),
        id = session$ns("eobs_rr_notif"), type = "message", duration = NULL)
      eobs_rr_task$invoke(units, project_path, .dev_pkg_path, get_app_options())
    })

    # Tick 1 s : rafraîchit le chrono de la notif de téléchargement `rr` (même id
    # → Shiny remplace le contenu en place) tant que la tâche tourne.
    shiny::observe({
      if (!isTRUE(rv$eobs_rr_running)) return()
      shiny::invalidateLater(1000)
      shiny::showNotification(
        .running_notif_content(i18n$t("regen_eobs_rr_running"),
                               shiny::isolate(rv$eobs_rr_start)),
        id = session$ns("eobs_rr_notif"), type = "message", duration = NULL)
    })

    shiny::observeEvent(eobs_rr_task$status(), {
      st <- eobs_rr_task$status()
      if (identical(st, "success") || identical(st, "error")) {
        rv$eobs_rr_running <- FALSE
        rv$eobs_rr_start <- NULL
        shiny::removeNotification(session$ns("eobs_rr_notif"))
      }
      if (identical(st, "success")) {
        ok <- tryCatch(eobs_rr_task$result(), error = function(e) FALSE)
        if (isTRUE(ok)) {
          rv$context_refresh <- (rv$context_refresh %||% 0L) + 1L   # re-rend la carte
          shiny::showNotification(i18n$t("regen_eobs_rr_done"), type = "message", duration = 6)
        } else {
          shiny::showNotification(i18n$t("regen_eobs_rr_failed"), type = "warning", duration = 10)
        }
      } else if (identical(st, "error")) {
        err <- tryCatch(eobs_rr_task$result(), error = function(e) conditionMessage(e))
        shiny::showNotification(
          sprintf("%s: %s", i18n$t("error"), .strip_ansi(as.character(err))),
          type = "error", duration = 10)
      }
    })

    # --- Moteur « risque de gel tardif » (R7, meteoland) ------------------
    shiny::observeEvent(input$run_frost, {
      if (deny_if_readonly(app_state, i18n)) {
        bslib::update_task_button("run_frost", state = "ready")
        return()
      }
      units <- rv$result %||% units_sf()   # enrichir le résultat courant si présent
      project_path <- tryCatch(app_state$current_project$path, error = function(e) NULL)
      if (is.null(units) || is.null(project_path)) {
        bslib::update_task_button("run_frost", state = "ready")
        shiny::showNotification(i18n$t("regen_need_project"), type = "warning")
        return()
      }
      if (!regen_meteoland_available()) {
        bslib::update_task_button("run_frost", state = "ready")
        shiny::showNotification(i18n$t("regen_frost_unavailable"), type = "warning", duration = 8)
        return()
      }
      na_null <- function(x) if (is.null(x) || (length(x) == 1 && is.na(x))) NULL else x
      cfg <- list(
        year_moyenne = na_null(input$year_moyenne),
        year_canicule = na_null(input$year_canicule),
        buffer_m = (input$buffer_km %||% 25) * 1000)
      rv$frost_running <- TRUE
      rv$frost_start <- Sys.time()
      shiny::showNotification(
        .running_notif_content(i18n$t("regen_frost_running"), rv$frost_start),
        id = session$ns("frost_notif"), type = "message", duration = NULL)
      frost_task$invoke(units, project_path, cfg, .dev_pkg_path, get_app_options())
    })

    # Tick 1 s : chrono de la notif « interpolation gel ».
    shiny::observe({
      if (!isTRUE(rv$frost_running)) return()
      shiny::invalidateLater(1000)
      shiny::showNotification(
        .running_notif_content(i18n$t("regen_frost_running"),
                               shiny::isolate(rv$frost_start)),
        id = session$ns("frost_notif"), type = "message", duration = NULL)
    })

    shiny::observeEvent(frost_task$status(), {
      st <- frost_task$status()
      if (identical(st, "success") || identical(st, "error")) {
        rv$frost_running <- FALSE
        rv$frost_start <- NULL
        shiny::removeNotification(session$ns("frost_notif"))
      }
      if (identical(st, "success")) {
        res <- tryCatch(frost_task$result(), error = function(e) NULL)
        if (!is.null(res) && !is.null(res$units)) {
          rv$result <- res$units
          app_state$regeneration_result <- res$units
          if (identical(res$r7_status, "calculated")) {
            shiny::showNotification(i18n$t("regen_frost_done"), type = "message", duration = 6)
          } else {
            # Tmin indisponible (meteoland/SAFRAN KO, < N stations) : R7 skip,
            # jamais un crash — l'utilisateur sait pourquoi le radar n'a pas R7.
            shiny::showNotification(i18n$t("regen_frost_skipped"), type = "warning", duration = 10)
          }
        }
      } else if (identical(st, "error")) {
        err <- tryCatch(frost_task$result(), error = function(e) conditionMessage(e))
        shiny::showNotification(
          sprintf("%s: %s", i18n$t("error"), .strip_ansi(as.character(err))),
          type = "error", duration = 10)
      }
    })

    # Invalidation manuelle du cache PAI (§3 brief pai-cache) : supprime
    # cache/regeneration/pai.tif → le prochain run recalcule la structure de
    # végétation depuis le nuage LiDAR. Sans effet si le fichier n'existe pas.
    shiny::observeEvent(input$recompute_pai, {
      if (deny_if_readonly(app_state, i18n)) return()
      project_path <- tryCatch(app_state$current_project$path, error = function(e) NULL)
      if (is.null(project_path)) return()
      f <- file.path(project_path, "cache", "regeneration", "pai.tif")
      if (file.exists(f)) {
        unlink(f)
        shiny::showNotification(i18n$t("regen_pai_cache_cleared"), type = "message")
      }
    })

    shiny::observeEvent(input$run_engine, {
      if (deny_if_readonly(app_state, i18n)) {
        bslib::update_task_button("run_engine", state = "ready")
        return()
      }
      units <- units_sf()
      project_path <- tryCatch(app_state$current_project$path, error = function(e) NULL)
      if (is.null(units) || is.null(project_path)) {
        bslib::update_task_button("run_engine", state = "ready")
        shiny::showNotification(i18n$t("regen_need_project"), type = "warning")
        return()
      }
      forcing <- input$forcing %||% "safran"
      pre <- regen_engine_prereqs(project_path, forcing)
      if (!isTRUE(pre$ok)) {
        bslib::update_task_button("run_engine", state = "ready")
        shiny::showNotification(i18n$t(pre$reason), type = "warning", duration = 8)
        return()
      }
      na_null <- function(x) if (is.null(x) || (length(x) == 1 && is.na(x))) NULL else x
      cfg <- list(
        year_moyenne = na_null(input$year_moyenne),
        year_canicule = na_null(input$year_canicule),
        forest_type = input$forest_type %||% "feuillu",
        forcing = forcing,
        # ewm NULL => SoilGrids par UGF ; valeur saisie => sol uniforme forcé.
        ewm = na_null(input$ewm),
        rooting_depth_cm = na_null(input$rooting_depth_cm),
        lai_max = na_null(input$lai_max),
        budburst = na_null(input$budburst),
        leaf_fall = na_null(input$leaf_fall)
      )
      # Supprimer un engine_status.json périmé d'un run précédent : sans ça, le
      # poll afficherait une phase fantôme du run antérieur au démarrage.
      unlink(file.path(project_path, "cache", "regeneration", "engine_status.json"))
      rv$engine_running <- TRUE
      rv$engine_start <- Sys.time()
      # Notif persistante bas-droite (retirée en fin de tâche) — le moteur réel
      # (LiDAR HD + ERA5 + microclimf) peut durer plusieurs minutes. Le libellé
      # est ensuite rafraîchi phase par phase par l'observe de poll ci-dessous.
      shiny::showNotification(i18n$t("regen_engine_running"), type = "message",
                              duration = NULL, id = session$ns("engine_notif"))
      engine_task$invoke(units, project_path, cfg, .dev_pkg_path, get_app_options())
    })

    # Poll (1 s) du fichier d'état écrit par le worker : rafraîchit la notif
    # persistante bas-droite (même id → Shiny remplace le contenu en place) avec
    # le libellé de la phase en cours + chrono. Cf. brief engine-phase-status §3.3.
    shiny::observe({
      if (!isTRUE(rv$engine_running)) return()
      shiny::invalidateLater(1000)
      project_path <- tryCatch(app_state$current_project$path, error = function(e) NULL)
      st  <- .regen_read_phase(project_path)
      lbl <- if (is.null(st)) i18n$t("regen_engine_running") else .regen_phase_label(i18n, st)
      if (!nzchar(lbl)) lbl <- i18n$t("regen_engine_running")   # done / illisible
      shiny::showNotification(
        .running_notif_content(lbl, rv$engine_start),
        id = session$ns("engine_notif"), type = "message", duration = NULL)
    })

    shiny::observeEvent(engine_task$status(), {
      st <- engine_task$status()
      if (identical(st, "success")) {
        rv$engine_running <- FALSE
        rv$engine_start <- NULL
        shiny::removeNotification(session$ns("engine_notif"))
        .regen_cleanup_status(app_state)
        # Le worker a écrit sensibilite.gpkg / biljou.gpkg : recharger le
        # precomputed et relancer l'analyse normale (fast-path) pour rafraîchir
        # carte/table. Les avertissements spécifiques du moteur (échec réel
        # microclimf/BILJOU) sont remontés en toast.
        eng <- tryCatch(engine_task$result(), error = function(e) NULL)
        project_path <- tryCatch(app_state$current_project$path, error = function(e) NULL)
        units <- units_sf()
        res <- NULL   # peut rester NULL : le bloc de rechargement est conditionnel
        if (!is.null(units) && !is.null(project_path)) {
          precomputed <- load_regeneration_precomputed(project_path)
          na_null <- function(x) if (is.null(x) || (length(x) == 1 && is.na(x))) NULL else x
          cfg <- list(
            year_moyenne = na_null(input$year_moyenne),
            year_canicule = na_null(input$year_canicule),
            forest_type = input$forest_type %||% "feuillu",
            lai_max = na_null(input$lai_max),
            species = if (nzchar(input$species %||% "")) input$species else NULL,
            hydric_only = isTRUE(input$hydric_only))
          res <- tryCatch(run_regeneration(units, cfg = cfg, precomputed = precomputed),
                          error = function(e) NULL)
          if (!is.null(res)) {
            rv$result <- res$units
            rv$years <- res$years
            app_state$regeneration_result <- res$units
          }
        }
        # Provenance rapportée par le moteur (fiable : survit au cache gpkg qui
        # perdrait l'attribut lai_source lu par detect_ndp).
        rv$canopy_source <- eng$canopy %||% NA_character_
        rv$lai_source <- eng$lai_source %||% NA_character_
        rv$lai_max <- eng$lai_max
        rv$ewm <- eng$ewm
        rv$ewm_source <- eng$ewm_source %||% NA_character_

        # B3.b — CUMULER, ne pas écraser. `res$warnings` sont ceux du re-run
        # fast-path (un ensemble sans rapport) ; les écrire seuls effaçait les
        # diagnostics du moteur, qui ne survivaient alors que dans un toast de 10 s.
        eng_warns <- eng$warnings %||% character(0)
        log <- .regen_read_log(project_path)
        rv$engine_log <- log
        .regen_relay_log(log)                            # B3.c — console du process principal
        rv$warnings <- unique(c(res$warnings %||% character(0), eng_warns))

        if (length(eng_warns)) {
          shiny::showNotification(
            htmltools::tags$span(paste(eng_warns, collapse = " — ")),
            type = "warning", duration = 10)
        }
        shiny::showNotification(i18n$t("regen_engine_done"), type = "message", duration = 6)
      } else if (identical(st, "error")) {
        rv$engine_running <- FALSE
        rv$engine_start <- NULL
        shiny::removeNotification(session$ns("engine_notif"))
        .regen_cleanup_status(app_state)
        project_path <- tryCatch(app_state$current_project$path, error = function(e) NULL)
        err <- tryCatch(engine_task$result(), error = function(e) conditionMessage(e))

        # B3.b — le worker est mort (OOM, kill) : `engine_task$result()` lève et
        # `eng` n'existe pas. Les avertissements accumulés avant la mort ne sont
        # récupérables que par le journal disque.
        log <- .regen_read_log(project_path)
        rv$engine_log <- log
        .regen_relay_log(log)
        err_txt <- .strip_ansi(as.character(err))
        cli::cli_alert_danger("regen engine: {err_txt}")
        rv$warnings <- unique(c(.regen_log_issues(log), err_txt))

        shiny::showNotification(
          sprintf("%s: %s", i18n$t("error"), err_txt),
          type = "error", duration = 10)
      }
    })

    output$engine_status <- shiny::renderUI({
      if (isTRUE(rv$engine_running)) {
        shiny::invalidateLater(1000)   # chrono re-rendu chaque seconde
        return(htmltools::div(class = "small text-info mt-1",
          bsicons::bs_icon("hourglass-split", class = "me-1"),
          i18n$t("regen_engine_running_short"),
          htmltools::tags$span(class = "ms-1 font-monospace",
                               .fmt_elapsed(rv$engine_start))))
      }
      project_path <- tryCatch(app_state$current_project$path, error = function(e) NULL)
      if (is.null(project_path)) return(NULL)
      pre <- regen_engine_prereqs(project_path, input$forcing %||% "safran")
      if (isTRUE(pre$reason == "regen_engine_prereq_core")) {
        return(htmltools::div(class = "small text-muted mt-1",
          bsicons::bs_icon("info-circle", class = "me-1"), i18n$t("regen_engine_prereq_core")))
      }
      # Une ligne par moteur : prêt (vert) ou prérequis manquant (gris).
      line <- function(ready, ok_key, ko_key) {
        if (isTRUE(ready)) {
          htmltools::div(class = "small text-success",
            bsicons::bs_icon("check-circle", class = "me-1"), i18n$t(ok_key))
        } else {
          htmltools::div(class = "small text-muted",
            bsicons::bs_icon("dash-circle", class = "me-1"), i18n$t(ko_key))
        }
      }
      htmltools::div(class = "mt-1",
        line(pre$microclimf, "regen_engine_ready_micro", "regen_engine_status_micro"),
        line(pre$biljou, "regen_engine_ready_biljou", "regen_engine_status_biljou_era5"))
    })

    # Badge de provenance canopée (spec 033 D5). Priorité à la source rapportée
    # par le moteur (rv$canopy_source) ; sinon lecture de detect_ndp() sur le
    # résultat. Rien affiché tant qu'aucune canopée n'est utilisée.
    output$canopy_provenance <- shiny::renderUI({
      src <- rv$canopy_source
      if (is.null(src) || is.na(src)) {
        src <- if (!is.null(rv$result)) regen_canopy_provenance(rv$result) else NA_character_
      }
      if (is.na(src)) return(NULL)
      if (identical(src, "satellite")) {
        bslib::tooltip(
          htmltools::tags$span(class = "badge text-bg-warning mt-2 d-inline-block",
            bsicons::bs_icon("badge-sd", class = "me-1"), i18n$t("regen_canopee_satellite")),
          i18n$t("regen_canopee_satellite_info"), placement = "right")
      } else {
        # Canopée LiDAR HD : le PAI structural a été dérivé du nuage et mis en
        # cache (cache/regeneration/pai.tif). On expose ici l'invalidation manuelle
        # du cache — pertinente seulement dans ce cas (le repli satellite n'écrit
        # pas de pai.tif). Le lien n'apparaît donc qu'après un run LiDAR réussi.
        htmltools::tagList(
          htmltools::tags$span(class = "badge text-bg-success mt-2 d-inline-block",
            bsicons::bs_icon("badge-hd", class = "me-1"), i18n$t("regen_canopee_lidar")),
          # Le PAI structural alimente aussi le lai_max de BILJOU (spec 035 B1.a) :
          # le dire, sinon rien ne distingue ce run de l'ancien, où le bilan
          # hydrique retombait sur le défaut cœur par type de peuplement.
          if (identical(rv$lai_source, "pai_lidar"))
            htmltools::tags$small(class = "text-muted d-block mt-1",
              bsicons::bs_icon("droplet-half", class = "me-1"),
              i18n$t("regen_canopy_pai_lidar")),
          bslib::tooltip(
            shiny::actionLink(ns("recompute_pai"), i18n$t("regen_pai_recompute"),
              icon = bsicons::bs_icon("arrow-repeat"), class = "small d-block mt-1"),
            i18n$t("regen_pai_recompute_tip"), placement = "right")
        )
      }
    })

    # Valeurs dérivées (spec 035 B4.a) : ce que le moteur a RÉELLEMENT utilisé.
    # Sans ça, rien à l'écran ne distingue un lai_max par UGF d'un scalaire, et le
    # repli SoilGrids → sol uniforme reste invisible.
    output$derived_stats <- shiny::renderUI({
      na_null <- function(x) if (is.null(x) || (length(x) == 1 && is.na(x))) NULL else x
      forced_badge <- htmltools::tags$span(
        class = "badge text-bg-warning ms-1", i18n$t("regen_override_badge"))
      line <- function(icon, body) htmltools::tags$div(
        class = "small text-muted mt-1", bsicons::bs_icon(icon, class = "me-1"), body)

      lai <- if (!is.null(na_null(input$lai_max))) {
        line("sliders", htmltools::tagList(i18n$t("regen_lai_max"), forced_badge))
      } else {
        st <- .regen_derived_stats(rv$lai_max)
        if (!is.null(st))
          line("bounding-box", sprintf(i18n$t("regen_lai_derived_stats"),
                                       st$median, st$min, st$max, st$n))
      }

      ewm <- if (!is.null(na_null(input$ewm))) {
        line("sliders", htmltools::tagList(i18n$t("regen_ewm"), forced_badge))
      } else if (identical(rv$ewm_source, "soilgrids_fallback")) {
        # Le seul endroit où le repli silencieux devient visible sans lire le journal.
        htmltools::tags$div(class = "small text-warning mt-1",
          bsicons::bs_icon("exclamation-triangle-fill", class = "me-1"),
          sprintf(i18n$t("regen_ewm_fallback_uniform"),
                  format(round(as.numeric(rv$ewm)[1], 0), trim = TRUE)))
      } else {
        st <- .regen_derived_stats(rv$ewm, digits = 0)
        if (!is.null(st))
          line("layers", sprintf(i18n$t("regen_ewm_derived_stats"),
                                 st$median, st$min, st$max, st$n))
      }

      if (is.null(lai) && is.null(ewm)) return(NULL)
      htmltools::tagList(lai, ewm)
    })

    # Journal du moteur (B3.e) : `<details>` replié, horodatage + niveau + source.
    # Rendu dès qu'une entrée existe — y compris après un worker mort, où c'est
    # la seule trace des diagnostics.
    output$engine_log <- shiny::renderUI({
      log <- rv$engine_log
      if (!is.data.frame(log) || !nrow(log)) return(NULL)
      icon_of <- function(lvl) switch(lvl,
        "error" = bsicons::bs_icon("x-circle-fill", class = "text-danger me-1"),
        "warning" = bsicons::bs_icon("exclamation-triangle-fill", class = "text-warning me-1"),
        bsicons::bs_icon("info-circle", class = "text-muted me-1"))
      htmltools::tags$details(class = "small mt-1",
        htmltools::tags$summary(class = "text-muted",
          sprintf("%s (%d)", i18n$t("regen_engine_log"), nrow(log))),
        htmltools::tags$ul(class = "list-unstyled mb-0 mt-1",
          lapply(seq_len(nrow(log)), function(i) htmltools::tags$li(
            icon_of(log$level[i]),
            htmltools::tags$span(class = "font-monospace text-muted me-1",
              format(as.POSIXct(log$ts[i], origin = "1970-01-01"), "%H:%M:%S")),
            htmltools::tags$strong(class = "me-1", log$source[i]),
            log$message[i]))))
    })

    output$status <- shiny::renderUI({
      if (isTRUE(rv$running)) {
        return(htmltools::div(class = "alert alert-info py-2",
          bsicons::bs_icon("hourglass-split", class = "me-1"), i18n$t("regen_running")))
      }
      warns <- if (length(rv$warnings)) {
        htmltools::div(class = "alert alert-warning py-2 small",
          bsicons::bs_icon("exclamation-triangle", class = "me-1"),
          htmltools::tags$ul(class = "mb-0",
            lapply(rv$warnings, function(w) htmltools::tags$li(w))))
      }
      # Sans résultat, on affiche quand même les avertissements : un moteur qui
      # meurt (OOM) ne produit AUCUN résultat, et c'est précisément là qu'il faut
      # voir pourquoi. Auparavant le `return()` précoce les masquait.
      if (is.null(rv$result)) {
        return(htmltools::div(
          htmltools::div(class = "text-muted small fst-italic mb-2",
                         i18n$t("regen_need_project")),
          warns))
      }
      htmltools::div(
        htmltools::div(class = "alert alert-secondary py-2 small mb-2",
          bsicons::bs_icon("info-circle", class = "me-1"), i18n$t("regen_ndp_model")),
        warns
      )
    })

    # Base map — STABLE widget (parité cartes FORDEAD/FAST) : fonds OSM/Satellite
    # + contrôle de couches natif (boîtier coin haut-droit). Ne dépend que du
    # cadrage ; les polygones colorés (groupe « UGF ») sont (re)dessinés par
    # l'observer leafletProxy ci-dessous selon la couche choisie dans la sidebar.
    output$map <- leaflet::renderLeaflet({
      units <- units_sf()
      m <- leaflet::leaflet() |>
        leaflet::addProviderTiles("OpenStreetMap", group = "OSM") |>
        leaflet::addProviderTiles("Esri.WorldImagery", group = "Satellite") |>
        leaflet::addLayersControl(
          baseGroups    = c("OSM", "Satellite"),
          overlayGroups = "UGF",
          options       = leaflet::layersControlOptions(collapsed = TRUE))
      if (!is.null(units)) {
        # Contour UGF de base, visible immédiatement (avant toute analyse).
        # L'observer proxy le remplace par le choroplèthe dès qu'un résultat existe.
        geo <- tryCatch(sf::st_transform(units, 4326), error = function(e) units)
        m <- leaflet::addPolygons(m, data = geo, group = "UGF", weight = 1,
          color = "#1B6B1B", fillColor = "#1B6B1B", fillOpacity = 0.10)
        bb <- tryCatch(as.numeric(sf::st_bbox(geo)), error = function(e) NULL)
        if (!is.null(bb) && all(is.finite(bb))) {
          m <- leaflet::fitBounds(m, bb[1], bb[2], bb[3], bb[4])
        }
      }
      m
    })

    # Data layer via proxy — swap the choropleth + legend without recreating the
    # widget (préserve le LayersControl et le zoom). Réagit au résultat et au
    # radio « Couche affichée » de la sidebar.
    shiny::observe({
      res <- rv$result
      units <- units_sf()
      col <- input$map_layer %||% "indice_priorite_regen"
      proxy <- leaflet::leafletProxy("map")
      leaflet::clearGroup(proxy, "UGF")
      leaflet::removeControl(proxy, "regen_legend")
      # Pas (encore) de résultat exploitable → garder le contour UGF de base
      # visible plutôt qu'une carte vide.
      if (is.null(res) || !col %in% names(res)) {
        if (!is.null(units)) {
          geo <- tryCatch(sf::st_transform(units, 4326), error = function(e) units)
          leaflet::addPolygons(proxy, data = geo, group = "UGF", weight = 1,
            color = "#1B6B1B", fillColor = "#1B6B1B", fillOpacity = 0.10)
        }
        return()
      }

      vals <- suppressWarnings(as.numeric(res[[col]]))
      geo <- tryCatch(sf::st_transform(res, 4326), error = function(e) res)
      if (all(is.na(vals))) {
        leaflet::addPolygons(proxy, data = geo, group = "UGF", weight = 1,
          color = "#888", fillOpacity = 0.2)
        return()
      }
      # reverse = TRUE : pour les 4 couches, une valeur HAUTE est toujours
      # défavorable (plus vulnérable, plus de jours de stress, canopée moins
      # tamponnante). Sans inversion, RdYlGn peindrait les UG les plus critiques
      # en vert. Rouge = critique, vert = favorable.
      pal <- leaflet::colorNumeric("RdYlGn", domain = vals, na.color = "#cccccc",
                                   reverse = TRUE)
      proxy |>
        leaflet::addPolygons(data = geo, group = "UGF", weight = 1, color = "#333",
          fillColor = pal(vals), fillOpacity = 0.75) |>
        leaflet::addLegend(pal = pal, values = vals, position = "bottomright",
          layerId = "regen_legend",
          title = i18n$t(paste0("regen_map_",
            switch(col, indice_priorite_regen = "priorite", sensibilite = "sensibilite",
                   njstress = "njstress", d_tmax = "dtmax", r7_gel_days = "gel",
                   "priorite"))))
    })

    # Pourquoi la carte de contexte est vide, et quoi faire. Sans ce bandeau,
    # l'onglet affichait un fond de carte nu, sans la moindre explication.
    output$context_status <- shiny::renderUI({
      rv$context_refresh
      project_path <- tryCatch(app_state$current_project$path, error = function(e) NULL)
      if (is.null(project_path)) return(NULL)
      av <- regen_context_availability(project_path)
      if (isTRUE(av$tx) && isTRUE(av$rr)) return(NULL)   # tout est là : pas de bandeau

      # `tx` absent => l'utilisateur n'a jamais lancé « Auto (E-OBS) ».
      msg <- if (!isTRUE(av$tx)) i18n$t("regen_context_need_tx") else i18n$t("regen_context_need_rr")
      htmltools::div(class = "alert alert-warning py-2 small d-flex align-items-center",
        bsicons::bs_icon("exclamation-triangle-fill", class = "me-2"),
        htmltools::div(class = "flex-grow-1", msg),
        if (isTRUE(av$tx))
          bslib::input_task_button(ns("fetch_eobs_rr"), i18n$t("regen_eobs_rr_fetch"),
            icon = bsicons::bs_icon("cloud-download"),
            label_busy = i18n$t("regen_eobs_rr_running_short"),
            type = "outline-primary", class = "btn-sm ms-2")
      )
    })

    # Fonds OSM/Satellite + contrôle de couches + emprise UGF (bleu) + semis
    # E-OBS. Le semis (centres de mailles E-OBS ~11 km, classe bivariée
    # température × précipitation — des POINTS, pas un raster ; l'opacité règle
    # celle des points) est dessiné DANS ce rendu, et non via un proxy séparé :
    # un observer proxy ne se redéclenchait pas à l'activation de l'onglet (ses
    # dépendances stables), laissant la carte vide alors que les deux séries
    # E-OBS étaient bien en cache. Le fitBounds vise l'emprise UGF (stable), donc
    # un changement de buffer/opacité ne déplace pas la vue.
    output$context_map <- leaflet::renderLeaflet({
      units <- units_sf()
      shiny::req(units)
      rv$context_refresh                       # re-rend après acquisition de `rr`
      op <- input$context_opacity %||% 0.8
      project_path <- tryCatch(app_state$current_project$path, error = function(e) NULL)
      geo_ugf <- tryCatch(sf::st_transform(units, 4326), error = function(e) units)
      m <- leaflet::leaflet() |>
        leaflet::addProviderTiles("OpenStreetMap", group = "OSM") |>
        leaflet::addProviderTiles("Esri.WorldImagery", group = "Satellite") |>
        leaflet::addPolygons(data = geo_ugf, group = "UGF", weight = 1.5,
          color = "#1f6feb", fillColor = "#1f6feb", fillOpacity = 0.10) |>
        leaflet::addLayersControl(
          baseGroups    = c("OSM", "Satellite"),
          overlayGroups = c("UGF", "E-OBS"),
          options       = leaflet::layersControlOptions(collapsed = TRUE))
      bb <- tryCatch(as.numeric(sf::st_bbox(geo_ugf)), error = function(e) NULL)
      if (!is.null(bb) && all(is.finite(bb))) {
        m <- leaflet::fitBounds(m, bb[1], bb[2], bb[3], bb[4])
      }
      cells <- regeneration_context_eobs(
        units, precomputed = load_regeneration_precomputed(project_path),
        buffer_m = (input$buffer_km %||% 25) * 1000,
        project_path = project_path)
      if (inherits(cells, "sf") && nrow(cells) > 0) {
        geo <- tryCatch(sf::st_transform(cells, 4326), error = function(e) cells)
        cls <- if ("classe_bivariee" %in% names(cells)) as.numeric(cells$classe_bivariee) else rep(5, nrow(cells))
        pal <- leaflet::colorNumeric("viridis", domain = c(1, 9), na.color = "#ccc")
        m <- leaflet::addCircleMarkers(m, data = geo, group = "E-OBS", radius = 7,
          stroke = TRUE, weight = 1, color = "#333",
          fillColor = pal(cls), fillOpacity = op)
      }
      m
    })

    output$table <- DT::renderDataTable({
      res <- rv$result
      shiny::req(res)
      df <- sf::st_drop_geometry(res)
      cols <- intersect(c("ug_id", "priorite", "indice_priorite_regen", "sensibilite",
        "rang_sensibilite", "njstress", "istress", "deb_stress", "rew_min",
        "d_tmax", "d_vpd", "couverture_pct"), names(df))
      df <- df[, cols, drop = FALSE]
      if (isTRUE(input$filter_coverage) && "couverture_pct" %in% names(df)) {
        keep <- is.na(df$couverture_pct) | df$couverture_pct >= 50
        df <- df[keep, , drop = FALSE]
      }
      order_col <- if ("rang_sensibilite" %in% names(df)) "rang_sensibilite" else NULL
      DT::datatable(df, rownames = FALSE, selection = "single",
        options = list(pageLength = 15, scrollX = TRUE,
          order = if (!is.null(order_col)) list(list(which(names(df) == order_col) - 1, "asc")) else list()))
    })

    output$parcel_sheet <- shiny::renderUI({
      res <- rv$result
      if (is.null(res)) return(htmltools::div(class = "text-muted fst-italic",
                                              i18n$t("regen_need_project")))
      sel <- input$table_rows_selected
      if (is.null(sel) || length(sel) == 0) {
        return(htmltools::div(class = "text-muted fst-italic", i18n$t("regen_select_ug")))
      }
      df <- sf::st_drop_geometry(res)
      row <- df[sel[1], , drop = FALSE]
      fields <- intersect(c("indice_priorite_regen", "sensibilite", "njstress",
        "istress", "deb_stress", "rew_min", "d_tmax", "d_vpd", "couverture_pct"), names(row))
      htmltools::tags$table(class = "table table-sm",
        lapply(fields, function(f) htmltools::tags$tr(
          htmltools::tags$th(i18n$t(paste0("regen_col_",
            switch(f, indice_priorite_regen = "indice", sensibilite = "sensibilite",
              njstress = "njstress", istress = "istress", deb_stress = "deb_stress",
              rew_min = "rew_min", d_tmax = "dtmax", d_vpd = "dvpd",
              couverture_pct = "couverture", f)))),
          htmltools::tags$td(format(row[[f]], digits = 3)))))
    })

    # --- Export GPKG (§7) -------------------------------------------------
    output$export_gpkg <- shiny::downloadHandler(
      filename = function() "regeneration.gpkg",
      content = function(file) {
        res <- rv$result
        if (is.null(res)) {
          shiny::showNotification(i18n$t("regen_export_empty"), type = "warning")
          return(invisible(NULL))
        }
        export_regeneration_geopackage(res, file)
      }
    )

    # --- Persistance versionnée en base (§6A) -----------------------------
    shiny::observeEvent(input$persist_db, {
      if (deny_if_readonly(app_state, i18n)) return()
      res <- rv$result
      if (is.null(res)) {
        shiny::showNotification(i18n$t("regen_export_empty"), type = "warning")
        return()
      }
      if (!is_db_configured()) {
        shiny::showNotification(i18n$t("regen_db_unavailable"), type = "warning")
        return()
      }
      pid <- tryCatch(app_state$current_project$id, error = function(e) NULL)
      ver <- tryCatch({
        con <- get_db_connection()
        on.exit(close_db_connection(con), add = TRUE)
        db_save_regeneration(con, pid, res)
      }, error = function(e) {
        shiny::showNotification(
          sprintf("%s: %s", i18n$t("error"), conditionMessage(e)), type = "error")
        NULL
      })
      if (!is.null(ver)) {
        shiny::showNotification(sprintf(i18n$t("regen_persisted"), ver), type = "message")
      }
    })

    list(result = shiny::reactive(rv$result))
  })
}

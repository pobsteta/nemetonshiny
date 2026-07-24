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

# Exécute le moteur reGénération sous PLAFOND mémoire (cgroup systemd) quand le
# cœur le permet, sinon repli sur l'appel direct dans ce worker (brief 035
# regen-capped). Appelé DANS le worker `future` (nemeton + nemetonshiny chargés).
#
# Anti-OOM : le moteur microclimf+BILJOU tournait dans un worker multisession NU,
# dans le scope de l'app — un pic mémoire postérieur au run faisait tuer RStudio
# par systemd-oomd (incident 2026-07-15). `nemeton::run_memory_capped()` isole le
# heavy dans un enfant sous cgroup plafonné (comme FORDEAD, spec 008).
#
# Garde de CAPACITÉ : la version généralisée de run_memory_capped (arguments
# `package=`/`options=`, nemeton >= 0.158.0) sait lancer une fonction INTERNE
# d'un autre package (`run_regeneration_engine` n'est pas exporté). Tant que le
# cœur installé ne l'expose pas, on retombe proprement sur le chemin nu — l'app
# ne casse pas sur un cœur antérieur. Flag `nemetonshiny.regen_capped` (TRUE par
# défaut) : le passer FALSE force le repli (utile en dev quand la lib installée
# est en retard sur la source pkgload, cf. caveat §1 du brief).
.regen_run_engine_capped <- function(units, project_path, cfg, app_opts) {
  options(nemeton.app_options = app_opts)
  fn <- getFromNamespace("run_regeneration_engine", "nemetonshiny")
  # Même référence pour le test de capacité ET l'appel (cohérence + mockable).
  rmc <- if (requireNamespace("nemeton", quietly = TRUE)) nemeton::run_memory_capped else NULL
  capped_ok <- isTRUE(getOption("nemetonshiny.regen_capped", TRUE)) &&
    !is.null(rmc) &&
    all(c("package", "options") %in% names(formals(rmc)))
  if (!capped_ok) return(fn(units, project_path, cfg))   # repli : appel direct
  rmc(
    fun     = "run_regeneration_engine",
    package = "nemetonshiny",
    args    = list(units = units, project_path = project_path, cfg = cfg),
    options = list(nemeton.app_options = app_opts),
    # memory_max = NULL → défaut cœur (70 % RAM) ; MemorySwapMax=0 déjà géré côté
    # cœur. Le progress reste le canal disque (engine_status.json/engine.log),
    # poll 1 s inchangé : pas de progress_callback nécessaire.
    quiet   = FALSE)
}

# Plage d'un axe de la légende bivariée (Contexte régional E-OBS). Utilise la
# plage RÉELLEMENT observée sur la zone — quantiles du raster downscalé exposés
# par le cœur dans la sous-méta `sub$palette$low/high` — plutôt que les seuils
# fixes de classification. Garantit que 0 (et sa ligne pointillée blanche) reste
# DANS la plage, avec une marge de 10 %, même si toutes les tendances sont du
# même signe. La position fractionnaire du 0 est alors recalculée sur cette plage.
# Repli sur les seuils cœur (`break_vec`) + la position 0 cœur (`core_zero`) quand
# la plage observée est absente (vieux cache, sous-méta manquante).
# Renvoie `list(range = c(lo, hi), zero = frac_0_dans_range)`.
.regen_biv_axis <- function(sub, break_vec, core_zero) {
  lo <- suppressWarnings(as.numeric(sub$palette$low))
  hi <- suppressWarnings(as.numeric(sub$palette$high))
  if (length(lo) == 1L && length(hi) == 1L && is.finite(lo) && is.finite(hi)) {
    if (hi < lo) { t <- lo; lo <- hi; hi <- t }
    pad <- 0.1 * max(hi - lo, abs(lo), abs(hi), 1e-6)   # marge visibilité du 0
    lo <- min(lo, -pad); hi <- max(hi, pad)             # 0 strictement dans [lo, hi]
    return(list(range = c(lo, hi), zero = (0 - lo) / (hi - lo)))
  }
  list(range = break_vec, zero = core_zero)             # repli seuils fixes
}

# Découpe le libellé de la légende bivariée en deux lignes : la partie AVANT la
# première parenthèse (titre principal) + le contenu de la parenthèse finale
# (sous-titre, le couple de variables « T°max estivale × précipitations »).
# Sans parenthèse : titre seul, sous-titre NULL (compat cache/label ancien).
# Renvoie `list(title = ..., subtitle = ...|NULL)`.
.regen_biv_title <- function(raw) {
  raw <- trimws(as.character(raw %||% ""))
  m <- regmatches(raw, regexec("^(.*?)\\s*\\(([^()]*)\\)\\s*$", raw))[[1]]
  if (length(m) == 3L && nzchar(trimws(m[2])) && nzchar(trimws(m[3]))) {
    return(list(title = trimws(m[2]), subtitle = paste0("(", trimws(m[3]), ")")))
  }
  list(title = raw, subtitle = NULL)
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

#' Selectize render bolding species present on the AOI (BDforêt v2)
#'
#' Returns the JS `render` object (as a string, wrap in `I()`) for the target
#' species selectize: any option whose optgroup equals the "present" group label
#' is shown in bold, both in the dropdown and once selected. Lets the user see at
#' a glance where the present species (BDforêt v2) end and the adaptation ones
#' begin. `present_label` is the localized optgroup title used as the marker.
#' @noRd
.species_bold_render <- function(present_label) {
  lbl <- gsub("\\", "\\\\", present_label, fixed = TRUE)
  lbl <- gsub('"', '\\"', lbl, fixed = TRUE)
  fn <- paste0(
    "function(item, escape){",
    "var b = item.optgroup === \"", lbl, "\";",
    "var l = escape(item.label);",
    "return '<div>' + (b ? '<strong>' + l + '</strong>' : l) + '</div>';}")
  paste0("{option: ", fn, ", item: ", fn, "}")
}

#' Archive a rendered reGénération PDF into the project's `exports/` directory
#'
#' Mirrors `.archive_action_plan_pdf` (mod_action_plan.R): a single current PDF
#' per project under `<project_path>/exports/<slug>_regeneration.pdf`. Called
#' only on the success path of `output$export_pdf`. Best-effort: a missing path,
#' an unwritable directory or a failed copy is warned but never aborts the
#' browser download.
#'
#' @param rendered_file Path to the freshly rendered PDF handed to the browser.
#' @param project The current project (list with `path` and `metadata$name`).
#' @return Invisibly `TRUE` when a copy was made, `FALSE` otherwise.
#' @noRd
.archive_regeneration_pdf <- function(rendered_file, project) {
  project_path <- tryCatch(project$path, error = function(e) NULL)
  if (is.null(project_path) || !dir.exists(project_path)) return(invisible(FALSE))
  exports_dir <- file.path(project_path, "exports")
  if (!dir.exists(exports_dir)) {
    dir.create(exports_dir, recursive = TRUE, showWarnings = FALSE)
  }
  export_name <- .project_export_slug(project, "nemeton")
  export_file <- file.path(exports_dir, paste0(export_name, "_regeneration.pdf"))
  tryCatch(
    {
      ok <- file.copy(rendered_file, export_file, overwrite = TRUE)
      invisible(isTRUE(ok))
    },
    error = function(e) {
      cli::cli_warn("reGénération PDF archive failed: {conditionMessage(e)}")
      invisible(FALSE)
    }
  )
}

#' Shiny-safe input key for a per-UGF comment textarea
#'
#' UGF ids may contain characters invalid in an input id. Maps `ug` to a stable
#' token `regcom_<sanitised>`. `input[[key]]` and `ns(key)` share this token.
#' @noRd
.regen_comment_key <- function(ug) {
  paste0("regcom_", gsub("[^A-Za-z0-9]", "_", as.character(ug)))
}

#' Top-N regeneration species ranking UI block for one UGF (spec 039)
#'
#' Renders the deterministic top-N species (from `nemeton::regen_rank_species`,
#' long data.frame) for a single UGF: each species with its suitability 0-100,
#' translated limiting factor and confidence. Returns `NULL` when the ranking is
#' absent or has no row for that UGF (NA/empty-safe). `ug` is the UGF id string.
#' @noRd
.regen_species_ranking_ui <- function(rank_df, ug, i18n) {
  if (is.null(rank_df) || !is.data.frame(rank_df) ||
      !all(c("ug_id", "rank", "label", "suitability") %in% names(rank_df))) {
    return(NULL)
  }
  sub <- rank_df[as.character(rank_df$ug_id) == as.character(ug), , drop = FALSE]
  if (nrow(sub) == 0L) return(NULL)
  sub <- sub[order(sub$rank), , drop = FALSE]
  lf_label <- function(lf) {
    lf <- as.character(lf)
    key <- switch(lf,
      secheresse = "regen_limit_secheresse", gel = "regen_limit_gel",
      chaleur = "regen_limit_chaleur", ombre = "regen_limit_ombre", NA_character_)
    if (is.na(key)) lf else i18n$t(key)
  }
  conf_label <- function(cf) {
    cf <- as.character(cf)
    key <- switch(cf,
      eleve = "regen_conf_eleve", moyen = "regen_conf_moyen",
      faible = "regen_conf_faible", NA_character_)
    if (is.na(key)) cf else i18n$t(key)
  }
  htmltools::div(class = "mt-1 mb-3",
    htmltools::tags$strong(class = "small", i18n$t("regen_species_ranking_title")),
    htmltools::tags$ol(class = "mb-0 ps-3 small",
      lapply(seq_len(nrow(sub)), function(i) {
        s <- sub[i, , drop = FALSE]
        htmltools::tags$li(
          htmltools::tags$span(class = "fw-semibold", as.character(s$label)),
          sprintf(" \u2014 %d/100", round(as.numeric(s$suitability))),
          if (!is.na(s$limiting_factor)) htmltools::tags$span(class = "text-muted",
            sprintf(" \u00b7 %s : %s", i18n$t("regen_species_limiting"),
                    lf_label(s$limiting_factor))),
          if (!is.na(s$confidence)) htmltools::tags$span(class = "text-muted",
            sprintf(" \u00b7 %s", conf_label(s$confidence))))
      })))
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

      # Verrou instantané côté client (brief 035 verrou-boutons, retour UX) :
      # dès qu'un bouton `.regen-calc-btn` est cliqué, TOUS passent `disabled`
      # sans attendre l'aller-retour serveur — supprime la fenêtre où les autres
      # boutons paraissent encore cliquables. Le serveur reste l'autorité :
      # l'observer de verrou ré-active (ou maintient grisé) selon l'état réel.
      # `setTimeout(0)` : laisser l'événement de clic courant enregistrer son
      # input Shiny AVANT de désactiver. Idempotent (garde `__regenCalcLock`).
      htmltools::tags$script(htmltools::HTML(paste0(
        "(function(){",
        "if(window.__regenCalcLock)return;window.__regenCalcLock=true;",
        "document.addEventListener('click',function(e){",
        "var h=e.target.closest('.regen-calc-btn');",
        "if(!h||h.disabled)return;",
        "setTimeout(function(){",
        "document.querySelectorAll('.regen-calc-btn').forEach(function(b){b.disabled=true;});",
        "},0);},false);})();"))),

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
          label_busy = i18n$t("regen_busy_generic"),
          type = "outline-secondary", class = "btn-sm mb-2 regen-calc-btn"),
        i18n$t("regen_year_auto_tip"), placement = "right"),
      shiny::uiOutput(ns("eobs_status")),
      htmltools::tags$hr(class = "my-2"),

      # --- Moteur microclimf réel (opt-in, coûteux) ----------------------
      # Lance le vrai run microclimf (LiDAR HD + ERA5) via nemeton, en async.
      # Désactivé tant que les prérequis (grille LiDAR HD + identifiants CDS)
      # ne sont pas réunis ; le statut est affiché sous le bouton.
      htmltools::tags$small(class = "text-muted d-block mb-1", i18n$t("regen_engine_section")),
      bslib::tooltip(
        bslib::input_task_button(ns("run_engine"), i18n$t("regen_engine_run"),
          icon = bsicons::bs_icon("cpu"),
          label_busy = i18n$t("regen_busy_generic"),
          type = "outline-primary", class = "btn-sm w-100 regen-calc-btn"),
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
            label_busy = i18n$t("regen_busy_generic"),
            type = "outline-primary", class = "btn-sm w-100 regen-calc-btn"),
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
      # Bilan persistant du dernier calcul gel (retour UX) : surtout le cas
      # « aucun jour de gel tardif », sinon un run réussi sans gel semble n'avoir
      # rien produit. Alimenté par l'observer de fin de frost_task.
      shiny::uiOutput(ns("frost_status")),
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
      shiny::radioButtons(ns("forcing"),
        label_tt(i18n$t("regen_forcing"), i18n$t("regen_forcing_tip")),
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
        class = "btn-primary w-100 regen-calc-btn", icon = bsicons::bs_icon("play-fill")),

      # Provenance de la donnée canopée effectivement utilisée (spec 033 D5) :
      # LiDAR HD (PAI structural) ou repli satellite S2/PROSAIL (NDP 0). Lu
      # depuis nemeton::detect_ndp() ; ne s'affiche que si une canopée est utilisée.
      shiny::uiOutput(ns("canopy_provenance")),
      # Valeurs réellement utilisées par le moteur (spec 035 B4.a) : médiane +
      # étendue du lai_max et de la réserve utile par UGF, ou badge « forcé ».
      # Les exports ont migré dans le sidebar droit de Carte+Tableau, sous le
      # panneau « Affiner la reGénération avec l'IA ».
      shiny::uiOutput(ns("derived_stats"))
    ),

    # --- Résultats -------------------------------------------------------
    shiny::uiOutput(ns("status")),
    bslib::navset_card_tab(
      bslib::nav_panel(
        i18n$t("regen_map_table_view"),
        # Pattern « Plan d'actions » : Carte (colonne gauche, pleine hauteur) et,
        # dans la colonne droite, Tableau des UGF (moitié haute) + Fiches parcelles
        # (moitié basse) empilés. Un clic carte sélectionne l'UGF (surlignage + ligne
        # de table), la table pilote la carte en retour (croisé, multi-sélection).
        # Sidebar DROITE : panneau IA repliable « Affiner la reGénération avec l'IA »
        # (pattern « Affiner » du Plan d'actions) — conseil narratif (P2) en surcouche
        # du classement déterministe top-3 (P1).
        bslib::layout_sidebar(
          fillable = FALSE,
          sidebar = bslib::sidebar(
            position = "right", open = "always", width = 340,
            htmltools::tags$div(
              class = "card",
              htmltools::tags$div(
                class = "card-header bg-success text-white py-2",
                style = "cursor: pointer;",
                `data-bs-toggle` = "collapse",
                `data-bs-target` = paste0("#", ns("regen_ai_collapse")),
                `aria-expanded` = "true",
                `aria-controls` = ns("regen_ai_collapse"),
                htmltools::div(
                  class = "d-flex align-items-center justify-content-between",
                  htmltools::div(class = "d-flex align-items-center",
                    bsicons::bs_icon("robot", class = "me-2"),
                    i18n$t("regen_ai_panel_title")),
                  bsicons::bs_icon("chevron-down", class = "collapse-icon"))),
              htmltools::tags$div(
                id = ns("regen_ai_collapse"), class = "collapse show",
                htmltools::tags$div(
                  class = "card-body",
                  htmltools::tags$p(class = "small text-muted",
                    i18n$t("regen_ai_panel_help")),
                  # Conseil(s) affiché(s) en haut (façon historique de chat).
                  shiny::uiOutput(ns("regen_ai_advice")),
                  # Portée : mêmes possibilités que « Affiner le plan avec l'IA ».
                  shiny::radioButtons(ns("regen_ai_scope"),
                    i18n$t("regen_ai_scope"),
                    choices = stats::setNames(
                      c("all", "selected"),
                      c(i18n$t("regen_ai_scope_all"), i18n$t("regen_ai_scope_sel"))),
                    selected = "all", inline = TRUE),
                  shiny::checkboxInput(ns("regen_ai_replace"),
                    i18n$t("regen_ai_replace"), value = TRUE),
                  # Zone de texte libre + exemple en placeholder.
                  shiny::textAreaInput(ns("regen_ai_input"), label = NULL,
                    placeholder = i18n$t("regen_ai_placeholder"),
                    rows = 3, width = "100%", resize = "vertical"),
                  htmltools::div(class = "d-flex gap-2 mt-2",
                    shiny::actionButton(ns("regen_ai_clear"),
                      i18n$t("regen_ai_clear"),
                      icon = shiny::icon("broom"),
                      class = "btn-sm btn-outline-secondary flex-fill"),
                    shiny::actionButton(ns("regen_ai_send"),
                      i18n$t("regen_ai_send"),
                      icon = shiny::icon("paper-plane"),
                      class = "btn-sm btn-primary flex-fill"))))),
            # --- Exports (sous le panneau IA) : regroupés dans un accordéon
            # repliable « Exports » (replié par défaut). Mêmes boutons/toasts que
            # le Plan d'actions ; Envoyer vers Terrain / Enregistrer en base =
            # toast bas-droite au clic (onclick) + gel via .regen-calc-btn.
            bslib::accordion(
              open = FALSE, class = "mt-3",
              bslib::accordion_panel(
                title = i18n$t("action_plan_section_exports"),
                icon = bsicons::bs_icon("box-arrow-up"),
                htmltools::tagAppendAttributes(
                  shiny::actionButton(ns("export_terrain"), i18n$t("regen_export_terrain"),
                    icon = shiny::icon("crosshairs"),
                    class = "btn-outline-success btn-sm w-100 mb-2 regen-calc-btn"),
                  onclick = sprintf("nemetonShowDownloadToast(%s);",
                    jsonlite::toJSON(i18n$t("send_to_field_busy"), auto_unbox = TRUE))),
                htmltools::tagAppendAttributes(
                  shiny::downloadButton(ns("export_gpkg"), i18n$t("regen_download_gpkg"),
                    icon = shiny::icon("database"),
                    class = "btn-outline-success btn-sm w-100 mb-2"),
                  onclick = sprintf("nemetonShowDownloadToast(%s);",
                    jsonlite::toJSON(i18n$t("regen_export_gpkg_busy"), auto_unbox = TRUE))),
                htmltools::tagAppendAttributes(
                  shiny::downloadButton(ns("export_pdf"), i18n$t("regen_download_pdf"),
                    icon = shiny::icon("file-pdf"),
                    class = "btn-outline-success btn-sm w-100 mb-2"),
                  onclick = sprintf("nemetonShowDownloadToast(%s);",
                    jsonlite::toJSON(i18n$t("regen_export_pdf_busy"), auto_unbox = TRUE))),
                htmltools::tags$hr(class = "my-2"),
                htmltools::tagAppendAttributes(
                  shiny::actionButton(ns("persist_db"), i18n$t("save_to_db_button"),
                    icon = bsicons::bs_icon("database"),
                    class = "btn-outline-secondary btn-sm w-100 regen-calc-btn"),
                  onclick = sprintf("nemetonShowDownloadToast(%s);",
                    jsonlite::toJSON(i18n$t("save_to_db_busy"), auto_unbox = TRUE)))))
          ),
        bslib::layout_columns(
          col_widths = c(6, 6), fillable = FALSE,
          # --- Carte (colonne gauche) ------------------------------------------
          # Sidebar DROITE dédiée : le radio « Couche affichée » vit à droite de la
          # carte ; les fonds OSM/Satellite/UGF restent dans le contrôle natif
          # Leaflet (coin carte).
          bslib::card(
            full_screen = TRUE,
            bslib::card_header(i18n$t("regen_map_card_title")),
            bslib::card_body(
              class = "p-0",
              bslib::layout_sidebar(
                fillable = TRUE,
                sidebar = bslib::sidebar(
                  position = "right", open = "always", width = 240,
                  htmltools::tags$strong(i18n$t("regen_map_layer")),
                  # Chaque couche porte un « i » qui explique la variable
                  # cartographiée et comment lire sa légende.
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
                  # Essence cible : re-priorise la choroplèthe en direct (sans
                  # relancer l'analyse). N'affecte QUE la couche « Indice de
                  # priorité » — masquée pour les autres couches (conditionalPanel).
                  shiny::conditionalPanel(
                    condition = "input.map_layer == 'indice_priorite_regen'", ns = ns,
                    htmltools::tags$hr(class = "my-2"),
                    # Les essences PRÉSENTES sur les UGF (BDforêt v2, optgroup en
                    # tête) sont affichées EN GRAS pour distinguer d'emblée la
                    # frontière avec les essences d'adaptation. Le render selectize
                    # met en gras toute option dont l'optgroup == libellé « présentes ».
                    shiny::selectizeInput(ns("species"), label_tt(i18n$t("regen_species_target"),
                        i18n$t("regen_species_tip")),
                      choices = stats::setNames("", i18n$t("regen_species_generic")),
                      options = list(render = I(.species_bold_render(
                        i18n$t("regen_species_group_present"))))))
                ),
                leaflet::leafletOutput(ns("map"), height = "70vh")
              )
            )
          ),
          # --- Colonne droite : Tableau (moitié haute) + Fiches (moitié basse) --
          # Pile verticale calée sur la hauteur de la carte (70vh). Chaque carte
          # prend 50 % (flex:1) et fait défiler son contenu (min-height:0 requis
          # pour qu'un enfant flex puisse rétrécir sous son contenu).
          htmltools::div(
            style = htmltools::css(display = "flex", `flex-direction` = "column",
              height = "70vh", gap = "0.5rem"),
            bslib::card(
              full_screen = TRUE,
              style = "flex:1 1 0; min-height:0;",
              bslib::card_header(
                htmltools::div(
                  class = "d-flex justify-content-between align-items-center flex-wrap gap-2",
                  htmltools::tags$span(i18n$t("regen_table_card_title")),
                  htmltools::div(
                    class = "d-flex align-items-center gap-3",
                    htmltools::div(
                      class = "mb-0",
                      shiny::checkboxInput(ns("filter_coverage"),
                        i18n$t("regen_filter_coverage"), value = TRUE)),
                    shiny::actionButton(ns("clear_selection"),
                      i18n$t("regen_clear_selection"),
                      icon = shiny::icon("eraser"),
                      class = "btn-outline-secondary btn-sm")))),
              bslib::card_body(
                class = "overflow-auto",
                DT::dataTableOutput(ns("table")))
            ),
            bslib::card(
              full_screen = TRUE,
              style = "flex:1 1 0; min-height:0;",
              bslib::card_header(i18n$t("regen_selected_sheets")),
              bslib::card_body(
                class = "overflow-auto",
                shiny::uiOutput(ns("parcel_sheet")))
            )
          )
        )
        )
      ),
      bslib::nav_panel(i18n$t("regen_map_context"),
        # Contexte régional en RASTER downscalé (eobs_downscale, cœur >= 0.153.0),
        # 3 vues : tendance T°max (tx), tendance précipitations (rr), et croisement
        # BIVARIÉ (classes 1-9). La sidebar droite porte le sélecteur de vue, le
        # rayon (buffer) et l'opacité — tout ce qui ne concerne que cette carte.
        bslib::layout_sidebar(
          fillable = TRUE,
          sidebar = bslib::sidebar(
            position = "right", open = "always", width = 260,
            htmltools::tags$strong(i18n$t("regen_context_view")),
            shiny::radioButtons(ns("context_view"), NULL,
              choiceValues = c("tx", "rr", "bivariate"),
              # Chaque vue porte un « i » expliquant la variable et la lecture.
              choiceNames  = list(
                layer_tt(i18n$t("regen_context_view_tx"), i18n$t("regen_context_view_tx_info")),
                layer_tt(i18n$t("regen_context_view_rr"), i18n$t("regen_context_view_rr_info")),
                layer_tt(i18n$t("regen_context_view_bivariate"), i18n$t("regen_context_view_bivariate_info"))),
              selected = "tx"),
            # Téléchargement de la série précipitations (~800 Mo) : requis pour les
            # vues rr / bivariée.
            bslib::input_task_button(ns("fetch_eobs_rr"), i18n$t("regen_eobs_rr_fetch"),
              icon = bsicons::bs_icon("cloud-download"),
              label_busy = i18n$t("regen_busy_generic"),
              type = "outline-secondary", class = "btn-sm w-100 mb-2 regen-calc-btn"),
            # Température moyenne (tg) : requise par le diagramme ombrothermique
            # (Gaussen) du panneau au clic. Opt-in, même coût (~800 Mo) que rr.
            bslib::input_task_button(ns("fetch_eobs_tg"), i18n$t("regen_fetch_tg"),
              icon = bsicons::bs_icon("thermometer-half"),
              label_busy = i18n$t("regen_busy_generic"),
              type = "outline-secondary", class = "btn-sm w-100 mb-2 regen-calc-btn"),
            htmltools::tags$hr(class = "my-2"),
            shiny::numericInput(ns("buffer_km"), i18n$t("regen_buffer"),
              value = 25, min = 5, max = 100, step = 5),
            htmltools::tags$p(class = "text-muted small", i18n$t("regen_buffer_help")),
            htmltools::tags$strong(i18n$t("regen_context_opacity")),
            shiny::sliderInput(ns("context_opacity"), NULL,
              min = 0, max = 1, value = 0.8, step = 0.05, ticks = FALSE),
            # Purge le cache raster (.tif + meta) des 3 vues et relance le calcul
            # de la vue courante : à utiliser quand un raster caché est périmé
            # (ancienne version cœur, bornes/classement changés).
            htmltools::tags$hr(class = "my-2"),
            shiny::actionButton(ns("recompute_context"), i18n$t("regen_context_recompute"),
              icon = bsicons::bs_icon("arrow-repeat"),
              class = "btn-outline-warning btn-sm w-100")
          ),
          shiny::uiOutput(ns("context_status")),
          leaflet::leafletOutput(ns("context_map"), height = "70vh")
        ))
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
      # Incrémenté à chaque clic sur un bouton de calcul : force l'observer de
      # verrou à re-synchroniser l'état des boutons (verrou instantané côté client).
      click_tick = 0L,
      # Chrono des boutons async (spec 027 feedback) : instant de départ, remis
      # à NULL à la fin de la tâche. NULL = pas de run en cours.
      engine_running = FALSE, eobs_running = FALSE,
      engine_start = NULL, eobs_start = NULL,
      # Chrono du téléchargement des précipitations E-OBS (~800 Mo) : notif
      # persistante « engrenage + MM:SS » tant que la tâche tourne.
      eobs_rr_running = FALSE, eobs_rr_start = NULL,
      # Idem pour la T° moyenne (tg) — requise par le diagramme ombrothermique.
      eobs_tg_running = FALSE, eobs_tg_start = NULL,
      # Série au dernier point cliqué sur la carte de contexte (4 graphes E-OBS).
      click_series = NULL,
      # Chrono du moteur « risque de gel tardif » (R7, meteoland).
      frost_running = FALSE, frost_start = NULL,
      # Bilan persistant du dernier calcul gel (status none/detected/skipped/error
      # + stats) affiché sous le bouton « Risque de gel » — retour UX.
      frost_summary = NULL,
      # Raster de contexte régional (E-OBS downscalé) + son meta (status, palette,
      # value_label) ; chrono du calcul async (WMS IGN + krigeage, ~4 s).
      context_raster = NULL, context_meta = NULL,
      context_running = FALSE, context_start = NULL,
      context_loaded_view = NULL,   # vue actuellement chargée : tx / rr / bivariate
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
      # Raster de contexte : périmé au changement de projet/UGF → recalcul lazy.
      rv$context_raster <- NULL
      rv$context_meta <- NULL
      rv$context_loaded_view <- NULL
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
        shiny::updateSelectizeInput(session, "species",
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
      shiny::updateSelectizeInput(session, "species", choices = choices,
                                  selected = shiny::isolate(input$species) %||% "")
    })

    # Auto-detect the average / heatwave years from E-OBS for this AOI.
    # Async : l'acquisition E-OBS (CDS) est déléguée à un worker future (cf.
    # eobs_task) ; le résultat met à jour les deux champs Année.
    shiny::observeEvent(input$auto_years, {
      if (deny_if_busy()) return()
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
      if (isTRUE(rv$eobs_running)) {
        shiny::invalidateLater(1000)
        return(htmltools::div(class = "small text-info mt-1",
          bsicons::bs_icon("hourglass-split", class = "me-1"),
          i18n$t("regen_auto_running_short"),
          htmltools::tags$span(class = "ms-1 font-monospace",
                               .fmt_elapsed(rv$eobs_start))))
      }
      # Détection terminée : confirmation persistante sous le bouton (le toast
      # regen_auto_done ne dure que 6 s). Rappelle les millésimes retenus.
      if (is.null(rv$eobs)) return(NULL)
      htmltools::div(class = "small text-success mt-1",
        bsicons::bs_icon("check-circle", class = "me-1"),
        sprintf(i18n$t("regen_auto_done"),
                rv$eobs$year_moyenne %||% "?", rv$eobs$year_canicule %||% "?"))
    })

    # --- Run --------------------------------------------------------------
    shiny::observeEvent(input$run, {
      if (deny_if_busy()) return()
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
        # Reporter R7 (gel) du résultat précédent (brief 035 §8) : run_regeneration()
        # ne produit pas r7_gel_days (pas de tmin en entrée) et écrasait sèchement
        # rv$result, vidant la couche gel dès qu'on relançait l'analyse après R7.
        # `.regen_attach_r7` réattache par ug_id (no-op si aucun R7 antérieur).
        carried <- .regen_attach_r7(res$units, shiny::isolate(rv$result))
        rv$result <- carried
        rv$years <- res$years
        rv$warnings <- res$warnings %||% character(0)
        # Provenance canopée lue sur le résultat (repli satellite / LiDAR HD).
        rv$canopy_source <- regen_canopy_provenance(res$units)
        # Publier le résultat pour l'export PDF (section reGénération de mod_synthesis).
        app_state$regeneration_result <- carried
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
            .ensure_async_plan()
          }
        }
        promises::future_promise({
          # spec 008 §4 — le worker (superviseur) est PERSISTANT : lui rendre sa
          # mémoire. Avec le chemin capé, le heavy vit dans un ENFANT cgroup ; ce
          # worker ne fait que bloquer dessus (peu de RAM).
          on.exit(nemetonshiny:::.release_worker_memory(), add = TRUE)
          if (!is.null(dev_path) && requireNamespace("pkgload", quietly = TRUE)) {
            pkgload::load_all(dev_path, quiet = TRUE)
          } else {
            loadNamespace("nemetonshiny")
          }
          nemetonshiny:::.regen_run_engine_capped(units, project_path, cfg, app_opts)
        }, seed = TRUE)
      })

    # Auto E-OBS async : l'acquisition E-OBS (CDS, même clé qu'ERA5) peut être
    # lente / mise en file → worker future, ne renvoie que la paire d'années.
    eobs_task <- shiny::ExtendedTask$new(
      function(units, project_path, dev_path, app_opts) {
        if (requireNamespace("future", quietly = TRUE)) {
          plan_classes <- class(future::plan())
          if (!any(c("multisession", "multicore", "cluster") %in% plan_classes)) {
            .ensure_async_plan()
          }
        }
        promises::future_promise({
          # spec 008 §4 — le worker est PERSISTANT : lui rendre sa memoire.
          on.exit(nemetonshiny:::.release_worker_memory(), add = TRUE)
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
            .ensure_async_plan()
          }
        }
        promises::future_promise({
          # spec 008 §4 — le worker est PERSISTANT : lui rendre sa memoire.
          on.exit(nemetonshiny:::.release_worker_memory(), add = TRUE)
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

    # Acquisition de la T° moyenne (tg) pour le diagramme ombrothermique — même
    # patron que rr (opt-in, worker future, ~800 Mo, cache .nc).
    eobs_tg_task <- shiny::ExtendedTask$new(
      function(units, project_path, dev_path, app_opts) {
        if (requireNamespace("future", quietly = TRUE)) {
          plan_classes <- class(future::plan())
          if (!any(c("multisession", "multicore", "cluster") %in% plan_classes)) {
            .ensure_async_plan()
          }
        }
        promises::future_promise({
          on.exit(nemetonshiny:::.release_worker_memory(), add = TRUE)
          if (!is.null(dev_path) && requireNamespace("pkgload", quietly = TRUE)) {
            pkgload::load_all(dev_path, quiet = TRUE)
          } else {
            loadNamespace("nemetonshiny")
          }
          options(nemeton.app_options = app_opts)
          fn <- getFromNamespace("regen_fetch_eobs_tg", "nemetonshiny")
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
            .ensure_async_plan()
          }
        }
        promises::future_promise({
          # spec 008 §4 — le worker est PERSISTANT : lui rendre sa memoire.
          on.exit(nemetonshiny:::.release_worker_memory(), add = TRUE)
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

    # Raster de contexte régional (E-OBS downscalé, ~4 s WMS + krigeage) : worker
    # future, sortie cachée (.tif + meta.json). Renvoie le CHEMIN du raster (le
    # pointeur SpatRaster ne traverse pas la frontière future) + le meta.
    context_task <- shiny::ExtendedTask$new(
      function(units, project_path, view, buffer_m, dev_path, app_opts) {
        if (requireNamespace("future", quietly = TRUE)) {
          plan_classes <- class(future::plan())
          if (!any(c("multisession", "multicore", "cluster") %in% plan_classes)) {
            .ensure_async_plan()
          }
        }
        promises::future_promise({
          # spec 008 §4 — le worker est PERSISTANT : lui rendre sa memoire.
          on.exit(nemetonshiny:::.release_worker_memory(), add = TRUE)
          if (!is.null(dev_path) && requireNamespace("pkgload", quietly = TRUE)) {
            pkgload::load_all(dev_path, quiet = TRUE)
          } else {
            loadNamespace("nemetonshiny")
          }
          options(nemeton.app_options = app_opts)
          fn <- getFromNamespace("run_regeneration_context_raster", "nemetonshiny")
          fn(units, project_path, view = view, buffer_m = buffer_m)
        }, seed = TRUE)
      })

    # Lier chaque input_task_button à sa tâche : bslib désactive le bouton +
    # affiche le spinner tant que la tâche tourne, puis le réactive (succès OU
    # erreur). Empêche les runs concurrents (réécriture sensibilite/biljou.gpkg).
    # `bind_task_button()` prend l'id local au module (sans ns()).
    bslib::bind_task_button(engine_task, "run_engine")
    bslib::bind_task_button(eobs_task, "auto_years")
    bslib::bind_task_button(eobs_rr_task, "fetch_eobs_rr")
    bslib::bind_task_button(eobs_tg_task, "fetch_eobs_tg")
    bslib::bind_task_button(frost_task, "run_frost")

    # --- Verrou d'exclusion mutuelle des calculs (brief 035 verrou-boutons) ---
    # Chaque task button ne connaît que SA tâche (bind_task_button) : rien
    # n'empêche de lancer un 2e calcul lourd pendant qu'un 1er tourne (workers
    # future concurrents sur les mêmes rasters/RAM/cache/regeneration). `busy()`
    # est vrai dès qu'une tâche UTILISATEUR tourne. `context_task` est EXCLU : il
    # est auto-déclenché par un changement de vue/buffer et griserait la sidebar
    # de façon erratique sans que l'utilisateur ait rien lancé.
    busy <- shiny::reactive({
      any(vapply(
        list(engine_task, eobs_task, eobs_rr_task, eobs_tg_task, frost_task),
        function(t) identical(t$status(), "running"),
        logical(1)))
    })

    # Grisage client : task buttons via update_task_button (le binding bslib
    # ignore la clé `disabled`), actionButton classiques via updateActionButton.
    TASK_BTNS   <- c("run_engine", "auto_years", "fetch_eobs_rr", "fetch_eobs_tg", "run_frost")
    # `export_terrain` porte `.regen-calc-btn` (le verrou client le grise au clic)
    # mais n'est ni un task button ni un moteur : il DOIT figurer ici, sinon
    # l'observer d'autorité ne le ré-active jamais et il reste grisé à vie.
    ACTION_BTNS <- c("run", "recompute_pai", "persist_db", "export_terrain")
    # meteoland absent → run_frost est rendu désactivé : ne jamais le repasser
    # « ready » (sinon l'observer l'activerait alors qu'il ne doit pas l'être).
    frost_ok <- regen_meteoland_available()

    # Grisage instantané côté client (JS délégué, cf. UI) : au clic, TOUS les
    # boutons de calcul passent `disabled` sans attendre l'aller-retour serveur.
    # Cet observer est la source d'autorité qui RÉ-active — dès que le verrou
    # retombe. Il dépend de `rv$click_tick` pour se ré-exécuter APRÈS chaque clic,
    # y compris un clic qui n'a rien lancé (pas de projet, prérequis KO,
    # lecture seule…) : sans ça, le grisage client resterait collé.
    shiny::observe({
      rv$click_tick                                   # dépendance : re-sync post-clic
      locked <- isTRUE(busy()) || isTRUE(rv$running)
      for (btn in TASK_BTNS) {
        if (identical(btn, "run_frost") && !frost_ok) next
        bslib::update_task_button(btn, state = if (locked) "busy" else "ready")
      }
      for (btn in ACTION_BTNS) {
        shiny::updateActionButton(session, btn, disabled = locked)
      }
    })

    # Bump du compteur au moindre clic sur un bouton de calcul : force l'observer
    # ci-dessus à recalculer l'état d'autorité (re-active ce que le JS a grisé si
    # le clic n'a finalement lancé aucun calcul).
    shiny::observeEvent(
      list(input$run_engine, input$run_frost, input$auto_years,
           input$fetch_eobs_rr, input$fetch_eobs_tg, input$run,
           input$recompute_pai, input$persist_db, input$export_terrain), {
        rv$click_tick <- (rv$click_tick %||% 0L) + 1L
      }, ignoreInit = TRUE)

    # Garde serveur (la partie robuste) : le grisage client est contournable
    # (double-clic rapide, race websocket, page rechargée avec un état périmé).
    # En tête de chaque observeEvent de déclencheur. Ne remet PAS le task button
    # « ready » : le laisser grisé est cohérent avec le verrou ; l'observer
    # ci-dessus le réactive quand busy() repasse FALSE (fin de la tâche active).
    deny_if_busy <- function() {
      if (!isTRUE(busy())) return(FALSE)
      shiny::showNotification(i18n$t("regen_busy_already"), type = "warning", duration = 5)
      TRUE
    }

    shiny::observeEvent(input$fetch_eobs_rr, {
      if (deny_if_busy()) return()
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

    # --- Acquisition T° moyenne (tg) pour le diagramme ombrothermique --------
    shiny::observeEvent(input$fetch_eobs_tg, {
      if (deny_if_busy()) return()
      if (deny_if_readonly(app_state, i18n)) {
        bslib::update_task_button("fetch_eobs_tg", state = "ready")
        return()
      }
      units <- units_sf()
      project_path <- tryCatch(app_state$current_project$path, error = function(e) NULL)
      if (is.null(units) || is.null(project_path)) {
        bslib::update_task_button("fetch_eobs_tg", state = "ready")
        shiny::showNotification(i18n$t("regen_need_project"), type = "warning")
        return()
      }
      rv$eobs_tg_running <- TRUE
      rv$eobs_tg_start <- Sys.time()
      shiny::showNotification(
        .running_notif_content(i18n$t("regen_fetch_tg_running"), rv$eobs_tg_start),
        id = session$ns("eobs_tg_notif"), type = "message", duration = NULL)
      eobs_tg_task$invoke(units, project_path, .dev_pkg_path, get_app_options())
    })

    shiny::observe({
      if (!isTRUE(rv$eobs_tg_running)) return()
      shiny::invalidateLater(1000)
      shiny::showNotification(
        .running_notif_content(i18n$t("regen_fetch_tg_running"),
                               shiny::isolate(rv$eobs_tg_start)),
        id = session$ns("eobs_tg_notif"), type = "message", duration = NULL)
    })

    shiny::observeEvent(eobs_tg_task$status(), {
      st <- eobs_tg_task$status()
      if (identical(st, "success") || identical(st, "error")) {
        rv$eobs_tg_running <- FALSE
        rv$eobs_tg_start <- NULL
        shiny::removeNotification(session$ns("eobs_tg_notif"))
      }
      if (identical(st, "success")) {
        ok <- tryCatch(eobs_tg_task$result(), error = function(e) FALSE)
        shiny::showNotification(
          i18n$t(if (isTRUE(ok)) "regen_fetch_tg_done" else "regen_fetch_tg_failed"),
          type = if (isTRUE(ok)) "message" else "warning", duration = if (isTRUE(ok)) 6 else 10)
      } else if (identical(st, "error")) {
        err <- tryCatch(eobs_tg_task$result(), error = function(e) conditionMessage(e))
        shiny::showNotification(
          sprintf("%s: %s", i18n$t("error"), .strip_ansi(as.character(err))),
          type = "error", duration = 10)
      }
    })

    # --- Moteur « risque de gel tardif » (R7, meteoland) ------------------
    shiny::observeEvent(input$run_frost, {
      if (deny_if_busy()) return()
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
            # Bilan persistant sous le bouton (retour UX) : médiane/étendue des
            # jours de gel tardif par UGF, ou « aucun gel détecté » si tout est nul.
            vals <- suppressWarnings(as.numeric(res$units$r7_gel_days))
            vals <- vals[is.finite(vals)]
            rv$frost_summary <- if (length(vals) && max(vals) > 0) {
              f <- function(v) format(round(v, 1), trim = TRUE, nsmall = 1)
              list(status = "detected", stats = list(
                median = f(stats::median(vals)), min = f(min(vals)),
                max = f(max(vals)), n = length(vals)))
            } else {
              list(status = "none")
            }
            shiny::showNotification(i18n$t("regen_frost_done"), type = "message", duration = 6)
          } else {
            # Tmin indisponible (meteoland/SAFRAN KO, < N stations) : R7 skip,
            # jamais un crash — l'utilisateur sait pourquoi le radar n'a pas R7.
            rv$frost_summary <- list(status = "skipped")
            shiny::showNotification(i18n$t("regen_frost_skipped"), type = "warning", duration = 10)
          }
        }
      } else if (identical(st, "error")) {
        rv$frost_summary <- list(status = "error")
        err <- tryCatch(frost_task$result(), error = function(e) conditionMessage(e))
        shiny::showNotification(
          sprintf("%s: %s", i18n$t("error"), .strip_ansi(as.character(err))),
          type = "error", duration = 10)
      }
    })

    # Bilan persistant du dernier calcul gel, sous le bouton « Risque de gel »
    # (retour UX). Le cas « aucun gel » est le plus important — un run réussi
    # sans gélée tardive semblait sinon n'avoir rien produit. Rien tant qu'aucun
    # calcul n'a été lancé, ni pendant le run (la notif bas-droite suffit).
    output$frost_status <- shiny::renderUI({
      if (isTRUE(rv$frost_running)) return(NULL)
      s <- rv$frost_summary
      if (is.null(s)) return(NULL)
      line <- function(cls, icon, body) htmltools::div(
        class = sprintf("small %s mt-1", cls),
        bsicons::bs_icon(icon, class = "me-1"), body)
      switch(s$status %||% "",
        "none" = line("text-success", "check-circle", i18n$t("regen_frost_none_detected")),
        "detected" = {
          st <- s$stats
          if (is.null(st)) return(NULL)
          line("text-warning", "thermometer-snow",
               sprintf(i18n$t("regen_frost_detected_stats"),
                       st$median, st$min, st$max, st$n))
        },
        "skipped" = line("text-muted", "dash-circle", i18n$t("regen_frost_skipped")),
        NULL)
    })

    # --- Contexte régional (raster E-OBS downscalé) ----------------------
    # Calcul lazy quand l'onglet reGénération est actif et que la série tx est là.
    # Fast-path cache (.tif + meta.json) → rendu instantané ; sinon worker async
    # (~4 s WMS + krigeage). Une seule tentative par projet (rv$context_raster).
    # Déclenché aussi par le SÉLECTEUR de vue (tx / rr / bivariate) : chaque vue a
    # son propre raster + cache. Cache-first, sinon worker async. Bandeau
    # need_tx / need_rr si une série requise manque.
    shiny::observeEvent(
      list(app_state$active_main_tab, rv$context_refresh, units_sf(), input$context_view), {
      if (!identical(app_state$active_main_tab, "regeneration")) return()
      if (isTRUE(rv$context_running)) return()
      view <- input$context_view %||% "tx"
      # Déjà chargé pour CETTE vue → ne rien refaire.
      if (identical(rv$context_loaded_view, view) &&
          (!is.null(rv$context_raster) ||
           (!is.null(rv$context_meta) && !identical(rv$context_meta$status, "ok")))) return()
      units <- units_sf(); if (is.null(units)) return()
      project_path <- tryCatch(app_state$current_project$path, error = function(e) NULL)
      if (is.null(project_path)) return()

      av <- regen_context_availability(project_path)
      set_state <- function(rast, meta) {
        rv$context_raster <- rast; rv$context_meta <- meta; rv$context_loaded_view <- view
      }
      if (view %in% c("tx", "bivariate") && !isTRUE(av$tx)) return(set_state(NULL, list(status = "need_tx")))
      if (view %in% c("rr", "bivariate") && !isTRUE(av$rr)) return(set_state(NULL, list(status = "need_rr")))

      cached <- regeneration_context_cached(project_path, view)
      if (!is.null(cached)) return(set_state(cached$raster, cached$meta))

      rv$context_raster <- NULL; rv$context_loaded_view <- view
      rv$context_running <- TRUE; rv$context_start <- Sys.time()
      shiny::showNotification(
        .running_notif_content(i18n$t("regen_context_computing"), rv$context_start),
        id = session$ns("context_notif"), type = "message", duration = NULL)
      context_task$invoke(units, project_path, view, (input$buffer_km %||% 25) * 1000,
                          .dev_pkg_path, get_app_options())
    }, ignoreNULL = FALSE)

    # Recalcul forcé du contexte : purge le cache disque (.tif + meta) des 3 vues
    # — sinon l'observer lazy relit un raster périmé — puis re-déclenche le calcul
    # de la vue courante. Les autres vues se recalculeront à leur prochaine
    # sélection (cache absent). Indispensable après un changement cœur (classement
    # bivarié, bornes) qui rend un raster caché incorrect.
    shiny::observeEvent(input$recompute_context, {
      if (deny_if_busy()) return()
      if (deny_if_readonly(app_state, i18n)) return()
      project_path <- tryCatch(app_state$current_project$path, error = function(e) NULL)
      if (is.null(project_path)) {
        shiny::showNotification(i18n$t("regen_need_project"), type = "warning")
        return()
      }
      for (v in c("tx", "rr", "bivariate")) {
        p <- .regen_context_raster_paths(project_path, v)
        unlink(c(p$tif, p$meta))
      }
      rv$context_raster <- NULL; rv$context_meta <- NULL; rv$context_loaded_view <- NULL
      rv$context_refresh <- (rv$context_refresh %||% 0L) + 1L   # re-déclenche l'observer lazy
      shiny::showNotification(i18n$t("regen_context_recomputing"), type = "message", duration = 4)
    })

    # Tick 1 s : chrono de la notif « contexte régional ».
    shiny::observe({
      if (!isTRUE(rv$context_running)) return()
      shiny::invalidateLater(1000)
      shiny::showNotification(
        .running_notif_content(i18n$t("regen_context_computing"),
                               shiny::isolate(rv$context_start)),
        id = session$ns("context_notif"), type = "message", duration = NULL)
    })

    shiny::observeEvent(context_task$status(), {
      st <- context_task$status()
      if (identical(st, "success") || identical(st, "error")) {
        rv$context_running <- FALSE
        rv$context_start <- NULL
        shiny::removeNotification(session$ns("context_notif"))
      }
      if (identical(st, "success")) {
        res <- tryCatch(context_task$result(), error = function(e) NULL)
        rv$context_meta <- res$meta %||% list(status = "insufficient_data")
        if (!is.null(res$cache_path) && !is.na(res$cache_path) && file.exists(res$cache_path)) {
          rv$context_raster <- tryCatch(terra::rast(res$cache_path), error = function(e) NULL)
        }
      } else if (identical(st, "error")) {
        rv$context_meta <- list(status = "insufficient_data", reason = "eobs_downscale_no_dem")
      }
    })

    # Invalidation manuelle du cache PAI (§3 brief pai-cache) : supprime
    # cache/regeneration/pai.tif → le prochain run recalcule la structure de
    # végétation depuis le nuage LiDAR. Sans effet si le fichier n'existe pas.
    shiny::observeEvent(input$recompute_pai, {
      if (deny_if_busy()) return()
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
      if (deny_if_busy()) return()
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
            # Reporter R7 (gel) du résultat précédent (brief 035 §8) : le re-run
            # fast-path post-moteur n'a pas de tmin → sans report, il vidait la
            # couche gel calculée avant le moteur.
            carried <- .regen_attach_r7(res$units, shiny::isolate(rv$result))
            rv$result <- carried
            rv$years <- res$years
            app_state$regeneration_result <- carried
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
          layerId = if ("ug_id" %in% names(geo)) geo$ug_id else NULL,
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
            layerId = if ("ug_id" %in% names(geo)) geo$ug_id else NULL,
            color = "#1B6B1B", fillColor = "#1B6B1B", fillOpacity = 0.10)
        }
        return()
      }

      vals <- suppressWarnings(as.numeric(res[[col]]))
      geo <- tryCatch(sf::st_transform(res, 4326), error = function(e) res)
      if (all(is.na(vals))) {
        leaflet::addPolygons(proxy, data = geo, group = "UGF", weight = 1,
          layerId = if ("ug_id" %in% names(geo)) geo$ug_id else NULL,
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
          layerId = if ("ug_id" %in% names(geo)) geo$ug_id else NULL,
          fillColor = pal(vals), fillOpacity = 0.75) |>
        leaflet::addLegend(pal = pal, values = vals, position = "bottomright",
          layerId = "regen_legend",
          title = i18n$t(paste0("regen_map_",
            switch(col, indice_priorite_regen = "priorite", sensibilite = "sensibilite",
                   njstress = "njstress", d_tmax = "dtmax", r7_gel_days = "gel",
                   "priorite"))))
    })

    # --- Sélection croisée carte <-> tableau (pattern « Plan d'actions ») -----
    # `selected_ug_rv` (vecteur d'ug_id) est l'UNIQUE source de vérité de la
    # sélection. Un clic carte et une sélection de lignes la mettent à jour ;
    # l'overlay bleu et la/les fiche(s) parcelle en découlent. `reactiveVal`
    # dédoublonne par identical(), donc le round-trip (carte -> rv -> table ->
    # retour) ne boucle pas.
    selected_ug_rv <- shiny::reactiveVal(character())

    # Classement top-3 des essences de régénération par UGF (spec 039, cœur
    # `regen_rank_species` >= 0.162.0). Calculé une fois par résultat ; la fiche
    # parcelle le filtre par UGF sélectionnée. Déterministe (tolérances écologiques
    # vs conditions de station), le narratif IA viendra en surcouche (P2).
    species_ranking <- shiny::reactive({
      res <- rv$result
      if (is.null(res)) return(NULL)
      regeneration_species_ranking(res, top_n = 3L)
    })

    # --- Conseil de régénération par IA (spec 039, P2) -----------------------
    # Surcouche narrative du classement d\u00e9terministe : le LLM ne CLASSE pas, il
    # met en prose le top-3 + les conditions de station, via le profil FIG\u00c9
    # `regeneration` (comme le Plan d'actions fige `planificateur`). Panneau
    # \u00ab Affiner \u00bb : consigne libre + Port\u00e9e (toutes / s\u00e9lection) + case Remplacer.
    # L'historique est une liste de blocs {q, a} rendus empil\u00e9s.
    regen_ai_hist <- shiny::reactiveVal(list())

    shiny::observeEvent(input$regen_ai_clear, {
      regen_ai_hist(list())
      shiny::updateTextAreaInput(session, "regen_ai_input", value = "")
    })

    shiny::observeEvent(input$regen_ai_send, {
      provider <- get_app_config("llm_provider", "anthropic")
      key_var <- get_llm_api_key_var(provider)
      if (!is.null(key_var) && nchar(Sys.getenv(key_var)) == 0) {
        shiny::showNotification(
          gsub("\\{key_var\\}", key_var, i18n$t("ai_no_api_key")),
          type = "warning", duration = 8)
        return()
      }
      ranking <- species_ranking()
      res <- rv$result
      if (is.null(ranking) || is.null(res)) {
        shiny::showNotification(i18n$t("regen_ai_need_ranking"), type = "warning")
        return()
      }
      # Port\u00e9e : \u00ab selected \u00bb restreint aux UGF s\u00e9lectionn\u00e9es ; \u00ab all \u00bb = toutes.
      scope <- input$regen_ai_scope %||% "all"
      sel <- selected_ug_rv()
      if (identical(scope, "selected")) {
        if (length(sel) == 0L) {
          shiny::showNotification(i18n$t("regen_ai_need_selection"), type = "warning")
          return()
        }
        ranking <- ranking[as.character(ranking$ug_id) %in% sel, , drop = FALSE]
      }
      if (nrow(ranking) == 0L) {
        shiny::showNotification(i18n$t("regen_ai_need_ranking"), type = "warning")
        return()
      }
      question <- trimws(input$regen_ai_input %||% "")
      language <- if (identical(i18n$language, "fr")) "fran\u00e7ais" else "English"
      shiny::updateActionButton(session, "regen_ai_send",
        label = i18n$t("ai_generating"),
        icon = shiny::icon("spinner", class = "fa-spin"))
      notif_id <- shiny::showNotification(
        htmltools::div(shiny::icon("spinner", class = "fa-spin me-2"),
                       i18n$t("ai_generating")),
        type = "message", duration = NULL)
      tryCatch({
        system_prompt <- build_system_prompt(language, expert = "regeneration")
        prompt <- build_regen_advice_prompt(ranking, res, language, question = question)
        chat <- create_llm_chat(system_prompt)
        resp <- as.character(chat$chat(prompt, echo = FALSE))
        entry <- list(q = question, a = resp)
        # Case coch\u00e9e -> remplace l'affichage ; d\u00e9coch\u00e9e -> ajoute en dessous.
        if (isTRUE(input$regen_ai_replace)) {
          regen_ai_hist(list(entry))
        } else {
          regen_ai_hist(c(regen_ai_hist(), list(entry)))
        }
        shiny::updateTextAreaInput(session, "regen_ai_input", value = "")
        shiny::removeNotification(notif_id)
      }, error = function(e) {
        shiny::removeNotification(notif_id)
        shiny::showNotification(
          paste(i18n$t("ai_error"), ":", strip_ansi(conditionMessage(e))),
          type = "error", duration = 8)
      })
      shiny::updateActionButton(session, "regen_ai_send",
        label = i18n$t("regen_ai_send"), icon = shiny::icon("paper-plane"))
    })

    output$regen_ai_advice <- shiny::renderUI({
      hist <- regen_ai_hist()
      if (length(hist) == 0L) {
        return(htmltools::div(class = "text-muted small fst-italic",
          i18n$t("regen_ai_advice_empty")))
      }
      htmltools::div(class = "regen-ai-advice small",
        lapply(hist, function(e) {
          htmltools::tagList(
            if (nzchar(e$q %||% "")) htmltools::tags$p(
              class = "fw-bold mb-1", e$q),
            shiny::markdown(e$a %||% ""),
            htmltools::tags$hr(class = "my-2"))
        }))
    })

    # Source UNIQUE du tableau : mêmes lignes et même ordre pour le rendu DT, le
    # mapping clic->ligne et la/les fiche(s) parcelle. Le filtre couverture agit
    # ici -> les index de lignes DT restent cohérents avec les fiches.
    regen_table_df <- shiny::reactive({
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
      df
    })

    # Clic carte -> toggle l'UGF dans la sélection + surligne la/les ligne(s)
    # correspondante(s) du tableau (DT::selectRows). L'overlay bleu et les fiches
    # suivent via `selected_ug_rv`.
    shiny::observeEvent(input$map_shape_click, {
      click <- input$map_shape_click
      if (is.null(click$id)) return()
      uid <- as.character(click$id)
      cur <- selected_ug_rv()
      cur <- if (uid %in% cur) setdiff(cur, uid) else c(cur, uid)
      selected_ug_rv(cur)
      df <- regen_table_df()
      if (is.null(df) || nrow(df) == 0L) return()
      rows <- which(as.character(df$ug_id) %in% cur)
      DT::selectRows(DT::dataTableProxy("table"),
        if (length(rows) > 0L) rows else NULL)
    })

    # Sélection de lignes -> met à jour la source de vérité (sens tableau ->
    # carte). ignoreNULL = FALSE pour capter aussi la désélection totale.
    shiny::observeEvent(input$table_rows_selected, {
      df <- regen_table_df()
      sel <- input$table_rows_selected
      ugs <- if (is.null(sel) || is.null(df)) character() else as.character(df$ug_id[sel])
      selected_ug_rv(ugs)
    }, ignoreNULL = FALSE)

    # Bouton « Effacer la sélection » : vide la sélection carte + table.
    shiny::observeEvent(input$clear_selection, {
      selected_ug_rv(character())
      DT::selectRows(DT::dataTableProxy("table"), NULL)
    })

    # Overlay des UGF sélectionnées (groupe « Selection » distinct du groupe
    # « UGF » choroplèthe -> ne le recouvre pas, se met à jour seul). Même orange
    # (#FF8C00) que le surlignage de sélection du Plan d'actions, pour la cohérence.
    shiny::observe({
      units <- units_sf()
      sel <- selected_ug_rv()
      proxy <- leaflet::leafletProxy("map")
      leaflet::clearGroup(proxy, "Selection")
      if (is.null(units) || length(sel) == 0L) return()
      geo <- tryCatch(sf::st_transform(units, 4326), error = function(e) units)
      if (!"ug_id" %in% names(geo)) return()
      sub <- geo[as.character(geo$ug_id) %in% sel, , drop = FALSE]
      if (nrow(sub) == 0L) return()
      leaflet::addPolygons(proxy, data = sub, group = "Selection",
        layerId = paste0("sel_", sub$ug_id),
        color = "#FF8C00", weight = 4, fill = FALSE, opacity = 0.95)
    })

    # Bandeau du contexte régional (raster). La série requise dépend de la vue :
    # `tx` seule pour la tendance T°max, `tx` + `rr` pour les précipitations et la
    # bivariée. Quatre cas : série absente → bandeau `need_tx` / `need_rr` ; calcul
    # en cours → rien (la notif chrono suffit) ; dégradé → message issu de
    # `meta$reason`. Statut « ok » → pas de bandeau (hors note de fiabilité basse).
    output$context_status <- shiny::renderUI({
      rv$context_refresh
      project_path <- tryCatch(app_state$current_project$path, error = function(e) NULL)
      if (is.null(project_path)) return(NULL)
      if (isTRUE(rv$context_running)) return(NULL)         # chrono en bas-droite
      warn <- function(msg) htmltools::div(
        class = "alert alert-warning py-2 small d-flex align-items-center",
        bsicons::bs_icon("exclamation-triangle-fill", class = "me-2"),
        htmltools::div(class = "flex-grow-1", msg))
      meta   <- rv$context_meta %||% list()
      status <- meta$status
      # Série requise manquante (poussée par l'observer de déclenchement).
      if (identical(status, "need_tx")) return(warn(i18n$t("regen_context_need_tx")))
      if (identical(status, "need_rr")) return(warn(i18n$t("regen_context_need_rr")))
      if (identical(status, "ok")) {
        # OK : note de fiabilité basse (rr / bivarié), en info discrète.
        if (identical(meta$reliability, "low")) {
          return(htmltools::div(class = "alert alert-info py-2 small d-flex align-items-center",
            bsicons::bs_icon("info-circle", class = "me-2"),
            htmltools::div(class = "flex-grow-1", i18n$t("regen_context_reliability_low"))))
        }
        return(NULL)
      }
      if (is.null(status)) return(NULL)                    # pas encore évalué
      # Dégradé : mapper meta$reason (clé i18n) ; repli générique sinon.
      reason <- meta$reason %||% "eobs_downscale_too_few_cells"
      warn(if (i18n$has(reason)) i18n$t(reason) else i18n$t("eobs_downscale_too_few_cells"))
    })

    # Ajoute le raster de contexte + sa légende à une carte/proxy leaflet, selon
    # la vue : univariée tx/rr (colorNumeric, sens de palette piloté par
    # meta$palette$sense — hot_unfavorable=haut rouge, dry_unfavorable=bas rouge)
    # ou bivariée (colorFactor 1-9 + légende 2D 3×3). Partagé rendu/proxy.
    .context_add_layer <- function(m, rast, meta, op) {
      if (!inherits(rast, "SpatRaster") || !identical(meta$status, "ok")) return(m)
      pal <- meta$palette %||% list()
      opts <- leaflet::gridOptions(pane = "contexteRaster")
      if (!is.null(pal$colors)) {
        # Bivarié : classes 1..N*N → colorFactor + légende bivariée 2D N×N. Le
        # cœur livre 25 classes (5×5, `ncol = 5`) ; la légende suit `pal$ncol`.
        classes <- pal$classes %||% seq_along(pal$colors)
        # Coercition en entier : les classes bivariées SONT des entiers 1-25 ; une
        # valeur décimale (source ou reprojection) ne matcherait aucune classe et
        # laisserait un trou. Garde-fou complémentaire du `method = "ngb"` ci-dessous.
        rast <- terra::round(rast)
        cmap <- leaflet::colorFactor(pal$colors, domain = classes, na.color = "transparent")
        # Axes annotés par la plage RÉELLEMENT observée sur la zone (quantiles des
        # rasters downscalés) via `.regen_biv_axis`, avec 0 garanti dans la plage.
        zcore <- pal$zero %||% list()
        ys <- .regen_biv_axis(meta$tx, meta$breaks$tmax,   suppressWarnings(as.numeric(zcore[["tmax"]])))
        xs <- .regen_biv_axis(meta$rr, meta$breaks$precip, suppressWarnings(as.numeric(zcore[["precip"]])))
        # Titre en deux lignes : « Tendance bivariée » + le couple de variables
        # entre parenthèses (T°max estivale × précipitations) en sous-titre.
        raw_title <- meta$value_label %||% i18n$t("regen_context_bivariate")
        biv_title <- .regen_biv_title(raw_title)
        leg <- as.character(bivariate_legend_html(
          palette  = stats::setNames(pal$colors, classes),
          axis_x   = i18n$t("regen_context_axis_rr"),
          axis_y   = i18n$t("regen_context_axis_tx"),
          title    = biv_title$title,
          subtitle = biv_title$subtitle,
          ncol     = pal$ncol,
          zero    = list(tmax = ys$zero, precip = xs$zero),
          x_range = xs$range,
          y_range = ys$range))
        # method = "ngb" (plus proche voisin) OBLIGATOIRE pour un raster
        # CATÉGORIEL : la reprojection par défaut (bilinéaire) interpole les codes
        # de classe entiers 1-25 en valeurs décimales (3.4, 7.2…) qui ne
        # correspondent à aucune classe de la palette → pixels transparents (trous).
        m |>
          leaflet::addRasterImage(rast, colors = cmap, opacity = op, project = TRUE,
            method = "ngb", group = "Contexte E-OBS", options = opts) |>
          leaflet::addControl(html = leg, position = "bottomright", layerId = "context_legend",
            className = "nmt-bivariate-control")
      } else {
        # Univarié : colorNumeric, sens piloté par meta$palette$sense.
        lo <- pal$low; hi <- pal$high
        if (is.null(lo) || is.null(hi)) {
          vv <- suppressWarnings(range(terra::values(rast), na.rm = TRUE)); lo <- vv[1]; hi <- vv[2]
        }
        if (!is.finite(lo) || !is.finite(hi) || lo == hi) return(m)
        rev <- !identical(pal$sense, "dry_unfavorable")   # dry_unfavorable → bas = rouge
        cn <- leaflet::colorNumeric("RdYlBu", domain = c(lo, hi), reverse = rev,
                                    na.color = "transparent")
        m |>
          leaflet::addRasterImage(rast, colors = cn, opacity = op, project = TRUE,
            group = "Contexte E-OBS", options = opts) |>
          leaflet::addLegend(pal = cn, values = c(lo, hi), position = "bottomright",
            title = meta$value_label %||% i18n$t("regen_context_value_tx_trend"),
            layerId = "context_legend")
      }
    }

    # Carte de contexte — patron FAST/FORDEAD : fond STABLE (jamais re-rendu) +
    # `leafletProxy` pour le raster/l'opacité/la légende. Rendu de base lit raster
    # + opacité en `isolate` → zoom + fond OSM/Satellite préservés au slider et au
    # changement de fond ; l'observer proxy met à jour sans reconstruire. Raster
    # dans un map-pane dédié SOUS l'emprise UGF.
    output$context_map <- leaflet::renderLeaflet({
      units <- units_sf()
      shiny::req(units)
      geo_ugf <- tryCatch(sf::st_transform(units, 4326), error = function(e) units)
      op <- max(0, min(1, as.numeric(shiny::isolate(input$context_opacity) %||% 0.8)))
      m <- leaflet::leaflet() |>
        leaflet::addProviderTiles("OpenStreetMap", group = "OSM") |>
        leaflet::addProviderTiles("Esri.WorldImagery", group = "Satellite") |>
        leaflet::addMapPane("contexteRaster", zIndex = 250) |>
        leaflet::addLayersControl(
          baseGroups    = c("OSM", "Satellite"),
          overlayGroups = c("Contexte E-OBS", "UGF"),
          options       = leaflet::layersControlOptions(collapsed = TRUE))
      m <- .context_add_layer(m, shiny::isolate(rv$context_raster),
                              shiny::isolate(rv$context_meta) %||% list(), op)
      m <- leaflet::addPolygons(m, data = geo_ugf, group = "UGF", weight = 1.5,
        color = "#1f6feb", fillColor = "#1f6feb", fillOpacity = 0.05)
      bb <- tryCatch(as.numeric(sf::st_bbox(geo_ugf)), error = function(e) NULL)
      if (!is.null(bb) && all(is.finite(bb))) {
        m <- leaflet::fitBounds(m, bb[1], bb[2], bb[3], bb[4])
      }
      m
    })

    # Mise à jour RASTER + LÉGENDE via leafletProxy : réagit au raster (calcul
    # async / changement de vue), au meta et au curseur d'opacité — sans
    # re-render (zoom + fond préservés). Respecte la décoche du group.
    shiny::observe({
      rast <- rv$context_raster
      meta <- rv$context_meta %||% list()
      op   <- max(0, min(1, as.numeric(input$context_opacity %||% 0.8)))
      shown <- input$context_map_groups
      proxy <- leaflet::leafletProxy("context_map") |>
        leaflet::clearGroup("Contexte E-OBS") |>
        leaflet::removeControl("context_legend")
      proxy <- .context_add_layer(proxy, rast, meta, op)
      if (!is.null(shown) && !("Contexte E-OBS" %in% shown)) {
        leaflet::hideGroup(proxy, "Contexte E-OBS")
      }
    })

    # --- Graphiques au clic sur la maille E-OBS (spec 036) -------------------
    # Le clic leaflet (input$context_map_click, lng/lat 4326) ouvre un panneau de
    # 4 graphes rendant la donnée SOUS la couleur, à la maille cliquée. Toute la
    # donnée vient des accesseurs cœur (eobs_summer_series / eobs_trend_fit /
    # eobs_monthly_climatology) ; l'app ne fait que clic → point → tracé.
    #
    # Cache de session des stacks estivaux (une couche/an, résolution E-OBS
    # native ~11 km — cf. §7 honnêteté spatiale) : le 1er clic lit le .nc caché,
    # les suivants sont instantanés. Clé = projet + buffer + variable.
    summer_stacks <- new.env(parent = emptyenv())
    .load_summer_stack <- function(units, project_path, var, buffer_m) {
      key <- paste(project_path %||% "-", buffer_m, var, sep = "|")
      if (!is.null(summer_stacks[[key]])) return(summer_stacks[[key]]$stk)
      stk <- .regen_load_eobs_buffered(units, project_path, var, buffer_m)
      summer_stacks[[key]] <- list(stk = stk)   # mémoïse même NULL (évite de re-tenter)
      stk
    }

    # Extrait la série estivale au point + son ajustement de tendance pour une
    # variable ; renvoie NULL si le stack est absent. `pt` = c(lng, lat) en 4326.
    .ctx_series_entry <- function(units, project_path, var, pt, buffer_m) {
      stk <- .load_summer_stack(units, project_path, var, buffer_m)
      if (is.null(stk)) return(NULL)
      ser <- tryCatch(nemeton::eobs_summer_series(stk, pt), error = function(e) NULL)
      if (is.null(ser)) return(NULL)
      fit <- tryCatch(nemeton::eobs_trend_fit(ser), error = function(e) NULL)
      list(var = var, ser = ser, fit = fit,
        label = i18n$t(if (var == "tx") "regen_ctx_series_tx" else "regen_ctx_series_rr"),
        unit  = i18n$t(if (var == "tx") "regen_ctx_unit_tx" else "regen_ctx_unit_rr"),
        sense = if (var == "tx") "hot_red" else "dry_red")
    }

    # Entrée de distribution pour une variable : lit le raster de pente caché
    # (`context_<var>.tif`) + extrait la pente au point cliqué. Renvoie NULL si le
    # raster de pente n'a pas (encore) été calculé pour cette variable.
    .ctx_distrib_entry <- function(var, ev, project_path) {
      cached <- regeneration_context_cached(project_path, var)
      rast <- cached$raster
      if (!inherits(rast, "SpatRaster")) return(NULL)
      vals <- tryCatch(as.numeric(terra::values(rast)), error = function(e) NULL)
      pv <- tryCatch({
        v <- terra::vect(cbind(ev$lng, ev$lat), crs = "EPSG:4326")
        v <- terra::project(v, terra::crs(rast))
        as.numeric(terra::extract(rast, v)[1, 2])
      }, error = function(e) NA_real_)
      list(values = vals, point = pv,
        label = i18n$t(if (var == "tx") "regen_ctx_series_tx" else "regen_ctx_series_rr"),
        unit  = i18n$t(if (var == "tx") "regen_ctx_unit_tx" else "regen_ctx_unit_rr"))
    }

    # Garde anti-multi-clics (parité FAST/FORDEAD) : ignore tout nouveau clic tant
    # que le graphique précédent n'a pas fini de se calculer.
    ctx_computing <- shiny::reactiveVal(FALSE)

    shiny::observeEvent(input$context_map_click, {
      if (isTRUE(shiny::isolate(ctx_computing()))) return()
      ev <- input$context_map_click
      shiny::req(ev$lng, ev$lat)
      units <- units_sf()
      project_path <- tryCatch(app_state$current_project$path, error = function(e) NULL)
      if (is.null(units) || is.null(project_path)) {
        shiny::showNotification(i18n$t("regen_need_project"), type = "warning")
        return()
      }
      pt   <- c(ev$lng, ev$lat)                 # EPSG:4326, accepté tel quel
      buf  <- (input$buffer_km %||% 25) * 1000
      view <- rv$context_loaded_view %||% input$context_view %||% "tx"

      notif_id <- session$ns("ctx_computing")
      ctx_computing(TRUE)
      # Message « Calcul du graphique en cours… » affiché TOUT DE SUITE.
      shiny::showNotification(i18n$t("regen_ctx_computing"),
        id = notif_id, type = "message", duration = NULL)

      # Calcul APRÈS le flush courant (`onFlushed`) : la notification part au
      # client AVANT l'extraction synchrone (série/climatologie, quelques
      # secondes) ; le flag `ctx_computing` bloque les clics entre-temps.
      session$onFlushed(function() {
        on.exit({
          shiny::removeNotification(notif_id, session = session)
          shiny::isolate(ctx_computing(FALSE))
        }, add = TRUE)

        # Graphes 1-2 : série(s) + tendance(s). Vue tx/rr → variable de la vue ;
        # bivariée → les DEUX (le sens du croisement).
        vars <- switch(view, tx = "tx", rr = "rr", bivariate = c("tx", "rr"), "tx")
        entries <- Filter(Negate(is.null),
          lapply(vars, function(v) .ctx_series_entry(units, project_path, v, pt, buf)))

        # Graphe 3 : distribution des pentes du buffer + pente de la maille. Une
        # entrée par variable de la vue (bivariée → tx ET rr). Le raster de pente
        # est relu depuis le cache `context_<var>.tif` (la bivariée porte des
        # classes 1-25, donc on lit les rasters univariés sous-jacents).
        distrib_entries <- Filter(Negate(is.null),
          lapply(vars, function(v) .ctx_distrib_entry(v, ev, project_path)))

        # Graphe 4 : climatologie mensuelle. tg si acquise (honnête), sinon repli tx.
        nc_tg <- regen_eobs_cached_nc(project_path, "tg")
        nc_t  <- nc_tg %||% regen_eobs_cached_nc(project_path, "tx")
        temp_is_tg <- !is.null(nc_tg)
        nc_rr <- regen_eobs_cached_nc(project_path, "rr")
        clim_t  <- if (!is.null(nc_t)) tryCatch(nemeton::eobs_monthly_climatology(
          nc_t, pt, var = if (temp_is_tg) "tg" else "tx"), error = function(e) NULL)
        clim_rr <- if (!is.null(nc_rr)) tryCatch(nemeton::eobs_monthly_climatology(
          nc_rr, pt, var = "rr"), error = function(e) NULL)

        shiny::isolate(rv$click_series <- list(view = view, entries = entries,
          distrib = distrib_entries, clim_t = clim_t, clim_rr = clim_rr,
          temp_is_tg = temp_is_tg, has_temp = !is.null(nc_t)))

        # Bouton « plein écran » ancré en HAUT À DROITE du titre (même patron que
        # FAST/FORDEAD, mod_monitoring_pixel_map) : un JS bascule la classe BS5
        # `.modal-fullscreen` sur la `.modal-dialog` + dispatch `resize` (plotly
        # responsive n'écoute que window.resize). Le wrapper flex + la règle CSS
        # `.modal-fullscreen` font grandir le graphe à la taille de l'écran.
        shiny::showModal(shiny::modalDialog(
          title = htmltools::tagList(
            htmltools::span(sprintf(i18n$t("regen_ctx_cell_title"), ev$lat, ev$lng)),
            htmltools::tags$button(type = "button",
              class = "btn btn-sm btn-outline-secondary",
              style = "position:absolute;top:0.75rem;right:0.75rem;z-index:2;",
              title = i18n$t("regen_ctx_fullscreen"),
              onclick = paste0(
                "this.closest('.modal-dialog').classList.toggle('modal-fullscreen');",
                "setTimeout(function(){window.dispatchEvent(new Event('resize'));},250);"),
              bsicons::bs_icon("arrows-fullscreen"))),
          size = "xl", easyClose = TRUE,
          footer = shiny::modalButton(i18n$t("close")),
          htmltools::tags$style(htmltools::HTML(paste0(
            ".regen-ctx-wrap{height:440px;display:flex;flex-direction:column;}",
            ".regen-ctx-wrap>.tabbable{display:flex;flex-direction:column;flex:1 1 auto;min-height:0;}",
            ".regen-ctx-wrap .tab-content{flex:1 1 auto;min-height:0;}",
            ".regen-ctx-wrap .tab-pane.active{height:100%;}",
            ".modal-fullscreen .regen-ctx-wrap{height:calc(100vh - 170px)!important;}"))),
          htmltools::div(class = "regen-ctx-wrap",
            shiny::tabsetPanel(
              shiny::tabPanel(i18n$t("regen_ctx_series"),  plotly::plotlyOutput(ns("ctx_g1"), height = "100%")),
              shiny::tabPanel(i18n$t("regen_ctx_anomaly"), plotly::plotlyOutput(ns("ctx_g2"), height = "100%")),
              shiny::tabPanel(i18n$t("regen_ctx_distrib"), plotly::plotlyOutput(ns("ctx_g3"), height = "100%")),
              shiny::tabPanel(i18n$t("regen_ctx_ombro"),   plotly::plotlyOutput(ns("ctx_g4"), height = "100%"))))))
      }, once = TRUE)
    })

    # Graphe 1 — série(s) + tendance(s). Bivariée → deux panneaux côte à côte.
    output$ctx_g1 <- plotly::renderPlotly({
      cs <- rv$click_series
      if (is.null(cs) || !length(cs$entries)) return(.regen_ctx_empty_plot(i18n$t("regen_ctx_out_of_coverage")))
      plots <- lapply(cs$entries, function(e)
        .regen_ctx_series_plot(e$ser, e$fit, e$label, e$unit, i18n))
      if (length(plots) == 1L) plots[[1]] else plotly::subplot(plots, nrows = 1L, titleX = TRUE, titleY = TRUE, margin = 0.06)
    })

    # Graphe 2 — anomalies annuelles. Bivariée → deux panneaux.
    output$ctx_g2 <- plotly::renderPlotly({
      cs <- rv$click_series
      if (is.null(cs) || !length(cs$entries)) return(.regen_ctx_empty_plot(i18n$t("regen_ctx_out_of_coverage")))
      plots <- lapply(cs$entries, function(e)
        .regen_ctx_anomaly_plot(e$ser, e$label, e$sense, i18n))
      if (length(plots) == 1L) plots[[1]] else plotly::subplot(plots, nrows = 1L, titleX = TRUE, titleY = TRUE, margin = 0.06)
    })

    # Graphe 3 — distribution régionale des pentes + maille cliquée. Bivariée →
    # deux panneaux (tx et rr) côte à côte.
    output$ctx_g3 <- plotly::renderPlotly({
      cs <- rv$click_series
      d <- cs$distrib
      if (is.null(d) || !length(d)) return(.regen_ctx_empty_plot(i18n$t("regen_ctx_distrib_na")))
      plots <- lapply(d, function(e)
        .regen_ctx_distrib_plot(e$values, e$point, e$label, e$unit, i18n))
      if (length(plots) == 1L) plots[[1]] else plotly::subplot(plots, nrows = 1L, titleX = TRUE, titleY = TRUE, margin = 0.06)
    })

    # Graphe 4 — diagramme ombrothermique (Gaussen). Sans température acquise :
    # panneau d'invite à télécharger la série (bouton tg dans la sidebar).
    output$ctx_g4 <- plotly::renderPlotly({
      cs <- rv$click_series
      if (is.null(cs) || !isTRUE(cs$has_temp)) return(.regen_ctx_empty_plot(i18n$t("regen_ctx_ombro_need_temp")))
      .regen_ctx_ombro_plot(cs$clim_rr, cs$clim_t, cs$temp_is_tg, i18n)
    })

    output$table <- DT::renderDataTable({
      df <- regen_table_df()
      order_col <- if ("rang_sensibilite" %in% names(df)) "rang_sensibilite" else NULL
      DT::datatable(df, rownames = FALSE,
        selection = list(mode = "multiple", target = "row"),
        options = list(pageLength = 15, scrollX = TRUE,
          # Localisation des libellés DT (boîte de recherche, pagination...)
          language = if (identical(i18n$language, "fr")) {
            list(
              search = "Rechercher :",
              info = "_TOTAL_ UGF",
              lengthMenu = "Afficher _MENU_ UGF"
            )
          } else {
            list()
          },
          order = if (!is.null(order_col)) list(list(which(names(df) == order_col) - 1, "asc")) else list()))
    })

    # ============================================================
    # Commentaires de fiche parcelle (persist\u00e9s par projet) \u2014 spec 039
    # ============================================================
    # Un commentaire libre par UGF (notes, conseil IA ins\u00e9r\u00e9). Source de v\u00e9rit\u00e9 :
    # regen_comments_rv (liste ug_id -> texte), charg\u00e9e \u00e0 l'ouverture du projet
    # et sauvegard\u00e9e (d\u00e9bounce) dans data/regen_comments.json.
    regen_comments_rv <- shiny::reactiveVal(list())

    # (Re)chargement \u00e0 chaque changement de projet.
    shiny::observeEvent(app_state$current_project, {
      pid <- tryCatch(app_state$current_project$id, error = function(e) NULL)
      cm <- if (is.null(pid)) list() else tryCatch(load_regen_comments(pid),
        error = function(e) list())
      regen_comments_rv(cm %||% list())
    }, ignoreNULL = FALSE)

    # Sauvegarde d\u00e9bounc\u00e9e (au plus une \u00e9criture disque par seconde de repos).
    regen_comments_deb <- shiny::debounce(shiny::reactive(regen_comments_rv()), 1000)
    shiny::observeEvent(regen_comments_deb(), {
      pid <- tryCatch(app_state$current_project$id, error = function(e) NULL)
      if (is.null(pid)) return()
      save_regen_comments(pid, regen_comments_deb())
    }, ignoreInit = TRUE)

    # Observateur paresseux par UGF s\u00e9lectionn\u00e9e : capte les \u00e9ditions du textarea
    # et les r\u00e9percute dans regen_comments_rv. Une seule fois par UGF.
    comment_obs_done <- new.env(parent = emptyenv())
    shiny::observe({
      sel <- selected_ug_rv()
      for (ug in sel) {
        key <- .regen_comment_key(ug)
        if (isTRUE(comment_obs_done[[key]])) next
        comment_obs_done[[key]] <- TRUE
        local({
          ug_local <- ug; key_local <- key
          shiny::observeEvent(input[[key_local]], {
            cur <- regen_comments_rv()
            cur[[ug_local]] <- input[[key_local]] %||% ""
            regen_comments_rv(cur)
          }, ignoreInit = TRUE)
        })
      }
    })

    # Bouton unique : ins\u00e8re le dernier conseil IA g\u00e9n\u00e9r\u00e9 dans le commentaire de
    # TOUTES les UGF s\u00e9lectionn\u00e9es (remplace le contenu ; \u00e9ditable ensuite).
    shiny::observeEvent(input$regen_comment_insert_ai, {
      sel <- selected_ug_rv()
      if (length(sel) == 0L) {
        shiny::showNotification(i18n$t("regen_select_ug"), type = "warning")
        return()
      }
      hist <- regen_ai_hist()
      advice <- if (length(hist) > 0L) hist[[length(hist)]]$a %||% "" else ""
      if (!nzchar(trimws(advice))) {
        shiny::showNotification(i18n$t("regen_comment_no_advice"), type = "warning")
        return()
      }
      cur <- regen_comments_rv()
      for (ug in sel) {
        cur[[ug]] <- advice
        shiny::updateTextAreaInput(session, .regen_comment_key(ug), value = advice)
      }
      regen_comments_rv(cur)
      shiny::showNotification(sprintf(i18n$t("regen_comment_inserted_fmt"), length(sel)),
        type = "message", duration = 4)
    })


    # Fiche(s) parcelle : une par UGF sélectionnée (multi-sélection). Les lignes
    # viennent de la MÊME `regen_table_df()` que le tableau, l'ordre suit la
    # sélection courante (`selected_ug_rv`, alimentée carte + table).
    output$parcel_sheet <- shiny::renderUI({
      df <- tryCatch(regen_table_df(), error = function(e) NULL)
      if (is.null(df)) return(htmltools::div(class = "text-muted fst-italic",
                                             i18n$t("regen_need_project")))
      sel <- selected_ug_rv()
      if (length(sel) == 0L) {
        return(htmltools::div(class = "text-muted fst-italic", i18n$t("regen_select_ug")))
      }
      fields <- c("indice_priorite_regen", "sensibilite", "njstress",
        "istress", "deb_stress", "rew_min", "d_tmax", "d_vpd", "couverture_pct")
      # Classement top-3 des essences (spec 039) : calculé une fois, filtré par UGF.
      rk <- tryCatch(species_ranking(), error = function(e) NULL)
      one_sheet <- function(row) {
        fs <- intersect(fields, names(row))
        ug <- if ("ug_id" %in% names(row)) as.character(row$ug_id) else ""
        # Zone de commentaire éditable par UGF (persistée). Lecture ISOLÉE pour
        # ne pas re-rendre la fiche à chaque frappe (sinon le curseur saute) ;
        # le bouton d'insertion IA passe par updateTextAreaInput (pas de re-render).
        key <- .regen_comment_key(ug)
        comment_zone <- htmltools::div(class = "mt-1 mb-3",
          htmltools::tags$label(class = "form-label small fw-bold mb-1",
            `for` = ns(key), i18n$t("regen_comment_label")),
          shiny::textAreaInput(ns(key), label = NULL,
            value = shiny::isolate(regen_comments_rv())[[ug]] %||% "",
            placeholder = i18n$t("regen_comment_placeholder"),
            rows = 4, width = "100%", resize = "vertical"))
        htmltools::tagList(
          htmltools::tags$table(class = "table table-sm mb-2",
            htmltools::tags$caption(class = "fw-bold text-body",
              sprintf("%s %s", i18n$t("regen_table_card_title"), ug)),
            lapply(fs, function(f) htmltools::tags$tr(
              htmltools::tags$th(i18n$t(paste0("regen_col_",
                switch(f, indice_priorite_regen = "indice", sensibilite = "sensibilite",
                  njstress = "njstress", istress = "istress", deb_stress = "deb_stress",
                  rew_min = "rew_min", d_tmax = "dtmax", d_vpd = "dvpd",
                  couverture_pct = "couverture", f)))),
              htmltools::tags$td(format(row[[f]], digits = 3))))),
          .regen_species_ranking_ui(rk, ug, i18n),
          comment_zone)
      }
      rows <- which(as.character(df$ug_id) %in% sel)
      if (length(rows) == 0L) {
        return(htmltools::div(class = "text-muted fst-italic", i18n$t("regen_select_ug")))
      }
      # Bouton unique : insère le dernier conseil IA dans TOUTES les UGF sélectionnées.
      insert_btn <- htmltools::div(class = "mb-2",
        shiny::actionButton(ns("regen_comment_insert_ai"),
          i18n$t("regen_comment_insert_ai"),
          icon = shiny::icon("robot"),
          class = "btn-sm btn-outline-primary"))
      htmltools::tagList(insert_btn,
        htmltools::div(class = "row",
          lapply(rows, function(i) htmltools::div(class = "col-md-6",
            one_sheet(df[i, , drop = FALSE])))))
    })

    # --- Export GPKG (§7) -------------------------------------------------
    # --- Envoyer vers Terrain : centro\u00efdes des UGF (s\u00e9lection ou toutes) vers la
    # couche \u00ab regeneration \u00bb du samples.gpkg (parit\u00e9 Plan d'actions). Rafra\u00eechit
    # mod_sampling via app_state$samples_refresh.
    shiny::observeEvent(input$export_terrain, {
      # Le toast bas-droite est affiché client-side (onclick) ; on le masque à la
      # fin de l'opération quel que soit le chemin (succès, garde, erreur).
      on.exit(session$sendCustomMessage("nemetonHideDownloadToast", list()), add = TRUE)
      if (deny_if_readonly(app_state, i18n)) return()
      project <- app_state$current_project
      res <- rv$result
      if (is.null(project) || is.null(res) || !inherits(res, "sf") || nrow(res) == 0L) {
        shiny::showNotification(i18n$t("regen_export_terrain_empty"), type = "warning")
        return()
      }
      ids <- as.character(res$ug_id)
      sel <- selected_ug_rv()
      keep <- if (length(sel) > 0L) which(ids %in% sel) else seq_along(ids)
      if (length(keep) == 0L) {
        shiny::showNotification(i18n$t("regen_export_terrain_empty"), type = "warning")
        return()
      }
      sub <- res[keep, , drop = FALSE]
      centroids <- tryCatch(
        suppressWarnings(sf::st_centroid(sf::st_geometry(sub))),
        error = function(e) NULL)
      if (is.null(centroids)) {
        shiny::showNotification(i18n$t("regen_export_terrain_failed"), type = "error")
        return()
      }
      dropped <- sf::st_drop_geometry(sub)
      num_col <- function(nm) if (nm %in% names(dropped)) as.numeric(dropped[[nm]]) else rep(NA_real_, nrow(sub))
      df <- data.frame(
        plot_id = paste0("regen_", as.character(sub$ug_id)),
        ug_id = as.character(sub$ug_id),
        indice_priorite = num_col("indice_priorite_regen"),
        sensibilite = num_col("sensibilite"),
        rang = num_col("rang_sensibilite"),
        stringsAsFactors = FALSE)
      sf_pts <- sf::st_sf(df, geometry = centroids)
      ok <- tryCatch(save_samples(project$id, sf_pts, layer = "regeneration"),
        error = function(e) FALSE)
      if (isTRUE(ok)) {
        app_state$samples_refresh <- (app_state$samples_refresh %||% 0L) + 1L
        shiny::showNotification(
          sprintf(i18n$t("regen_export_terrain_ok_fmt"), nrow(sf_pts)),
          type = "message", duration = 6)
      } else {
        shiny::showNotification(i18n$t("regen_export_terrain_failed"),
          type = "error", duration = 6)
      }
    })


    output$export_gpkg <- shiny::downloadHandler(
      filename = function() {
        paste0(.project_export_slug(app_state$current_project, "nemeton"),
               "_regeneration.gpkg")
      },
      content = function(file) {
        res <- rv$result
        if (is.null(res)) {
          shiny::showNotification(i18n$t("regen_export_empty"), type = "warning")
          return(invisible(NULL))
        }
        export_regeneration_geopackage(res, file)
      }
    )

    # --- Export PDF (fiche par UGF : conditions + top-3 + commentaire) -----
    # Même patron que le Plan d'actions : nom <slug>_regeneration.pdf, toast
    # client (nemetonShowDownloadToast) masqué en on.exit, et archivage best-
    # effort dans <projet>/exports/ sur le seul chemin succès.
    output$export_pdf <- shiny::downloadHandler(
      filename = function() {
        paste0(.project_export_slug(app_state$current_project, "nemeton"),
               "_regeneration.pdf")
      },
      content = function(file) {
        i18n <- get_i18n(app_state$language)
        on.exit(session$sendCustomMessage("nemetonHideDownloadToast", list()),
                add = TRUE)
        res <- rv$result
        project <- app_state$current_project
        if (is.null(res) || is.null(project)) {
          shiny::showNotification(i18n$t("regen_export_empty"), type = "warning")
          writeLines("No data available", file)
          return()
        }
        if (!is_quarto_installed()) {
          shiny::showNotification(i18n$t("regen_export_no_quarto"), type = "warning")
          writeLines("Quarto is required for the PDF export.", file)
          return()
        }
        result <- tryCatch(
          generate_regeneration_pdf(project, res, regen_comments_rv(), file,
            language = i18n$language),
          error = function(e) {
            err_msg <- conditionMessage(e)
            cli::cli_warn("reGénération PDF export failed: {err_msg}")
            shiny::showNotification(
              paste(i18n$t("regen_export_pdf_failed"), ":", err_msg),
              type = "error", duration = NULL)
            NULL
          })
        if (is.null(result)) {
          writeLines(
            c("PDF generation failed.",
              "See the in-app error toast for the underlying cause."), file)
        } else {
          # Chemin succès uniquement : archive une copie dans exports/, parité
          # avec le Plan d'actions et le rapport de synthèse. Best-effort.
          .archive_regeneration_pdf(file, project)
        }
      }
    )

    # --- Persistance versionnée en base (§6A) -----------------------------
    shiny::observeEvent(input$persist_db, {
      on.exit(session$sendCustomMessage("nemetonHideDownloadToast", list()), add = TRUE)
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

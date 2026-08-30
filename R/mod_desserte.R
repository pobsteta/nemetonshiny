# ===========================================================================
# Module - Creation de desserte forestiere (ForetAccess), sous-onglet Terrain
# ===========================================================================
#
# Presentation Shiny du service `R/service_desserte.R` (adaptateur autour des
# moteurs de creation de reseau de `foretaccess`). Aucune logique metier (regle
# 2) : le module orchestre l'UI, l'execution asynchrone (worker `future`) et le
# rendu carte/badges.
#
# v1 : moteur GLOUTON seul, OPT-IN. La duree depend de la SURFACE de l'emprise
# et de `skidding_m` (cf. service_desserte.R), pas du nombre de parcelles. Meme
# patron que reGeneration / Accessibilite : `ExtendedTask` + `future_promise`,
# notif persistante bas-droite avec chrono, retour immediat. Le reseau cree est
# affiche en overlay RASTER (leger) ; les lignes vectorielles detaillees partent
# a l'export GeoPackage.

# Lit `engine_status.json` du cache desserte, ecrit par le worker a chaque
# changement d'etape (`.dess_write_phase`). NULL si absent, illisible ou perime
# (> 2 min sans mise a jour) - meme contrat que `.regen_read_phase()`. Le seuil
# de peremption evite d'afficher indefiniment la phase d'un worker mort.
.dess_read_phase <- function(project_path) {
  if (is.null(project_path)) return(NULL)
  f <- file.path(project_path, "cache", "desserte", "engine_status.json")
  if (!file.exists(f)) return(NULL)
  st <- tryCatch(jsonlite::fromJSON(f), error = function(e) NULL)
  if (is.null(st) || is.null(st$phase)) return(NULL)
  if (!is.null(st$ts) && as.integer(Sys.time()) - st$ts > 120L) return(NULL)
  as.character(st$phase)[1]
}

# --- Groupes de couches de la carte Desserte --------------------------------
# Ces noms SONT les libelles du controle de couches leaflet : il n'y a pas de
# chaine d'affichage separee, d'ou les accents, ecrits en `\uXXXX` (regle 4).
#
# Portes par des constantes et non ecrits a chaque appel : chacun servait a cinq
# endroits (declaration, peinture, `clearGroup`, `hideGroup`, test d'etat), et
# en accentuer un seul aurait rompu le lien entre la case et la couche.

#' @noRd
DESS_GROUPE_PARCELLES <- "Parcelles"
#' @noRd
DESS_GROUPE_EXISTANTE <- "Desserte existante"
#' @noRd
DESS_GROUPE_RESEAU <- "R\u00e9seau cr\u00e9\u00e9"
#' @noRd
DESS_GROUPE_LIGNES <- "Lignes cr\u00e9\u00e9es"
#' @noRd
DESS_GROUPE_TYPE <- "R\u00e9seau typ\u00e9"
# " Pistes OSM " et non " pistes absentes de la BD TOPO " : le GeoPackage porte
# l'acquisition OSM BRUTE, doublons compris. Le " hors corridor " existe cote
# coeur mais n'en sort qu'en kilometres agreges (cf. `run_desserte_osm()`).
#' @noRd
DESS_GROUPE_OSM <- "Pistes OSM"
#' @noRd
DESS_GROUPE_DETECTEE <- "Routes d\u00e9tect\u00e9es"

#' Couleurs des classes de lin\u00e9aires d\u00e9tect\u00e9s (dessertR)
#'
#' Vocabulaire de `dsr_classer()`, ordonn\u00e9 du plus " vraie route " au moins :
#' route foresti\u00e8re, piste, desserte ind\u00e9termin\u00e9e, puis ce qui n'est PAS de la
#' desserte \u2014 cloisonnement d'exploitation, layon parcellaire, pare-feu.
#'
#' Palette choisie par MESURE et non au jug\u00e9, par recherche sous contraintes :
#' s\u00e9paration minimale de 27,4 en CIE Lab entre deux classes, **conserv\u00e9e sous
#' simulation de deut\u00e9ranopie, protanopie et tritanopie** \u2014 un seul de ces trois
#' filtres suffit \u00e0 faire s'effondrer une palette choisie \u00e0 l'\u0153il \u2014 et d'au
#' moins 20 vis-\u00e0-vis de toutes les autres couches de cette carte (r\u00e9seau typ\u00e9,
#' lignes cr\u00e9\u00e9es, desserte existante, raster, parcelles, pistes OSM).
#' Clart\u00e9 born\u00e9e \u00e0 L* \u2208 [28, 62] pour rester lisible aussi bien sur le fond OSM
#' clair que sur le fond satellite sombre. V\u00e9rifi\u00e9e par `test-desserte_visualisation.R`.
#' @noRd
DESS_DETECT_COLS <- c(
  route_forestiere           = "#4425AB",  # indigo  - la plus " ouvrage "
  piste_forestiere           = "#6484F6",  # bleu
  desserte                   = "#652E5A",  # prune
  cloisonnement_exploitation = "#A38A06",  # or     - PAS de la desserte
  layon_parcellaire          = "#C55EB0",  # orchidee
  pare_feu                   = "#9D3037",  # brique
  indetermine                = "#BDBDBD"   # gris
)

#' Translate a dessertR class code, falling back on the code itself
#'
#' The vocabulary is the core's, and it may grow: an unknown code is shown AS IS
#' rather than mapped to "autre", which would hide the arrival of a new class.
#'
#' @param x Character vector of `CLASSE` values.
#' @param i18n Translator.
#' @return A character vector of labels.
#' @noRd
.dess_detect_classe_label <- function(x, i18n) {
  x <- as.character(x)
  # Le vocabulaire connu est celui de la palette : y confronter le code AVANT
  # d'interroger l'i18n, faute de quoi chaque classe nouvelle produirait un
  # avertissement " Translation key not found " par troncon.
  connus <- names(DESS_DETECT_COLS)
  vapply(x, function(k) {
    if (is.na(k) || !nzchar(k)) return(i18n$t("dess_detect_classe_indetermine"))
    if (!(k %in% connus)) return(k)
    i18n$t(paste0("dess_detect_classe_", k))
  }, character(1), USE.NAMES = FALSE)
}

#' Popup of one detected segment
#'
#' Carries what qualifies the class and not only the class: the confidence, the
#' criteria that voted, and the proposed OSM tagging - which is a **proposal**,
#' never an upload.
#'
#' @param d An `sf` of detected segments.
#' @param i18n Translator.
#' @return A character vector of HTML popups, one per row.
#' @noRd
.dess_detect_popup <- function(d, i18n) {
  n <- nrow(d)
  col <- function(nm) {
    v <- d[[nm]]
    if (is.null(v)) rep(NA, n) else v
  }
  esc <- function(v) htmltools::htmlEscape(ifelse(is.na(v), "", as.character(v)))
  cl <- .dess_detect_classe_label(col("CLASSE"), i18n)
  conf <- suppressWarnings(as.numeric(col("CLASSE_CONF")))
  motif <- as.character(col("CLASSE_MOTIF"))
  tags <- as.character(col("OSM_TAGS"))

  paste0(
    "<b>", esc(cl), "</b>",
    ifelse(is.finite(conf),
           paste0("<br>", i18n$t("dess_detect_popup_conf"), " ",
                  sprintf("%.0f %%", 100 * conf)), ""),
    ifelse(is.na(motif) | !nzchar(motif), "",
           paste0("<br>", i18n$t("dess_detect_popup_motif"), " ", esc(motif))),
    ifelse(is.na(tags) | !nzchar(tags), "",
           paste0("<br>", i18n$t("dess_detect_popup_osm"), " <code>", esc(tags),
                  "</code><br><span class='text-muted'>",
                  i18n$t("dess_detect_popup_osm_note"), "</span>")))
}


#' Field label carrying its help text in an "i"
#'
#' The Desserte sidebar showed one `text-muted small` paragraph — sometimes a
#' full `alert` — under nearly every input. Stacked, they pushed the « Lancer le
#' calcul » button below the fold and turned the panel into a page of prose that
#' is read once and skipped forever after.
#'
#' [info_popover_in_label()] and not [info_popover()]: a click inside a `<label>`
#' activates its control, so the plain "i" would flip the radio on its way to
#' opening the popover.
#'
#' @param label Character. The visible label.
#' @param ... Popover content.
#' @return A `tagList` usable as the `label` of a shiny input.
#' @noRd
.dess_label_info <- function(label, ...) {
  htmltools::tagList(label, info_popover_in_label(...))
}

#' Warning content inside a popover
#'
#' Keeps the triangle that the `alert-warning` block carried: folding a caution
#' away must not turn it into a neutral note.
#'
#' @param texte Character. The warning text.
#' @return A `<div>` for popover content.
#' @noRd
.dess_alerte <- function(texte) {
  htmltools::div(
    class = "mb-0",
    htmltools::tags$span(class = "text-warning me-1",
                         shiny::icon("triangle-exclamation")),
    texte)
}

#' An action button and its explanation, side by side
#'
#' Where the intro of a panel goes. NOT in the panel title: an accordion title
#' is a collapse toggle, and an "i" placed there folds the panel one is reading.
#' Verified in Chrome — `stopPropagation()`, `preventDefault()` and a
#' capture-phase document listener all failed, Bootstrap having registered its
#' handler first. See the note in `utils_theme.R`.
#'
#' The button keeps the width it had; the "i" takes the gutter next to it, right
#' where the reader is deciding whether to press.
#'
#' @param bouton The action button.
#' @param ... Popover content.
#' @return A flex row.
#' @noRd
.dess_action_info <- function(bouton, ...) {
  htmltools::div(
    class = "d-flex align-items-center gap-2 mb-2",
    bouton,
    info_popover(..., placement = "left"))
}

#' @noRd
mod_desserte_ui <- function(id) {
  ns <- shiny::NS(id)
  i18n <- get_i18n(get_app_options()$language %||% "fr")

  bslib::layout_sidebar(
    # Barre laterale GAUCHE : commandes du CALCUL.
    sidebar = bslib::sidebar(
      id = ns("sidebar"),
      width = 320, open = TRUE, position = "left",
      # Carte repliable, MEME structure que le bloc " Accessibilite " et que
      # " Ingestion terrain " de l'onglet Import terrain : en-tete vert
      # cliquable, icone de l'onglet, chevron. Depliee par defaut - elle porte le
      # bouton " Lancer le calcul ".
      htmltools::tags$div(
        class = "card mb-3",
        htmltools::tags$div(
          class = "card-header bg-success text-white py-2",
          style = "cursor: pointer;",
          `data-bs-toggle` = "collapse",
          `data-bs-target` = paste0("#", ns("dess_collapse")),
          `aria-expanded` = "true",
          `aria-controls` = ns("dess_collapse"),
          htmltools::div(
            class = "d-flex align-items-center justify-content-between",
            htmltools::div(
              class = "d-flex align-items-center",
              bsicons::bs_icon("diagram-3", class = "me-2"),
              i18n$t("tab_terrain_desserte")
            ),
            bsicons::bs_icon("chevron-down", class = "collapse-icon")
          )
        ),
        htmltools::tags$div(
          id = ns("dess_collapse"),
          class = "collapse show",
          htmltools::tags$div(
            class = "card-body",
          # L'avertissement " calcul long " tient dans le " i " du choix de
          # moteur : le glouton trace un A* par CELLULE de parcelle non
          # desservie, donc le temps croit avec la surface de l'emprise et
          # decroit avec `skidding_m`. Il reste signale par un triangle DANS le
          # popover - la mise en garde n'est pas diluee, elle est repliee.
          shiny::radioButtons(
            ns("engine"),
            .dess_label_info(i18n$t("dess_engine_label"),
                             .dess_alerte(i18n$t("dess_slow_help"))),
            choices = stats::setNames(DESSERTE_ENGINES,
                                      c(i18n$t("dess_engine_glouton"),
                                        i18n$t("dess_engine_steiner"))),
            selected = DESSERTE_ENGINES[[1]]),

          # --- Calibrages deplaces vers " Sources & parametres " -----------------
          # Le tampon d'emprise, la distance de debardage, la pente maximale
          # constructible et la tarification de la pente (bareme / terrassement +
          # largeur de plateforme) ont quitte ce sidebar pour l'onglet
          # " Sources & parametres " de la modale des reglages, ou ils sont
          # persistes par projet - meme mouvement que les seuils du Suivi
          # sanitaire (v0.126.2). Ce sont des decisions de MASSIF : ce qu'on
          # acquiert, ce qu'on considere desservi, jusqu'ou l'on construit et
          # comment on chiffre la pente. Seul le choix du moteur reste ici,
          # c'est lui qu'on fait varier d'un essai a l'autre.
          #
          # Le rappel ci-dessous garde les valeurs en vigueur sous les yeux :
          # sans elles, " aucune route a construire " (mesure sur Dabo : 39
          # routes a 100 m de debardage, aucune a 300 m) resterait
          # inintelligible.
          shiny::uiOutput(ns("params_recap")),

          # Empreinte memoire estimee de l'emprise courante : le pic du glouton est
          # previsible a partir de la seule grille (cf. .desserte_memory_check), donc
          # affiche AVANT le clic - un depassement se paie sinon par un OOM au bout
          # d'un quart d'heure de calcul.
          shiny::uiOutput(ns("mem_estimate")),

          .dess_action_info(
            bslib::input_task_button(
              ns("run"), i18n$t("dess_run"),
              label_busy = i18n$t("dess_running"),
              icon = bsicons::bs_icon("play-fill"),
              type = "primary", class = "w-100"),
            i18n$t("dess_intro")),
          shiny::uiOutput(ns("run_status"))
          )
        )
      )
    ),

    bslib::card(
      full_screen = TRUE,
      bslib::card_header(i18n$t("dess_map_title")),
      bslib::layout_sidebar(
        fillable = TRUE,
        sidebar = bslib::sidebar(
          position = "right", open = "always", width = 280,
          # Bilan du reseau cree (badges).
          htmltools::tags$strong(i18n$t("dess_summary_title")),
          shiny::uiOutput(ns("summary")),
          htmltools::tags$hr(class = "my-2"),
          shiny::sliderInput(
            ns("opacity"), i18n$t("dess_opacity"),
            min = 0, max = 1, value = 0.8, step = 0.05, ticks = FALSE),
          htmltools::tags$hr(class = "my-2"),
          # Toutes les actions de la vue sous UN seul en-tete " Tableau des
          # actions ", comme l'onglet Plan d'actions - le bilan et l'opacite
          # ci-dessus n'en sont pas : l'un rend compte, l'autre regle
          # l'affichage. L'accordeon interne reste replie, donc le panneau ne
          # s'allonge pas : on gagne un repere commun, pas six lignes de plus.
          action_table_card(
            ns("dess_actions_collapse"),
            i18n$t("action_plan_actions_title"),
            body_class = "card-body p-2",
          bslib::accordion(
            open = FALSE,
            # Typage du reseau : flux de bois mobilise -> primaire/secondaire/
            # tertiaire (nemeton::volume_mobilisable -> foretaccess::typer_desserte).
            bslib::accordion_panel(
              title = i18n$t("dess_typage_title"),
              value = "typage",
              icon = bsicons::bs_icon("diagram-2"),
              shiny::numericInput(
                ns("typage_taux"), i18n$t("dess_typage_taux"),
                value = 0.5, min = 0, max = 5, step = 0.1),
              shiny::numericInput(
                ns("typage_horizon"), i18n$t("dess_typage_horizon"),
                value = 30, min = 1, max = 200, step = 1),
              # `actionButton` et non `input_task_button` : le typage est
              # synchrone. Sa classe porte donc la couleur DIRECTEMENT - le
              # `type =` de bslib n'existe pas ici, et deviendrait un attribut
              # HTML `type="outline-primary"` sur le `<button>`, c'est-a-dire un
              # type inconnu que le navigateur traite comme " submit ".
              .dess_action_info(
                shiny::actionButton(
                  ns("run_typage"), i18n$t("dess_typage_run"),
                  icon = shiny::icon("diagram-project"),
                  class = "btn-outline-primary btn-sm w-100"),
                i18n$t("dess_typage_intro")),
              shiny::uiOutput(ns("typage_result"))),
            # Integrite du reseau (spec 025). Action SEPAREE et non une etape du
            # calcul : mesure 376,8 s sur Dabo (3 122 troncons) contre 39,7 s
            # pour la creation entiere - l'inclure rendrait " Generer la
            # desserte " dix fois plus lent.
            bslib::accordion_panel(
              title = i18n$t("dess_integrite_title"),
              value = "integrite",
              icon = bsicons::bs_icon("diagram-3-fill"),
              .dess_action_info(
                bslib::input_task_button(
                  ns("run_integrite"), i18n$t("dess_integrite_run"),
                  label_busy = i18n$t("dess_integrite_running"),
                  icon = bsicons::bs_icon("check2-square"),
                  type = "outline-primary", class = "btn-sm w-100"),
                i18n$t("dess_integrite_intro")),
              shiny::uiOutput(ns("integrite_status"))),
            # Optimisation du reseau cree. Action separee : chaque essai est une
            # construction gloutonne complete. Mesure sur Dabo - glouton 82,2 s /
            # cout 16 673 contre multistart 100,2 s / cout 15 002, soit 1,2x le
            # temps pour -10 % de cout.
            bslib::accordion_panel(
              title = i18n$t("dess_optim_title"),
              value = "optim",
              icon = bsicons::bs_icon("stars"),
              shiny::selectInput(
                ns("optim_strategie"), i18n$t("dess_optim_strategie"),
                choices = stats::setNames(
                  DESSERTE_OPTIM_STRATEGIES,
                  c(i18n$t("dess_optim_multistart"), i18n$t("dess_optim_recuit"),
                    i18n$t("dess_optim_riprute"))),
                selected = DESSERTE_OPTIM_STRATEGIES[[1]]),
              shiny::numericInput(ns("optim_n_start"), i18n$t("dess_optim_n_start"),
                                  value = DESSERTE_OPTIM_N_START, min = 2, max = 32, step = 2),
              .dess_action_info(
                bslib::input_task_button(
                  ns("run_optim"), i18n$t("dess_optim_run"),
                  label_busy = i18n$t("dess_optim_running"),
                  icon = bsicons::bs_icon("stars"),
                  type = "outline-primary", class = "btn-sm w-100"),
                i18n$t("dess_optim_intro")),
              shiny::uiOutput(ns("optim_result"))),
            # Complement OSM de la BD TOPO (spec 028).
            bslib::accordion_panel(
              title = i18n$t("dess_osm_title"),
              value = "osm",
              icon = bsicons::bs_icon("signpost-2"),
              .dess_action_info(
                bslib::input_task_button(
                  ns("run_osm"), i18n$t("dess_osm_run"),
                  label_busy = i18n$t("dess_osm_running"),
                  icon = bsicons::bs_icon("cloud-download"),
                  type = "outline-primary", class = "btn-sm w-100"),
                i18n$t("dess_osm_intro")),
              shiny::uiOutput(ns("osm_result"))),
            # Detection de routes absentes de la BD TOPO (dessertR, spec 026).
            # La plus lourde du panneau : mesure 7,91 Go de pic et 189 s SANS
            # nuage LiDAR sur 1 855 ha, et > 10 min avec. D'ou le garde-fou
            # memoire cote service et l'avertissement ci-dessous.
            bslib::accordion_panel(
              title = i18n$t("dess_detect_title"),
              value = "detect",
              icon = bsicons::bs_icon("search"),
              shiny::checkboxInput(ns("detect_lidar"), i18n$t("dess_detect_lidar"),
                                   value = TRUE),
              .dess_action_info(
                bslib::input_task_button(
                  ns("run_detect"), i18n$t("dess_detect_run"),
                  label_busy = i18n$t("dess_detect_running"),
                  icon = bsicons::bs_icon("search"),
                  type = "outline-primary", class = "btn-sm w-100"),
                htmltools::tags$p(i18n$t("dess_detect_intro")),
                .dess_alerte(i18n$t("dess_detect_warn"))),
              shiny::uiOutput(ns("detect_result")))),
          # Exports SORTIS de l'accordeon (parite reGeneration) : un sous-titre
          # " Exports " en h6 et un bouton pleine largeur, directement dans le
          # bloc. Un panneau repliable pour un unique bouton coutait un clic de
          # plus pour rien.
          htmltools::tags$h6(class = "mt-2",
                             i18n$t("action_plan_section_exports")),
          .dess_action_info(
            htmltools::tagAppendAttributes(
              shiny::downloadButton(
                ns("export_gpkg"), i18n$t("dess_download_gpkg"),
                icon = shiny::icon("database"),
                class = "btn-outline-success btn-sm w-100 mb-2"),
              onclick = sprintf("nemetonShowDownloadToast(%s);",
                jsonlite::toJSON(i18n$t("dess_export_gpkg_busy"),
                                 auto_unbox = TRUE))),
            i18n$t("dess_download_gpkg_note")),
          # Le chemin du cache RESTE visible : ce n'est pas une explication
          # qu'on lit une fois, c'est une valeur qu'on copie.
          shiny::uiOutput(ns("cache_path")))
        ),
        leaflet::leafletOutput(ns("map"), height = "72vh")
      )
    )
  )
}

#' @noRd
mod_desserte_server <- function(id, app_state) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    i18n <- get_i18n(get_app_options()$language %||% "fr")

    # ----- Calibrages lus dans les metadonnees du projet ---------------------
    # Tampon, distance de debardage, pente maximale, tarification de la pente :
    # tous regles dans " Sources & parametres ". Un enregistrement la-bas
    # recharge `app_state$current_project`, donc ce reactive s'invalide et le
    # rappel + l'estimation memoire suivent sans intervention.
    dess_params_r <- shiny::reactive({
      project_desserte_params(app_state$current_project$metadata)
    })

    # Rappel des valeurs en vigueur, avec le chemin pour les changer.
    output$params_recap <- shiny::renderUI({
      dp <- dess_params_r()
      meth <- if (identical(dp$methode_pente, "terrassement")) {
        i18n$t("dess_methode_terrassement")
      } else i18n$t("dess_methode_bareme")
      htmltools::div(
        class = "small text-muted border rounded p-2 mb-3",
        htmltools::div(
          class = "fw-semibold mb-1",
          bsicons::bs_icon("sliders", class = "me-1"),
          i18n$t("dess_params_section")),
        htmltools::tags$div(sprintf(
          "%s : %s km \u00b7 %s : %s m \u00b7 %s : %s %%",
          i18n$t("dess_buffer_short"), format(dp$buffer_km),
          i18n$t("dess_skidding_short"), format(dp$skidding_m),
          i18n$t("dess_pente_max_short"), format(dp$pente_max_pct))),
        htmltools::tags$div(sprintf(
          "%s : %s%s", i18n$t("dess_methode_pente"), meth,
          if (identical(dp$methode_pente, "terrassement")) {
            sprintf(" (%s m)", format(dp$largeur_m))
          } else "")),
        htmltools::tags$div(class = "fst-italic mt-1",
                            i18n$t("dess_params_where"))
      )
    })

    rv <- shiny::reactiveValues(result = NULL, running = FALSE, start = NULL)

    .dev_pkg_path <- tryCatch(
      if (isTRUE(pkgload::is_dev_package("nemetonshiny")))
        find.package("nemetonshiny") else NULL,
      error = function(e) NULL)

    # Parcelles a desservir = AOI projet (EPSG:2154), repli indicators_sf ->
    # UGF -> parcelles (helper partage avec l'accessibilite).
    units_sf <- shiny::reactive({
      .resolve_project_aoi_2154(app_state$current_project)
    })

    # Estimation de l'empreinte memoire pour l'emprise courante (parcelles +
    # tampon), recalculee a chaque changement du tampon. Sert d'avertissement
    # amont ; le refus effectif reste cote service (run_desserte), qui est la
    # seule barriere fiable (rule 2 : pas de decision metier dans le module).
    output$mem_estimate <- shiny::renderUI({
      aoi <- units_sf()
      if (is.null(aoi)) return(NULL)
      buffer_m <- dess_params_r()$buffer_km * 1000
      # Le tampon est applique a la BBOX, pas aux geometries : c'est la seule
      # chose dont depend la grille, et ca evite un st_buffer() a chaque frappe.
      mem <- .desserte_memory_check(aoi, res_m = 5, buffer_m = buffer_m)
      if (!is.finite(mem$cells) || !is.finite(mem$bytes)) return(NULL)
      fmt <- function(x, d = 1) formatC(x, format = "f", digits = d, big.mark = "\u202f")
      txt <- sprintf(i18n$t("dess_mem_estimate_fmt"),
                     formatC(mem$cells, format = "d", big.mark = "\u202f"),
                     fmt(mem$bytes / 1024^3),
                     if (is.finite(mem$available)) fmt(mem$available / 1024^3) else "?")
      htmltools::div(
        class = if (isTRUE(mem$ok)) "alert alert-light py-2 small mb-2"
                else "alert alert-danger py-2 small mb-2",
        htmltools::tags$div(txt),
        htmltools::tags$div(
          class = "fw-semibold",
          i18n$t(if (isTRUE(mem$ok)) "dess_mem_ok" else "dess_mem_risk")))
    })

    # --- Worker asynchrone : acquisition + cout + moteur de creation ----------
    dess_task <- shiny::ExtendedTask$new(
      function(aoi_path, engine, cache_dir, buffer_m, skidding_m, methode_pente,
               largeur_m, pente_max_pct, dev_path, app_opts) {
        if (requireNamespace("future", quietly = TRUE)) {
          plan_classes <- class(future::plan())
          if (!any(c("multisession", "multicore", "cluster") %in% plan_classes)) {
            .ensure_async_plan()
          }
        }
        promises::future_promise({
          on.exit(utils::getFromNamespace(".release_worker_memory", "nemetonshiny")(), add = TRUE)
          if (!is.null(dev_path) && requireNamespace("pkgload", quietly = TRUE)) {
            pkgload::load_all(dev_path, quiet = TRUE)
          } else {
            loadNamespace("nemetonshiny")
          }
          options(nemeton.app_options = app_opts)
          utils::getFromNamespace("run_desserte", "nemetonshiny")(aoi_path, engine, cache_dir, buffer_m,
                                      skidding_m = skidding_m,
                                      methode_pente = methode_pente,
                                      largeur_m = largeur_m,
                                      pente_max_pct = pente_max_pct)
        }, seed = TRUE)
      })

    bslib::bind_task_button(dess_task, "run")

    # --- Lancement -------------------------------------------------------------
    # ORCHESTRATION - meme motif que mod_accessibility : corps extrait pour
    # que le bouton de l'onglet et le lancement enchaine partagent le meme
    # chemin, gardes comprises.
    .lancer_desserte <- function() {
      if (isTRUE(rv$running)) {
        shiny::showNotification(i18n$t("dess_busy_already"), type = "warning",
                                duration = 5)
        return()
      }
      if (deny_if_readonly(app_state, i18n)) {
        bslib::update_task_button("run", state = "ready")
        return()
      }
      project_path <- tryCatch(app_state$current_project$path,
                               error = function(e) NULL)
      aoi <- units_sf()
      if (is.null(aoi) || is.null(project_path)) {
        bslib::update_task_button("run", state = "ready")
        shiny::showNotification(i18n$t("dess_need_project"), type = "warning")
        return()
      }
      engine <- intersect(input$engine %||% character(0), DESSERTE_ENGINES)[1]
      if (is.na(engine) || length(engine) == 0L) {
        bslib::update_task_button("run", state = "ready")
        shiny::showNotification(i18n$t("dess_need_engine"), type = "warning")
        return()
      }
      # AOI passee au worker PAR FICHIER (pointeur externe sf non serialisable).
      cache_dir <- .desserte_cache_dir(project_path)
      dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
      aoi_path <- file.path(cache_dir, "aoi_input.gpkg")
      ok <- tryCatch({
        sf::st_write(aoi, aoi_path, layer = "parcelles", quiet = TRUE,
                     delete_dsn = TRUE)
        TRUE
      }, error = function(e) FALSE)
      if (!isTRUE(ok)) {
        bslib::update_task_button("run", state = "ready")
        shiny::showNotification(i18n$t("dess_need_project"), type = "warning")
        return()
      }

      rv$running <- TRUE
      rv$start <- Sys.time()
      shiny::showNotification(
        .running_notif_content(i18n$t("dess_running"), rv$start),
        id = session$ns("dess_notif"), type = "message", duration = NULL)
      dp <- dess_params_r()
      buffer_m <- dp$buffer_km * 1000
      params <- .desserte_params_projet(dp)
      skidding_m <- params$skidding_m
      tryCatch(
        dess_task$invoke(aoi_path, engine, cache_dir, buffer_m, skidding_m,
                         params$methode_pente, params$largeur_m,
                         params$pente_max_pct,
                         .dev_pkg_path, get_app_options()),
        error = function(e) {
          rv$running <- FALSE
          rv$start <- NULL
          shiny::removeNotification(session$ns("dess_notif"))
          bslib::update_task_button("run", state = "ready")
          shiny::showNotification(
            paste0(i18n$t("desserte_engine_failed"), " \u2014 ",
                   .strip_ansi(conditionMessage(e))),
            type = "error", duration = NULL)
        })
      invisible(TRUE)
    }

    shiny::observeEvent(input$run, .lancer_desserte())

    # --- Lancement enchaine : etape « desserte » -----------------------
    pipeline_req <- shiny::reactiveVal(NULL)

    shiny::observeEvent(app_state$pipeline_request, {
      req <- app_state$pipeline_request
      if (!pipeline_targets(req, "desserte")) return()

      if (is.null(units_sf()) ||
          is.null(tryCatch(app_state$current_project$path, error = function(e) NULL))) {
        pipeline_answer(app_state, req, "skipped", i18n$t("pipeline_no_project"))
        return()
      }
      if (isTRUE(rv$running)) {
        pipeline_answer(app_state, req, "skipped", i18n$t("pipeline_skip_busy"))
        return()
      }
      pipeline_req(req)
      if (!isTRUE(.lancer_desserte())) {
        pipeline_req(NULL)
        pipeline_answer(app_state, req, "skipped", i18n$t("pipeline_skip_not_started"))
      }
    })

    # Libelle " en cours " enrichi de la phase publiee par le worker sur le canal
    # disque. Le moteur glouton peut tourner des dizaines de minutes sur de
    # grandes parcelles : sans la phase, l'utilisateur ne voit qu'un chrono et
    # conclut que rien ne se passe.
    dess_running_label <- function() {
      pp <- tryCatch(app_state$current_project$path, error = function(e) NULL)
      ph <- .dess_read_phase(pp)
      base <- i18n$t("dess_running")
      if (is.null(ph)) return(base)
      i <- match(ph, DESSERTE_PHASES)
      lbl <- i18n$t(paste0("dess_phase_", ph))
      if (is.na(i)) return(paste0(base, " \u2014 ", lbl))
      sprintf("%s \u2014 %s (%d/%d)", base, lbl, i, length(DESSERTE_PHASES))
    }

    # Rafraichit le chrono ET la phase de la notif persistante tant que le
    # worker tourne.
    shiny::observe({
      if (!isTRUE(rv$running)) return()
      shiny::invalidateLater(1000)
      shiny::showNotification(
        .running_notif_content(dess_running_label(), shiny::isolate(rv$start)),
        id = session$ns("dess_notif"), type = "message", duration = NULL)
    })

    output$run_status <- shiny::renderUI({
      if (!isTRUE(rv$running) || is.null(rv$start)) return(NULL)
      shiny::invalidateLater(1000)
      htmltools::div(
        class = "small text-info mt-1 text-center",
        .running_notif_content(dess_running_label(), rv$start))
    })
    shiny::outputOptions(output, "run_status", suspendWhenHidden = FALSE)

    # --- Fin de tache ----------------------------------------------------------
    shiny::observeEvent(dess_task$status(), {
      st <- dess_task$status()
      if (!identical(st, "success") && !identical(st, "error")) return()
      rv$running <- FALSE
      rv$start <- NULL
      shiny::removeNotification(session$ns("dess_notif"))
      # Retire le canal de phase : un `engine_status.json` laisse sur disque
      # ferait afficher une phase perimee au prochain lancement, avant que le
      # worker n'ait publie la sienne.
      tryCatch({
        pp <- app_state$current_project$path
        if (!is.null(pp)) unlink(file.path(pp, "cache", "desserte",
                                           "engine_status.json"))
      }, error = function(e) invisible(NULL))

      res <- tryCatch(dess_task$result(), error = function(e) {
        list(status = "error", reason = "desserte_engine_failed",
             detail = conditionMessage(e))
      })
      ok_moteur <- is.list(res) && identical(res$status, "success")
      req_pipeline <- shiny::isolate(pipeline_req())
      if (!is.null(req_pipeline)) {
        pipeline_answer(app_state, req_pipeline,
                        if (ok_moteur) "ok" else "error",
                        if (ok_moteur) NULL
                        else i18n$t(tryCatch(res$reason, error = function(e) NULL) %||%
                                      "desserte_engine_failed"))
        pipeline_req(NULL)
      }
      if (!ok_moteur) {
        reason <- tryCatch(res$reason, error = function(e) NULL) %||%
          "desserte_engine_failed"
        msg <- i18n$t(reason)
        detail <- tryCatch(res$detail, error = function(e) NULL)
        if (!is.null(detail) && nzchar(detail)) {
          msg <- paste0(msg, " \u2014 ", .strip_ansi(as.character(detail)))
        }
        shiny::showNotification(msg, type = "error", duration = NULL)
        return()
      }
      # Recharger depuis le cache disque (chemins + sidecar de scalaires).
      project_path <- tryCatch(app_state$current_project$path,
                               error = function(e) NULL)
      rv$result <- .load_cached_desserte(project_path, .desserte_params_projet(dess_params_r())) %||% res
      # Zero route creee est un SUCCES, pas un resultat vide : a `skidding_m`
      # realiste, une foret bien desservie n'a rien a construire (mesure sur
      # Dabo : 39 routes a 100 m, aucune a 300 m). Sans message dedie,
      # l'utilisateur lirait " desserte creee " devant une carte sans route.
      nr <- suppressWarnings(as.integer(res$n_routes %||% NA_integer_))
      shiny::showNotification(
        if (!is.na(nr) && nr == 0L) {
          sprintf(i18n$t("dess_done_none_fmt"),
                  suppressWarnings(as.numeric(res$skidding_m %||% NA_real_)))
        } else {
          sprintf(i18n$t("dess_done_fmt"),
                  res$n_desservies %||% NA_integer_, res$n_parcelles %||% NA_integer_)
        },
        type = "message", duration = 8)
    })

    # Restaure un reseau DEJA calcule depuis le cache - PARESSEUSEMENT : lecture au
    # premier affichage de l'onglet Desserte seulement (une fois par projet), pour
    # que le clic sur un projet recent reste rapide. Observer unique (main_nav +
    # terrain_nav + projet), meme patron que mod_accessibility.
    dess_loaded_for <- shiny::reactiveVal(NULL)
    shiny::observeEvent(
      list(app_state$active_main_tab, app_state$active_terrain_tab,
           app_state$current_project),
      {
        project_path <- tryCatch(app_state$current_project$path,
                                 error = function(e) NULL)
        key <- project_path %||% ""
        if (identical(dess_loaded_for(), key)) return()
        on_tab <- identical(app_state$active_main_tab, "terrain") &&
          identical(app_state$active_terrain_tab, "desserte")
        if (!on_tab) {
          rv$result <- NULL
          return()
        }
        dess_loaded_for(key)
        cached <- tryCatch(
          .load_cached_desserte(project_path, .desserte_params_projet(dess_params_r())),
          error = function(e) NULL)
        rv$result <- cached
        if (!is.null(cached)) {
          shiny::showNotification(i18n$t("dess_cache_loaded"), type = "message",
                                  duration = 5)
        }
      }, ignoreNULL = FALSE)

    # --- Badges du reseau cree -------------------------------------------------
    output$summary <- shiny::renderUI({
      res <- rv$result
      if (is.null(res) || !identical(res$status %||% "success", "success")) {
        return(htmltools::tags$p(class = "text-muted small",
                                 i18n$t("dess_no_result_yet")))
      }
      # Chaque badge porte le " i " de l'app : un chiffre sans unite ni regle de
      # lecture ne se comprend pas (un cout dans l'unite du raster, des
      # infractions au sens d'une annexe reglementaire, un " 0 " qui est un
      # succes et non un echec). Le libelle n'est pas un <label> de controle :
      # `info_popover()` suffit, sans neutraliser l'activation.
      badge <- function(label, value, cls = "bg-secondary", info = NULL) {
        htmltools::div(class = "d-flex justify-content-between align-items-center mb-1",
          htmltools::tags$span(
            class = "small", label,
            if (!is.null(info)) htmltools::tagList(" ", info_popover(info))),
          htmltools::tags$span(class = paste("badge", cls), value))
      }
      nd <- res$n_desservies %||% NA_integer_
      np <- res$n_parcelles %||% NA_integer_
      # `raccorde` (foretaccess >= 1.11) est le VRAI indicateur qualite : " toutes
      # les routes creees sont-elles rattachees au reseau existant ? ". On l'affiche
      # a la place de `connexe` (presque toujours FALSE car domine par la
      # fragmentation du reseau existant - trompeur pour l'utilisateur).
      raccorde <- res$raccorde %||% NA
      cout <- res$cout %||% NA_real_
      nroutes <- suppressWarnings(as.integer(res$n_routes %||% NA_integer_))
      integ <- res$integrite
      htmltools::tagList(
        badge(i18n$t("dess_badge_desservies"),
              if (is.na(nd) || is.na(np)) "\u2014" else sprintf("%d / %d", nd, np),
              if (!is.na(nd) && !is.na(np) && nd >= np) "bg-success" else "bg-warning",
              info = i18n$t("dess_badge_info_desservies")),
        badge(i18n$t("dess_badge_raccorde"),
              if (is.na(raccorde)) "\u2014" else if (isTRUE(raccorde)) i18n$t("dess_yes") else i18n$t("dess_no"),
              if (isTRUE(raccorde)) "bg-success" else "bg-warning",
              info = i18n$t("dess_badge_info_raccorde")),
        badge(i18n$t("dess_badge_routes"),
              if (is.na(nroutes)) "\u2014" else format(nroutes, big.mark = " "),
              if (!is.na(nroutes) && nroutes == 0L) "bg-success" else "bg-secondary",
              info = i18n$t("dess_badge_info_routes")),
        badge(i18n$t("dess_badge_cout"),
              if (is.na(cout)) "\u2014" else format(round(cout), big.mark = " "),
              info = i18n$t("dess_badge_info_cout")),
        # Integrite du reseau OBTENU (existant union cree), spec 025. Complete
        # `raccorde`, qui ne dit que " les routes creees sont-elles rattachees ? "
        # et reste muet sur la coherence du graphe resultant. Absent = controle
        # indisponible (dessertR injoignable), surtout PAS " 0 infraction ".
        if (is.null(integ)) {
          badge(i18n$t("dess_badge_integrite"), i18n$t("dess_integrite_na"),
                "bg-light text-dark", info = i18n$t("dess_badge_info_integrite"))
        } else {
          htmltools::tagList(
            badge(i18n$t("dess_badge_infractions"),
                  format(integ$n_infractions, big.mark = " "),
                  if (isTRUE(integ$n_infractions == 0L)) "bg-success" else "bg-warning",
                  info = i18n$t("dess_badge_info_infractions")),
            badge(i18n$t("dess_badge_orphelins"),
                  sprintf("%s / %s",
                          format(integ$n_composants_orphelins, big.mark = " "),
                          format(integ$n_composants, big.mark = " ")),
                  if (isTRUE(integ$n_composants_orphelins == 0L)) "bg-success" else "bg-warning",
                  info = i18n$t("dess_badge_info_orphelins")))
        },
        if (!is.na(nroutes) && nroutes == 0L) {
          htmltools::div(class = "alert alert-success py-2 small mt-2 mb-0",
                         i18n$t("dess_no_road_needed"))
        })
    })

    # --- Carte : fonds + parcelles + desserte existante + reseau cree (raster) -
    output$map <- leaflet::renderLeaflet({
      aoi <- units_sf()
      geo <- if (!is.null(aoi)) {
        tryCatch(sf::st_transform(aoi, 4326), error = function(e) NULL)
      }
      # Fond relief CVAT (overlay semi-transparent) quand un CVAT existe deja pour
      # le projet - meme helper que la carte Accessibilite.
      project_path <- tryCatch(app_state$current_project$path, error = function(e) NULL)
      cvat_bg <- .acc_cvat_overlay_raster(project_path)
      # " Lignes creees " est declare AVEC les autres, systematiquement : un
      # groupe peint mais non declare n'a pas de case pour l'eteindre (cf. le
      # relief de la carte Accessibilite, corrige en 0.122.6).
      overlays <- c(if (!is.null(geo)) DESS_GROUPE_PARCELLES else NULL,
                    if (!is.null(cvat_bg)) "Relief CVAT" else NULL,
                    DESS_GROUPE_EXISTANTE, DESS_GROUPE_RESEAU, DESS_GROUPE_LIGNES,
                    DESS_GROUPE_TYPE, DESS_GROUPE_OSM, DESS_GROUPE_DETECTEE,
                    PLACES_DEPOT_GROUP)
      m <- leaflet::leaflet() |>
        leaflet::addProviderTiles("OpenStreetMap", group = "OSM") |>
        leaflet::addProviderTiles("Esri.WorldImagery", group = "Satellite") |>
        leaflet::addMapPane("nemetonCvatBase", zIndex = 230) |>
        leaflet::addMapPane("nemetonDessRaster", zIndex = 250) |>
        # Les lignes AU-DESSUS du raster qui les a produites : sinon la
        # grille en escalier masque le trace qu'elle a servi a calculer.
        leaflet::addMapPane("nemetonDessLignes", zIndex = 420) |>
        leaflet::addLayersControl(
          baseGroups = c("OSM", "Satellite"),
          overlayGroups = overlays,
          options = leaflet::layersControlOptions(collapsed = TRUE)) |>
        # OSM et detection partent ETEINTS : ce sont des diagnostics, pas le
        # resultat de l'onglet. Les allumer d'office noierait le reseau concu
        # sous des centaines de troncons OSM (544 sur Dabo, contre 39 routes
        # creees). La logique `shown` des observers respecte ensuite le choix
        # de l'utilisateur - `hideGroup()` ici n'agit qu'a la creation.
        leaflet::hideGroup(c(DESS_GROUPE_OSM, DESS_GROUPE_DETECTEE))
      if (!is.null(cvat_bg)) {
        grey <- leaflet::colorNumeric(grDevices::grey.colors(64, 0, 1),
          domain = c(0, 1), na.color = "transparent")
        m <- leaflet::addRasterImage(m, cvat_bg, colors = grey, opacity = 0.6,
          group = "Relief CVAT", maxBytes = 16 * 1024^2,
          options = leaflet::gridOptions(pane = "nemetonCvatBase"))
      }
      if (!is.null(geo)) {
        m <- leaflet::addPolygons(m, data = geo, group = DESS_GROUPE_PARCELLES,
          color = "#1f78b4", weight = 2, opacity = 0.9, fillOpacity = 0)
        bb <- tryCatch(as.numeric(sf::st_bbox(geo)), error = function(e) NULL)
        if (!is.null(bb) && all(is.finite(bb))) {
          m <- leaflet::fitBounds(m, bb[1], bb[2], bb[3], bb[4])
        }
      }
      m
    })
    shiny::outputOptions(output, "map", suspendWhenHidden = FALSE)

    opacity_d <- shiny::debounce(
      shiny::reactive(suppressWarnings(as.numeric(input$opacity)) %||% 0.8), 250)

    # Overlay du reseau cree (raster) via leafletProxy : peint dans le pane dedie
    # `nemetonDessRaster`, stable au changement de fond. Raster masque 1 = route.
    shiny::observe({
      res <- rv$result
      op <- opacity_d()
      # `isolate()` : leaflet renvoie cet input a chaque ajout/retrait de
      # groupe, et cet observe en ajoute - une lecture reactive le rendrait
      # auto-declenchant (peintures multiples). Cf. mod_accessibility.
      shown <- shiny::isolate(input$map_groups)
      proxy <- leaflet::leafletProxy("map") |> leaflet::clearGroup(DESS_GROUPE_RESEAU)
      rp <- tryCatch(res$reseau_path, error = function(e) NULL)
      if (is.null(rp) || !file.exists(rp)) return()
      rast <- tryCatch(terra::rast(rp), error = function(e) NULL)
      if (is.null(rast)) return()
      cmap <- leaflet::colorFactor("#B71C1C", domain = 1, na.color = "transparent")
      proxy |>
        leaflet::addRasterImage(rast, colors = cmap, opacity = op, method = "ngb",
          group = DESS_GROUPE_RESEAU,
          options = leaflet::gridOptions(pane = "nemetonDessRaster"))
      if (!is.null(shown) && !(DESS_GROUPE_RESEAU %in% shown)) {
        leaflet::hideGroup(proxy, DESS_GROUPE_RESEAU)
      }
    })

    # Overlay " Lignes creees " : le resultat VECTORIEL du moteur, lu depuis la
    # couche `reseau_cree` du GeoPackage - deja ecrite par `run_desserte()`,
    # elle n'etait simplement jamais peinte.
    #
    # Pourquoi en plus du raster et non a sa place : le raster est le support du
    # CALCUL (le moteur trace un chemin de cellules) et se lit en escalier au
    # zoom ; les lignes portent en revanche les attributs par route - `ordre` de
    # creation, `cout`, `longueur` - qu'une grille ne peut pas porter. Les deux
    # disent la meme chose a des echelles differentes, d'ou deux cases.
    shiny::observe({
      res <- rv$result
      shown <- shiny::isolate(input$map_groups)
      proxy <- leaflet::leafletProxy("map") |> leaflet::clearGroup(DESS_GROUPE_LIGNES)
      gp <- tryCatch(res$gpkg_path, error = function(e) NULL)
      if (is.null(gp) || !file.exists(gp)) return()
      lyr <- tryCatch(sf::st_layers(gp)$name, error = function(e) character(0))
      if (!("reseau_cree" %in% lyr)) return()
      d <- tryCatch(sf::st_read(gp, layer = "reseau_cree", quiet = TRUE),
                    error = function(e) NULL)
      if (!inherits(d, "sf") || nrow(d) == 0L) return()
      d <- tryCatch(sf::st_transform(d, 4326), error = function(e) d)
      # Infobulle par route : l'ordre de creation dit l'arbitrage du moteur
      # (il dessert d'abord ce qui rapporte le plus), le cout et la longueur
      # disent ce qu'elle vaut. C'est l'interet du vecteur sur le raster.
      lbl <- tryCatch({
        ordre <- suppressWarnings(as.integer(d[["ordre"]]))
        long <- suppressWarnings(as.numeric(d[["longueur"]]))
        cout <- suppressWarnings(as.numeric(d[["cout"]]))
        ifelse(is.finite(ordre) & is.finite(long),
               sprintf(i18n$t("dess_ligne_label_fmt"), ordre, long, cout),
               i18n$t("dess_ligne_label_court"))
      }, error = function(e) NULL)
      proxy |>
        leaflet::addPolylines(data = d, group = DESS_GROUPE_LIGNES,
          color = "#FF6F00", weight = 3, opacity = 0.95,
          label = lbl,
          options = leaflet::pathOptions(pane = "nemetonDessLignes"))
      if (!is.null(shown) && !(DESS_GROUPE_LIGNES %in% shown)) {
        leaflet::hideGroup(proxy, DESS_GROUPE_LIGNES)
      }
    })

    # Overlay " Desserte existante " (reseau a raccorder), lu depuis le GPKG.
    shiny::observe({
      res <- rv$result
      shown <- shiny::isolate(input$map_groups)
      proxy <- leaflet::leafletProxy("map") |> leaflet::clearGroup(DESS_GROUPE_EXISTANTE)
      gp <- tryCatch(res$gpkg_path, error = function(e) NULL)
      if (is.null(gp) || !file.exists(gp)) return()
      d <- tryCatch(sf::st_read(gp, layer = "desserte_existante", quiet = TRUE),
                    error = function(e) NULL)
      if (!inherits(d, "sf") || nrow(d) == 0L) return()
      d <- tryCatch(sf::st_transform(d, 4326), error = function(e) d)
      proxy |>
        leaflet::addPolylines(data = d, group = DESS_GROUPE_EXISTANTE,
          color = "#37474F", weight = 1.5, opacity = 0.7)
      if (!is.null(shown) && !(DESS_GROUPE_EXISTANTE %in% shown)) {
        leaflet::hideGroup(proxy, DESS_GROUPE_EXISTANTE)
      }
    })

    # Overlay " Places de depot " : points calcules par la correction LiDAR de la
    # desserte cote Accessibilite (couche `places_depot` du GeoPackage
    # d'accessibilite du projet). Affiches ici aussi pour situer les depots vis-a-vis
    # du reseau cree/type. Se relit a l'arrivee sur l'onglet (active_terrain_tab).
    shiny::observe({
      app_state$active_terrain_tab  # dependance : relire en arrivant sur l'onglet
      shown <- shiny::isolate(input$map_groups)
      proxy <- leaflet::leafletProxy("map") |>
        leaflet::clearGroup(PLACES_DEPOT_GROUP)
      project_path <- tryCatch(app_state$current_project$path, error = function(e) NULL)
      pd <- .acc_read_places_depot(.accessibility_gpkg_path(project_path))
      if (is.null(pd)) return()
      proxy |>
        leaflet::addCircleMarkers(data = pd, group = PLACES_DEPOT_GROUP,
          radius = 5, color = "#B71C1C", weight = 1, fillColor = "#E53935",
          fillOpacity = 0.85, label = i18n$t("acc_places_depot"))
      if (!is.null(shown) && !(PLACES_DEPOT_GROUP %in% shown)) {
        leaflet::hideGroup(proxy, PLACES_DEPOT_GROUP)
      }
    })

    # --- Typage du reseau (flux de bois mobilise) ------------------------------
    # Chaine nemeton::volume_mobilisable(m3_total) -> foretaccess::calculer_flux ->
    # typer_desserte, sur l'objet reseau persiste par le run desserte. Calcul court
    # (le glouton n'est PAS relance) : a la demande avec notification.
    rv_typage <- shiny::reactiveVal(NULL)
    # --- Integrite du reseau : worker dedie (376,8 s mesures sur Dabo) ---------
    # Asynchrone obligatoirement : le typage voisin tourne en synchrone, ce qui
    # est tenable pour lui (quelques secondes) mais gelerait toute l'app ici.
    integ_start <- shiny::reactiveVal(NULL)
    integ_task <- shiny::ExtendedTask$new(
      function(cache_dir, aoi_path, dev_path, app_opts) {
        if (requireNamespace("future", quietly = TRUE)) {
          pc <- class(future::plan())
          if (!any(c("multisession", "multicore", "cluster") %in% pc)) .ensure_async_plan()
        }
        promises::future_promise({
          on.exit(utils::getFromNamespace(".release_worker_memory", "nemetonshiny")(), add = TRUE)
          if (!is.null(dev_path) && requireNamespace("pkgload", quietly = TRUE)) {
            pkgload::load_all(dev_path, quiet = TRUE)
          } else {
            loadNamespace("nemetonshiny")
          }
          options(nemeton.app_options = app_opts)
          utils::getFromNamespace("run_desserte_integrite", "nemetonshiny")(cache_dir, aoi_path)
        }, seed = TRUE)
      })
    bslib::bind_task_button(integ_task, "run_integrite")

    # ORCHESTRATION - controle d'integrite du reseau. Lit le cache que le
    # moteur desserte a rempli : vient donc apres lui dans la chaine.
    .lancer_integrite <- function() {
      project_path <- tryCatch(app_state$current_project$path, error = function(e) NULL)
      if (is.null(project_path)) {
        bslib::update_task_button("run_integrite", state = "ready")
        shiny::showNotification(i18n$t("dess_need_project"), type = "warning")
        return()
      }
      cache_dir <- .desserte_cache_dir(project_path)
      integ_start(Sys.time())
      shiny::showNotification(
        .running_notif_content(i18n$t("dess_integrite_running"), integ_start()),
        id = session$ns("integ_notif"), type = "message", duration = NULL)
      integ_task$invoke(cache_dir, file.path(cache_dir, "aoi_input.gpkg"),
                        .dev_pkg_path, get_app_options())
      invisible(TRUE)
    }

    shiny::observeEvent(input$run_integrite, .lancer_integrite())

    # --- Lancement enchaine : etape « desserte_integrite » -------------
    integ_pipeline_req <- shiny::reactiveVal(NULL)

    shiny::observeEvent(app_state$pipeline_request, {
      req <- app_state$pipeline_request
      if (!pipeline_targets(req, "desserte_integrite")) return()
      if (is.null(tryCatch(app_state$current_project$path, error = function(e) NULL))) {
        pipeline_answer(app_state, req, "skipped", i18n$t("pipeline_no_project"))
        return()
      }
      if (identical(integ_task$status(), "running")) {
        pipeline_answer(app_state, req, "skipped", i18n$t("pipeline_skip_busy"))
        return()
      }
      integ_pipeline_req(req)
      if (!isTRUE(.lancer_integrite())) {
        integ_pipeline_req(NULL)
        pipeline_answer(app_state, req, "skipped", i18n$t("pipeline_skip_not_started"))
      }
    })

    shiny::observe({
      st <- integ_task$status()
      req <- shiny::isolate(integ_pipeline_req())
      if (is.null(req) || !st %in% c("success", "error")) return()
      pipeline_answer(app_state, req,
                      if (identical(st, "success")) "ok" else "error",
                      if (identical(st, "success")) NULL else i18n$t("error"))
      integ_pipeline_req(NULL)
    })

    # Tick 1 s : chrono de la notif d'integrite.
    shiny::observe({
      if (is.null(integ_start())) return()
      shiny::invalidateLater(1000)
      shiny::showNotification(
        .running_notif_content(i18n$t("dess_integrite_running"),
                               shiny::isolate(integ_start())),
        id = session$ns("integ_notif"), type = "message", duration = NULL)
    })

    shiny::observeEvent(integ_task$status(), {
      st <- integ_task$status()
      if (!st %in% c("success", "error")) return()
      integ_start(NULL)
      shiny::removeNotification(session$ns("integ_notif"))
      res <- tryCatch(integ_task$result(), error = function(e) {
        list(status = "error", reason = "desserte_integrite_failed")
      })
      if (!is.list(res) || !identical(res$status, "success")) {
        shiny::showNotification(i18n$t(res$reason %||% "desserte_integrite_failed"),
                                type = "error", duration = NULL)
        return()
      }
      # Reinjecte dans le resultat courant pour que les badges se rafraichissent.
      cur <- rv$result
      if (is.list(cur)) { cur$integrite <- res$integrite; rv$result <- cur }
      shiny::showNotification(i18n$t("dess_integrite_done"), type = "message",
                              duration = 6)
    })

    output$integrite_status <- shiny::renderUI({
      res <- rv$result
      if (is.null(res) || !identical(res$status %||% "success", "success")) {
        return(htmltools::tags$p(class = "text-muted small",
                                 i18n$t("dess_integrite_hint")))
      }
      if (is.null(res$integrite)) {
        return(htmltools::tags$p(class = "text-muted small",
                                 i18n$t("dess_integrite_hint")))
      }
      NULL   # resultat rendu par les badges du bilan
    })

    # --- Optimisation et complement OSM : deux workers du meme patron ---------
    # Facteur commun : action separee + notif engrenage/chrono + sidecar relu.
    .async_panel <- function(id_btn, notif_id, label_key, invoke_fun, on_success) {
      start <- shiny::reactiveVal(NULL)
      task <- shiny::ExtendedTask$new(function(...) {
        args <- list(...)
        if (requireNamespace("future", quietly = TRUE)) {
          pc <- class(future::plan())
          if (!any(c("multisession", "multicore", "cluster") %in% pc)) .ensure_async_plan()
        }
        dev_path <- .dev_pkg_path; app_opts <- get_app_options()
        promises::future_promise({
          on.exit(utils::getFromNamespace(".release_worker_memory", "nemetonshiny")(), add = TRUE)
          if (!is.null(dev_path) && requireNamespace("pkgload", quietly = TRUE)) {
            pkgload::load_all(dev_path, quiet = TRUE)
          } else {
            loadNamespace("nemetonshiny")
          }
          options(nemeton.app_options = app_opts)
          do.call(invoke_fun, args)
        }, seed = TRUE)
      })
      bslib::bind_task_button(task, id_btn)
      shiny::observe({
        if (is.null(start())) return()
        shiny::invalidateLater(1000)
        shiny::showNotification(
          .running_notif_content(i18n$t(label_key), shiny::isolate(start())),
          id = session$ns(notif_id), type = "message", duration = NULL)
      })
      shiny::observeEvent(task$status(), {
        st <- task$status()
        if (!st %in% c("success", "error")) return()
        start(NULL)
        shiny::removeNotification(session$ns(notif_id))
        res <- tryCatch(task$result(), error = function(e) NULL)
        if (!is.list(res) || !identical(res$status, "success")) {
          shiny::showNotification(
            i18n$t(tryCatch(res$reason, error = function(e) NULL) %||%
                     "desserte_engine_failed"),
            type = "error", duration = NULL)
          return()
        }
        on_success(res)
      })
      list(task = task, start = start)
    }

    rv_optim <- shiny::reactiveVal(NULL)
    optim_panel <- .async_panel(
      "run_optim", "optim_notif", "dess_optim_running",
      function(...) utils::getFromNamespace("run_desserte_optimiser", "nemetonshiny")(...),
      function(res) {
        rv_optim(res)
        shiny::showNotification(i18n$t("dess_optim_done"), type = "message", duration = 6)
      })
    shiny::observeEvent(input$run_optim, {
      pp <- tryCatch(app_state$current_project$path, error = function(e) NULL)
      if (is.null(pp)) {
        bslib::update_task_button("run_optim", state = "ready")
        shiny::showNotification(i18n$t("dess_need_project"), type = "warning"); return()
      }
      cd <- .desserte_cache_dir(pp)
      bm <- dess_params_r()$buffer_km * 1000
      sk <- dess_params_r()$skidding_m
      ns_ <- suppressWarnings(as.integer(input$optim_n_start))
      if (!isTRUE(is.finite(ns_)) || ns_ < 2L) ns_ <- DESSERTE_OPTIM_N_START
      optim_panel$start(Sys.time())
      shiny::showNotification(
        .running_notif_content(i18n$t("dess_optim_running"), optim_panel$start()),
        id = session$ns("optim_notif"), type = "message", duration = NULL)
      optim_panel$task$invoke(cd, file.path(cd, "aoi_input.gpkg"),
                              input$optim_strategie, ns_,
                              DESSERTE_OPTIM_N_ITER, bm, sk)
    })
    output$optim_result <- shiny::renderUI({
      r <- rv_optim() %||% tryCatch(
        .load_cached_optim(.desserte_cache_dir(app_state$current_project$path)),
        error = function(e) NULL)
      if (is.null(r)) {
        return(htmltools::tags$p(class = "text-muted small", i18n$t("dess_optim_hint")))
      }
      base <- suppressWarnings(as.numeric(rv$result$cout))
      gain <- if (is.finite(base) && base > 0 && is.finite(r$cout)) {
        sprintf(" (%+.1f %%)", 100 * (r$cout - base) / base)
      } else ""
      htmltools::div(class = "small",
        htmltools::tags$div(sprintf("%s : %s%s", i18n$t("dess_badge_cout"),
                                    format(round(r$cout), big.mark = " "), gain)),
        htmltools::tags$div(sprintf("%s : %s", i18n$t("dess_badge_routes"),
                                    format(r$n_routes, big.mark = " "))))
    })

    rv_osm <- shiny::reactiveVal(NULL)
    osm_panel <- .async_panel(
      "run_osm", "osm_notif", "dess_osm_running",
      function(...) utils::getFromNamespace("run_desserte_osm", "nemetonshiny")(...),
      function(res) {
        rv_osm(res)
        shiny::showNotification(sprintf(i18n$t("dess_osm_done_fmt"), res$n_osm),
                                type = "message", duration = 6)
      })
    shiny::observeEvent(input$run_osm, {
      pp <- tryCatch(app_state$current_project$path, error = function(e) NULL)
      if (is.null(pp)) {
        bslib::update_task_button("run_osm", state = "ready")
        shiny::showNotification(i18n$t("dess_need_project"), type = "warning"); return()
      }
      cd <- .desserte_cache_dir(pp)
      bm <- dess_params_r()$buffer_km * 1000
      osm_panel$start(Sys.time())
      shiny::showNotification(
        .running_notif_content(i18n$t("dess_osm_running"), osm_panel$start()),
        id = session$ns("osm_notif"), type = "message", duration = NULL)
      osm_panel$task$invoke(cd, file.path(cd, "aoi_input.gpkg"), bm)
    })
    # Resultat courant OU sidecar relu : un seul point de verite, partage par le
    # panneau et par le calque. Les deux lisaient le cache chacun de leur cote,
    # et un calque qui relit le disque quand le panneau ne le relit pas se
    # desynchronise en silence.
    osm_res <- shiny::reactive({
      rv_osm() %||% tryCatch(
        .load_cached_osm(.desserte_cache_dir(app_state$current_project$path)),
        error = function(e) NULL)
    })

    output$osm_result <- shiny::renderUI({
      r <- osm_res()
      if (is.null(r)) {
        return(htmltools::tags$p(class = "text-muted small", i18n$t("dess_osm_hint")))
      }
      rows <- if (is.list(r$resume) && length(r$resume)) {
        lapply(names(r$resume), function(k) htmltools::tags$tr(
          htmltools::tags$td(class = "small", k),
          htmltools::tags$td(class = "small text-end",
                             format(r$resume[[k]], big.mark = " "))))
      } else NULL
      htmltools::tagList(
        htmltools::tags$div(class = "small mb-1",
                            sprintf(i18n$t("dess_osm_done_fmt"), r$n_osm)),
        if (!is.null(rows)) htmltools::tags$table(
          class = "table table-sm table-striped small mb-0",
          htmltools::tags$tbody(rows)))
    })

    rv_detect <- shiny::reactiveVal(NULL)
    detect_panel <- .async_panel(
      "run_detect", "detect_notif", "dess_detect_running",
      function(...) utils::getFromNamespace("run_desserte_detection", "nemetonshiny")(...),
      function(res) {
        rv_detect(res)
        shiny::showNotification(sprintf(i18n$t("dess_detect_done_fmt"), res$n_detecte),
                                type = "message", duration = 8)
      })
    shiny::observeEvent(input$run_detect, {
      pp <- tryCatch(app_state$current_project$path, error = function(e) NULL)
      if (is.null(pp)) {
        bslib::update_task_button("run_detect", state = "ready")
        shiny::showNotification(i18n$t("dess_need_project"), type = "warning"); return()
      }
      cd <- .desserte_cache_dir(pp)
      bm <- dess_params_r()$buffer_km * 1000
      detect_panel$start(Sys.time())
      shiny::showNotification(
        .running_notif_content(i18n$t("dess_detect_running"), detect_panel$start()),
        id = session$ns("detect_notif"), type = "message", duration = NULL)
      detect_panel$task$invoke(cd, file.path(cd, "aoi_input.gpkg"), bm,
                               isTRUE(input$detect_lidar), pp)
    })
    detect_res <- shiny::reactive({
      rv_detect() %||% tryCatch(
        .load_cached_detection(.desserte_cache_dir(app_state$current_project$path)),
        error = function(e) NULL)
    })

    output$detect_result <- shiny::renderUI({
      r <- detect_res()
      if (is.null(r)) {
        return(htmltools::tags$p(class = "text-muted small", i18n$t("dess_detect_hint")))
      }
      cl <- r$classes
      htmltools::tagList(
        htmltools::tags$div(class = "small",
                            sprintf(i18n$t("dess_detect_done_fmt"), r$n_detecte)),
        # Repartition par classe. Le brief sect.2 insiste : afficher `CLASSE` seule
        # serait trompeur - une classe posee sur peu de criteres renseignes doit
        # se voir. D'ou la confiance moyenne juste en dessous.
        if (is.list(cl) && length(cl$table)) {
          htmltools::tagList(
            htmltools::tags$table(
              class = "table table-sm table-striped small mt-2 mb-1",
              htmltools::tags$tbody(lapply(names(cl$table), function(k)
                htmltools::tags$tr(
                  # Meme libelle que le popup du calque : deux vocabulaires pour
                  # la meme classe se liraient comme deux classes.
                  htmltools::tags$td(class = "small",
                                     .dess_detect_classe_label(k, i18n)),
                  htmltools::tags$td(class = "small text-end",
                                     format(cl$table[[k]], big.mark = " ")))))),
            htmltools::tags$div(
              class = "small text-muted",
              sprintf(i18n$t("dess_detect_conf_fmt"),
                      100 * (cl$conf_moy %||% NA_real_))),
            if (isTRUE(cl$n_osm_tags > 0L)) {
              htmltools::div(class = "alert alert-info py-2 small mt-2 mb-0",
                             sprintf(i18n$t("dess_detect_osm_fmt"), cl$n_osm_tags))
            })
        },
        # Sans canal de surface le coeur avertit que la detection est " nettement
        # moins sure " : ne pas laisser lire un " 0 detection " comme un constat.
        if (!isTRUE(r$avec_lidar)) {
          htmltools::div(class = "alert alert-warning py-2 small mt-2 mb-0",
                         i18n$t("dess_detect_sans_lidar"))
        })
    })

    # ORCHESTRATION - le typage est SYNCHRONE (pas d'ExtendedTask) : il rend
    # `TRUE` s'il a type, `FALSE` si le calcul a echoue, `NULL` si une garde
    # l'a refuse. Les trois cas sont distingues par l'appelant enchaine.
    .lancer_typage <- function() {
      project_path <- tryCatch(app_state$current_project$path, error = function(e) NULL)
      parcelles <- units_sf()
      if (is.null(project_path) || is.null(parcelles)) {
        shiny::showNotification(i18n$t("dess_typage_no_parcelles"), type = "warning")
        return()
      }
      cache_dir <- .desserte_cache_dir(project_path)
      rv_typage(list(status = "running"))
      nid <- shiny::showNotification(i18n$t("dess_typage_running"), duration = NULL,
                                     type = "message")
      on.exit(shiny::removeNotification(nid), add = TRUE)
      res <- tryCatch(
        run_desserte_typage(cache_dir, parcelles,
                            taux_prelevement = input$typage_taux,
                            horizon_ans = input$typage_horizon),
        error = function(e) list(status = "error", reason = "desserte_typage_failed",
                                 detail = conditionMessage(e)))
      rv_typage(res)
      # « empty » = rien a typer parce que rien a creer : information, pas
      # erreur. Un toast rouge sur le meilleur resultat possible (reseau
      # existant suffisant) envoie chercher une panne qui n'existe pas.
      if (identical(res$status, "empty")) {
        shiny::showNotification(i18n$t(res$reason %||% "desserte_typage_rien_a_typer"),
                                type = "message", duration = 8)
      } else if (!identical(res$status, "success")) {
        msg <- i18n$t(res$reason %||% "desserte_typage_failed")
        det <- tryCatch(res$detail, error = function(e) NULL)
        if (!is.null(det) && nzchar(det)) msg <- paste0(msg, " \u2014 ", .strip_ansi(det))
        shiny::showNotification(msg, type = "error", duration = NULL)
      }
      st <- rv_typage()$status
      if (identical(st, "success")) TRUE else if (identical(st, "empty")) NA else FALSE
    }

    shiny::observeEvent(input$run_typage, .lancer_typage())

    # --- Lancement enchaine : etape « desserte_typage » ----------------
    shiny::observeEvent(app_state$pipeline_request, {
      req <- app_state$pipeline_request
      if (!pipeline_targets(req, "desserte_typage")) return()
      if (is.null(units_sf()) ||
          is.null(tryCatch(app_state$current_project$path, error = function(e) NULL))) {
        pipeline_answer(app_state, req, "skipped", i18n$t("pipeline_no_project"))
        return()
      }
      ok <- .lancer_typage()
      # Trois issues : type (TRUE), rien a typer (NA - le reseau existant
      # suffit, c'est un bon resultat), garde refusee (NULL), echec (FALSE).
      pipeline_answer(
        app_state, req,
        if (isTRUE(ok)) "ok" else if (is.null(ok) || is.na(ok)) "skipped" else "error",
        if (isTRUE(ok)) NULL
        else if (is.na(ok)) i18n$t("desserte_typage_rien_a_typer")
        else if (is.null(ok)) i18n$t("pipeline_skip_not_started")
        else i18n$t("desserte_typage_failed"))
    })

    # Le typage etait le SEUL des cinq a n'avoir aucun repli sur le cache : on
    # rouvrait le projet, `typage_<moteur>.gpkg` etait bien sur le disque, et
    # l'onglet redemandait de typer le reseau. Un echec de re-run laisse voir le
    # dernier typage reussi - c'est ce qui est sur le disque, et la notification
    # d'erreur dit deja que la nouvelle tentative a echoue.
    typage_res <- shiny::reactive({
      r <- rv_typage()
      st <- tryCatch(r$status, error = function(e) NULL) %||% ""
      if (identical(st, "running") || identical(st, "success")) return(r)
      tryCatch(.load_cached_typage(.desserte_cache_dir(app_state$current_project$path)),
               error = function(e) NULL)
    })

    output$typage_result <- shiny::renderUI({
      res <- typage_res()
      if (is.null(res)) {
        return(htmltools::tags$p(class = "text-muted small", i18n$t("dess_typage_hint")))
      }
      if (identical(res$status, "running")) return(NULL)
      if (!identical(res$status, "success")) return(NULL)
      rec <- res$recap
      if (!is.data.frame(rec) || nrow(rec) == 0L) return(NULL)
      # Table type -> longueur (km).
      km <- round(suppressWarnings(as.numeric(rec$longueur)) / 1000, 2)
      rows <- lapply(seq_len(nrow(rec)), function(i) {
        htmltools::tags$tr(
          htmltools::tags$td(class = "small", as.character(rec$type[i])),
          htmltools::tags$td(class = "small text-end", sprintf("%.2f km", km[i])))
      })
      htmltools::tags$table(
        class = "table table-sm table-striped small mb-0",
        htmltools::tags$thead(htmltools::tags$tr(
          htmltools::tags$th(i18n$t("dess_typage_col_type")),
          htmltools::tags$th(class = "text-end", i18n$t("dess_typage_col_long")))),
        htmltools::tags$tbody(rows))
    })

    # Overlay " Reseau type " : polylignes colorees par classe (primaire/secondaire/
    # tertiaire), lues depuis le GPKG du typage.
    dess_type_cols <- c(primaire = "#C62828", secondaire = "#FB8C00",
                        tertiaire = "#2E7D32")
    shiny::observe({
      res <- typage_res()
      shown <- shiny::isolate(input$map_groups)
      proxy <- leaflet::leafletProxy("map") |> leaflet::clearGroup(DESS_GROUPE_TYPE)
      gp <- tryCatch(res$gpkg_path, error = function(e) NULL)
      if (is.null(gp) || !file.exists(gp)) return()
      d <- tryCatch(sf::st_read(gp, layer = "reseau_type", quiet = TRUE),
                    error = function(e) NULL)
      if (!inherits(d, "sf") || nrow(d) == 0L) return()
      d <- tryCatch(sf::st_transform(d, 4326), error = function(e) d)
      ty <- tolower(as.character(d[["type"]] %||% rep("", nrow(d))))
      cols <- unname(dess_type_cols[ty]); cols[is.na(cols)] <- "#607D8B"
      proxy |>
        leaflet::addPolylines(data = d, group = DESS_GROUPE_TYPE,
          color = cols, weight = 3, opacity = 0.9, label = ~ as.character(type))
      if (!is.null(shown) && !(DESS_GROUPE_TYPE %in% shown)) {
        leaflet::hideGroup(proxy, DESS_GROUPE_TYPE)
      }
    })

    # Overlay " Pistes OSM " : l'acquisition Overpass telle quelle.
    #
    # Le libelle dit " pistes OSM " et non " pistes absentes de la BD TOPO ",
    # parce que c'est ce que contient le fichier. `comparer_desserte_osm()`
    # calcule bien un lineaire HORS CORRIDOR par troncon, mais ne renvoie que
    # des kilometres par type : la geometrie du gisement est jetee cote coeur.
    # La reconstruire ici (corridor + `st_difference`) dupliquerait la logique
    # du coeur avec un `corridor_m` qui pourrait diverger, pour 104 s de calcul
    # deja fait ailleurs - d'ou le calque honnete en attendant que
    # `foretaccess` renvoie `osm_hors_corridor`.
    shiny::observe({
      r <- osm_res()
      shown <- shiny::isolate(input$map_groups)
      proxy <- leaflet::leafletProxy("map") |> leaflet::clearGroup(DESS_GROUPE_OSM)
      gp <- tryCatch(r$gpkg_path, error = function(e) NULL)
      if (is.null(gp) || !file.exists(gp)) return()
      d <- tryCatch(sf::st_read(gp, layer = "osm_track", quiet = TRUE),
                    error = function(e) NULL)
      if (!inherits(d, "sf") || nrow(d) == 0L) return()
      d <- tryCatch(sf::st_transform(d, 4326), error = function(e) d)
      hw <- as.character(d[["highway"]] %||% rep("", nrow(d)))
      proxy |>
        leaflet::addPolylines(data = d, group = DESS_GROUPE_OSM,
          color = "#546E7A", weight = 2, opacity = 0.85, dashArray = "4,6",
          label = hw,
          popup = paste0("<b>", i18n$t("dess_osm_layer"), "</b><br>",
                         htmltools::htmlEscape(hw), "<br><span class='text-muted'>",
                         i18n$t("dess_osm_layer_note"), "</span>"))
      if (!is.null(shown) && !(DESS_GROUPE_OSM %in% shown)) {
        leaflet::hideGroup(proxy, DESS_GROUPE_OSM)
      }
    })

    # Overlay " Routes detectees " : trait TIRETE, pour dire hypothese et non
    # releve. Le popup porte `CLASSE_CONF` et `CLASSE_MOTIF` a cote de `CLASSE` -
    # une classe posee sur deux criteres sur six ne vaut pas une classe posee
    # sur six, et la moyenne affichee en sidebar ne le dit pas troncon par
    # troncon. Rappel : fosses et NDVI ne sont pas cables, la confiance est donc
    # structurellement plafonnee.
    shiny::observe({
      r <- detect_res()
      shown <- shiny::isolate(input$map_groups)
      proxy <- leaflet::leafletProxy("map") |> leaflet::clearGroup(DESS_GROUPE_DETECTEE)
      gp <- tryCatch(r$gpkg_path, error = function(e) NULL)
      if (is.null(gp) || !file.exists(gp)) return()
      d <- tryCatch(sf::st_read(gp, layer = "desserte_detectee", quiet = TRUE),
                    error = function(e) NULL)
      if (!inherits(d, "sf") || nrow(d) == 0L) return()
      d <- tryCatch(sf::st_transform(d, 4326), error = function(e) d)
      cl <- as.character(d[["CLASSE"]] %||% rep(NA_character_, nrow(d)))
      cols <- unname(DESS_DETECT_COLS[cl]); cols[is.na(cols)] <- "#BDBDBD"
      proxy |>
        leaflet::addPolylines(data = d, group = DESS_GROUPE_DETECTEE,
          color = cols, weight = 3, opacity = 0.9, dashArray = "8,6",
          label = .dess_detect_classe_label(cl, i18n),
          popup = .dess_detect_popup(d, i18n))
      if (!is.null(shown) && !(DESS_GROUPE_DETECTEE %in% shown)) {
        leaflet::hideGroup(proxy, DESS_GROUPE_DETECTEE)
      }
    })

    # Ou sont les fichiers. Les couches optionnelles sont aussi lisibles dans un
    # SIG, et rien dans l'interface ne disait ou les trouver.
    output$cache_path <- shiny::renderUI({
      pp <- tryCatch(app_state$current_project$path, error = function(e) NULL)
      if (is.null(pp)) return(NULL)
      cd <- tryCatch(.desserte_cache_dir(pp), error = function(e) NULL)
      if (is.null(cd)) return(NULL)
      htmltools::tags$p(
        class = "text-muted small mt-2 mb-0",
        htmltools::tags$span(i18n$t("dess_cache_path"), " "),
        htmltools::tags$code(class = "small text-break", cd))
    })

    # --- Export GeoPackage -----------------------------------------------------
    output$export_gpkg <- shiny::downloadHandler(
      filename = function() {
        paste0(.project_export_slug(app_state$current_project, "nemeton"),
               "_desserte.gpkg")
      },
      content = function(file) {
        on.exit(session$sendCustomMessage("nemetonHideDownloadToast", list()),
                add = TRUE)
        res <- rv$result
        if (is.null(res) || !isTRUE(export_desserte_geopackage(res, file))) {
          shiny::showNotification(i18n$t("dess_export_empty"), type = "warning")
          if (!file.exists(file)) writeLines("No data available", file)
        }
      }
    )
  })
}

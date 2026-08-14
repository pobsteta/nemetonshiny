# service_tour.R - Definition declarative du Tour guide (cicerone).
#
# L'AUTO-demarrage est optionnel : `run_app(tour = FALSE)` (ou NEMETON_TOUR=0)
# fait demarrer l'app directement, sans le tour. Motivation : le tour injecte du
# JS client 2 s apres la connexion, ce qui gene les demos/captures et rend les
# tests E2E instables (cicerone emet " There are no steps defined to iterate "
# sur le meme flush, ce qui destabilise la session shinytest2). Le tour reste
# lancable a la main depuis l'aide - seul l'auto-demarrage est supprime.
#
# Le tour est une liste ORDONNEE de steps, chacun ancre sur un element
# (id namespace) d'un onglet. cicerone (>= 1.0.4) sait activer l'onglet
# cible (`tab` + `tab_id`) AVANT de cadrer l'element : un seul guide peut
# donc traverser tous les onglets de `main_nav` sans orchestrateur
# manuel. Chaque step porte `tab_id = "main_nav"` ; cicerone bascule
# l'onglet, ce qui rend le tour robuste quel que soit l'onglet d'ou il
# est relance.
#
# Couverture (1 step cle par onglet + onboarding detaille sur l'Accueil) :
#   selection   -> recherche, carte, nom/description/owner, creer
#   synthesis   -> synthese (score / radar / perspective IA)
#   action_plan -> plan d'action
#   terrain     -> echantillonnage terrain (sous-onglet par defaut)
#   monitoring  -> mode de suivi (FAST / FORDEAD / RECONFORT)
#   familles    -> vue d'une famille d'indicateurs (Carbone, representative)
#
# ANCRES : ce sont des ids stables. Les `uiOutput`/cards/inputs cibles
# sont TOUJOURS presents dans le DOM (un `uiOutput` vide reste un
# conteneur) - on evite deliberement les boutons conditionnels (ex.
# `start_compute`, rendu seulement en statut draft). Si un module renomme
# une ancre, mettre a jour ici ET le test d'inventaire
# (`test-service_tour.R`).

#' Build the ordered guided-tour step specs.
#'
#' @param i18n A translator from [get_i18n()].
#' @param max_parcels Integer interpolated into the map step description.
#' @return A list of step specs, each a list with `el` (namespaced id),
#'   `title`, `description` and `tab` (the `main_nav` value to activate).
#' @noRd
build_tour_steps <- function(i18n, max_parcels = 30L) {
  list(
    # ----- Accueil (onboarding creation de projet) -----
    list(tab = "selection", el = "home-search_collapse",
         title = i18n$t("tour_search_title"),
         description = i18n$t("tour_search_desc")),
    list(tab = "selection", el = "home-map-map_card",
         title = i18n$t("tour_map_title"),
         description = i18n$t("tour_map_desc", max = max_parcels)),
    list(tab = "selection", el = "home-project-name",
         title = i18n$t("tour_project_title"),
         description = i18n$t("tour_project_desc")),
    list(tab = "selection", el = "home-project-description",
         title = i18n$t("tour_description_title"),
         description = i18n$t("tour_description_desc")),
    list(tab = "selection", el = "home-project-owner",
         title = i18n$t("tour_owner_title"),
         description = i18n$t("tour_owner_desc")),
    list(tab = "selection", el = "home-project-create_project",
         title = i18n$t("tour_create_title"),
         description = i18n$t("tour_create_desc")),
    # ----- 1 step cle par onglet -----
    list(tab = "synthesis", el = "synthesis-project_summary",
         title = i18n$t("tour_synthesis_title"),
         description = i18n$t("tour_synthesis_desc")),
    list(tab = "action_plan", el = "action_plan-action_sidebar",
         title = i18n$t("tour_action_plan_title"),
         description = i18n$t("tour_action_plan_desc")),
    list(tab = "terrain", el = "sampling-sidebar",
         title = i18n$t("tour_terrain_title"),
         description = i18n$t("tour_terrain_desc")),
    list(tab = "monitoring", el = "monitoring-mode",
         title = i18n$t("tour_monitoring_title"),
         description = i18n$t("tour_monitoring_desc")),
    list(tab = "famille_carbone", el = "famille_carbone-maps_row",
         title = i18n$t("tour_families_title"),
         description = i18n$t("tour_families_desc"))
  )
}

#' JS (run in cicerone's `on_highlight_started`) switching the active
#' `main_nav` tab by clicking its nav link.
#'
#' On NE PEUT PAS utiliser le couple `tab`/`tab_id` natif de cicerone :
#' son JS (cicerone.js) bascule l'onglet via
#' `Shiny.inputBindings.bindingNames['shiny.bootstrapTabInput'].binding.setValue()`,
#' incompatible avec le `page_navbar` bslib (Bootstrap 5) - l'appel leve
#' une exception qui AVORTE tout le tour (il ne se lance plus du tout).
#' On bascule donc l'onglet cote client en cliquant le lien de nav
#' (`#main_nav a[data-value="<tab>"]`, marque `data-bs-toggle="tab"`),
#' synchrone et compatible BS4/BS5. `on_highlight_started` s'execute juste
#' avant le cadrage de l'element, donc l'onglet est actif au moment du
#' highlight.
#'
#' IMPORTANT - le retour doit etre une **expression de fonction**
#' (`function(){...}`), pas un bloc d'instructions : cicerone (1.0.4) passe
#' `on_highlight_started` brut dans `new Function("return " + js)()`
#' (cicerone.js:101). Une chaine commencant par `var` produit
#' `return var ...` -> `SyntaxError: Unexpected token 'var'`, ce qui casse la
#' compilation des steps (driver.js se retrouve " no steps to iterate ") et
#' donc la bascule d'onglet du tour. L'envelopper en `function(){...}` la rend
#' valide : `new Function("return function(){...}")()` renvoie la fonction.
#' @noRd
.tour_switch_tab_js <- function(tab) {
  sprintf(
    "function(){var __l=document.querySelector('#main_nav a[data-value=\"%s\"]'); if(__l){__l.click();}}",
    tab
  )
}

#' Build a cicerone guide object from the step specs
#'
#' @param i18n A translator from [get_i18n()].
#' @param max_parcels Integer for the map step.
#' @return A `cicerone::Cicerone` R6 object with every step chained, or
#'   NULL when cicerone is unavailable.
#' @noRd
build_tour_guide <- function(i18n, max_parcels = 30L) {
  if (!requireNamespace("cicerone", quietly = TRUE)) return(NULL)
  steps <- build_tour_steps(i18n, max_parcels = max_parcels)
  guide <- cicerone::Cicerone$new()
  for (s in steps) {
    # `is_id = TRUE` (cicerone default) -> el is treated as an #id.
    # Tab switching is done client-side via on_highlight_started (see
    # .tour_switch_tab_js) - PAS via tab/tab_id (binding casse sous bslib).
    guide$step(
      el                  = s$el,
      title               = s$title,
      description         = s$description,
      on_highlight_started = .tour_switch_tab_js(s$tab)
    )
  }
  guide
}

#' Is the guided tour allowed to AUTO-start?
#'
#' Resolution order: env var `NEMETON_TOUR` (`0`/`false`/`no`/`non` disables,
#' `1`/`true`/`yes`/`oui` enables) > app option `tour` set by `run_app()` >
#' `TRUE`. The environment variable wins so a demo, a screencast or a test run
#' can suppress the tour without touching the call site.
#'
#' Only the AUTO-start is governed here: a tour explicitly requested from the
#' help menu (`app_state$restart_tour`) always runs.
#'
#' @return `TRUE` or `FALSE`.
#' @noRd
.tour_autostart_enabled <- function() {
  env <- tolower(trimws(Sys.getenv("NEMETON_TOUR", "")))
  if (env %in% c("0", "false", "no", "non", "off")) return(FALSE)
  if (env %in% c("1", "true", "yes", "oui", "on")) return(TRUE)
  opt <- tryCatch(get_app_options()$tour, error = function(e) NULL)
  if (is.null(opt) || length(opt) != 1L || is.na(as.logical(opt)[1])) return(TRUE)
  isTRUE(as.logical(opt)[1])
}

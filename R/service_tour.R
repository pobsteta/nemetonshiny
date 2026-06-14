# service_tour.R — Définition déclarative du Tour guidé (cicerone).
#
# Le tour est une liste ORDONNÉE de steps, chacun ancré sur un élément
# (id namespacé) d'un onglet. cicerone (>= 1.0.4) sait activer l'onglet
# cible (`tab` + `tab_id`) AVANT de cadrer l'élément : un seul guide peut
# donc traverser tous les onglets de `main_nav` sans orchestrateur
# manuel. Chaque step porte `tab_id = "main_nav"` ; cicerone bascule
# l'onglet, ce qui rend le tour robuste quel que soit l'onglet d'où il
# est relancé.
#
# Couverture (1 step clé par onglet + onboarding détaillé sur l'Accueil) :
#   selection   → recherche, carte, nom/description/owner, créer
#   synthesis   → synthèse (score / radar / perspective IA)
#   action_plan → plan d'action
#   terrain     → échantillonnage terrain (sous-onglet par défaut)
#   monitoring  → mode de suivi (FAST / FORDEAD / RECONFORT)
#   familles    → vue d'une famille d'indicateurs (Carbone, représentative)
#
# ANCRES : ce sont des ids stables. Les `uiOutput`/cards/inputs ciblés
# sont TOUJOURS présents dans le DOM (un `uiOutput` vide reste un
# conteneur) — on évite délibérément les boutons conditionnels (ex.
# `start_compute`, rendu seulement en statut draft). Si un module renomme
# une ancre, mettre à jour ici ET le test d'inventaire
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
    # ----- Accueil (onboarding création de projet) -----
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
    # ----- 1 step clé par onglet -----
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
    # `is_id = TRUE` (cicerone default) → el is treated as an #id.
    # `tab` + `tab_id` make cicerone activate the `main_nav` panel
    # before highlighting, so cross-tab steps frame correctly.
    guide$step(
      el          = s$el,
      title       = s$title,
      description = s$description,
      tab         = s$tab,
      tab_id      = "main_nav"
    )
  }
  guide
}

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
# ANCRES : ce sont des ids stables, et ils doivent etre STATIQUES. Present
# dans le DOM ne suffit pas : driver.js mesure l'element JUSTE APRES la
# bascule d'onglet, et un `uiOutput` porte par un onglet jusque-la masque est
# encore SUSPENDU (Shiny ne le rend qu'apres un aller-retour serveur). Il
# mesure donc 0 de haut, `canHighlight()` est faux, et driver SAUTE l'etape
# en silence - le popover reste sur l'etape precedente, ce que l'utilisateur
# lit comme un tour bloque. Mesure du 2026-09-19 : `synthesis-project_summary`
# = 447x0 et `famille_carbone-maps_row` = 1408x0 a l'instant du cadrage, la
# ou une carte ou une sidebar statique mesure sa vraie geometrie. On ancre
# donc sur des conteneurs statiques (`summary_card`, `family_header`,
# sidebars, inputs) et on evite les boutons conditionnels (ex.
# `start_compute`, rendu seulement en statut draft). Si un module renomme
# une ancre, mettre a jour ici ET le test d'inventaire
# (`test-service_tour.R`).
#
# TAILLE DE L'ANCRE : une ancre ne doit pas remplir la fenetre. driver.js
# pose son popover A COTE de l'element cadre ; face a un element presque aussi
# haut que la fenetre, il le pousse HORS de l'ecran (mesure : ancre
# `action_plan-action_sidebar`, 370x793 dans 900 de haut -> popover a
# top = -24). La page se met alors a osciller entre avec et sans barre de
# defilement, et chaque bascule reveille le ResizeObserver de bslib, qui
# redispatche un `resize` - que driver.js ecoute pour se recadrer. La boucle
# s'entretient : 122 evenements en 2 s, ~50 par seconde, sans fin. C'est le
# tremblement signale le 2026-09-19, et il ne touchait QUE cette etape
# (0 evenement sur toutes les autres ancres, mesure comparative). Ancrer sur
# la carte « Tableau des actions » (322x641) au lieu de la sidebar qui la
# contient ramene la mesure a 0.
#
# ONGLETS RESTREINTS : l'app renvoie sur l'Accueil toute navigation vers
# Synthese ou une famille tant que le projet n'est pas `completed`
# (cf. `.tab_requires_completed_project`). Le tour ne doit donc pas y aller
# dans cet etat : il declenchait un aller-retour serveur qui ramenait
# l'onglet sous le cadre - le fameux " ca tremble ".

#' Does a `main_nav` tab require a completed project?
#'
#' Single source of truth for the navigation guard in [app_server()] AND for
#' the guided-tour step filter: the two MUST agree, or the tour walks into a
#' tab the app immediately navigates away from.
#'
#' @param tab A `main_nav` value.
#' @return `TRUE` when the tab is only reachable with a completed project.
#' @noRd
.tab_requires_completed_project <- function(tab) {
  if (length(tab) != 1L || is.na(tab)) return(FALSE)
  identical(tab, "synthesis") || grepl("^famille_", tab)
}

#' Build the ordered guided-tour step specs.
#'
#' Steps on tabs that need a completed project are dropped unless the project
#' actually is completed - see `.tab_requires_completed_project()`.
#'
#' @param i18n A translator from [get_i18n()].
#' @param max_parcels Integer interpolated into the map step description.
#' @param project_status Current `app_state$project_status` (`NULL` when no
#'   project is loaded).
#' @return A list of step specs, each a list with `el` (namespaced id),
#'   `title`, `description` and `tab` (the `main_nav` value to activate).
#' @noRd
build_tour_steps <- function(i18n, max_parcels = 30L, project_status = NULL) {
  steps <- list(
    # ----- Accueil (onboarding creation de projet) -----
    list(tab = "selection", el = "home-search_card",
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
    list(tab = "synthesis", el = "synthesis-summary_card",
         title = i18n$t("tour_synthesis_title"),
         description = i18n$t("tour_synthesis_desc")),
    # `position = "left"` : la carte est collee au bord DROIT de la fenetre et
    # mesure 641 de haut. Au placement par defaut, driver.js pose le popover
    # au-dessus et le haut du texte sort de l'ecran (mesure : top = -21 avec
    # la description reelle, plus longue que le gabarit de test). A gauche, il
    # se cale au niveau de la carte et tient entierement dans la fenetre.
    list(tab = "action_plan", el = "action_plan-actions_card",
         position = "left",
         title = i18n$t("tour_action_plan_title"),
         description = i18n$t("tour_action_plan_desc")),
    list(tab = "terrain", el = "sampling-sidebar",
         title = i18n$t("tour_terrain_title"),
         description = i18n$t("tour_terrain_desc")),
    list(tab = "monitoring", el = "monitoring-mode",
         title = i18n$t("tour_monitoring_title"),
         description = i18n$t("tour_monitoring_desc")),
    list(tab = "famille_carbone", el = "famille_carbone-family_header",
         title = i18n$t("tour_families_title"),
         description = i18n$t("tour_families_desc"))
  )
  if (identical(project_status, "completed")) return(steps)
  Filter(function(s) !.tab_requires_completed_project(s$tab), steps)
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
#'
#' IMPORTANT (2) - le clic doit etre ETOUFFE avant `window`. driver.js ecoute
#' les clics sur `window` (`bind()` -> `onClick`) et, des qu'un element est
#' deja mis en avant, tout clic hors du popover et hors de cet element
#' declenche `reset()` (`allowClose` vaut TRUE par defaut). Or notre clic de
#' bascule part DANS `on_highlight_started`, c.-a-d. pendant que le step
#' precedent est encore l'element courant : driver fermait donc le tour, puis
#' la suite de `highlight()` reaffichait quand meme popover et cadre. D'ou le
#' symptome observe (2026-09-19) : l'etape 2 s'affiche, mais `isActivated` est
#' repasse a FALSE et plus rien n'est cliquable - ni Suivant, ni Fermer, ni
#' les fleches du clavier - et la re-mesure `resize` ci-dessous devenait elle
#' aussi inoperante. La premiere etape y echappait seulement parce qu'aucun
#' element n'etait encore mis en avant (`hasHighlightedElement()` faux).
#' @noRd
.tour_switch_tab_js <- function(tab) {
  # Cliquer suffit a ACTIVER l'onglet, pas a le MESURER. Un `.tab-pane`
  # masque est en `display: none` : ses elements ont des dimensions NULLES
  # jusqu'a ce que le navigateur ait pose le panneau, et Bootstrap 5 ajoute
  # une transition `.fade` par-dessus. Or driver.js cadre l'element dans la
  # foulee de `on_highlight_started` - il mesurait donc une geometrie qui
  # n'existait pas encore, d'ou des cadres a cote sur la plupart des etapes
  # (constate le 2026-09-18, surtout a la premiere ouverture ou rien n'est
  # encore en cache).
  #
  # On attend donc `shown.bs.tab`, que Bootstrap emet APRES la transition,
  # puis on force une re-mesure. Le canal choisi est l'evenement `resize` :
  # driver.js l'ecoute deja (`bind()` -> `onResize()` -> `refresh()`) et ne
  # re-mesure QUE si un tour est actif (`isActivated`). Aucun interne de
  # cicerone n'est touche, donc rien a reprendre si le paquet evolue.
  #
  # Le repli a 250 ms n'est pas de la ceinture-bretelles : si l'onglet vise
  # est DEJA actif, le clic ne declenche aucun `shown.bs.tab` et l'attente
  # ne se resoudrait jamais.
  sprintf(
    paste0(
      "function(){",
      "var __l=document.querySelector('#main_nav a[data-value=\"%s\"]');",
      "if(!__l){return;}",
      "var __r=function(){try{window.dispatchEvent(new Event('resize'));}catch(e){}};",
      "var __done=false;",
      "var __h=function(){if(__done){return;}__done=true;",
      "__l.removeEventListener('shown.bs.tab',__h);",
      "requestAnimationFrame(function(){requestAnimationFrame(__r);});};",
      "__l.addEventListener('shown.bs.tab',__h);",
      # Le clic synthetique ne doit PAS remonter jusqu'a `window` : driver.js
      # y ecoute les clics et fermerait le tour (voir commentaire ci-dessus).
      # On le stoppe au niveau de `document`, APRES le handler delegue de
      # Bootstrap (enregistre au chargement, donc avant celui-ci) : l'onglet
      # bascule normalement, mais l'evenement n'atteint jamais driver.js.
      "var __s=function(ev){ev.stopPropagation();};",
      "document.addEventListener('click',__s,false);",
      "try{__l.click();}finally{document.removeEventListener('click',__s,false);}",
      "setTimeout(__h,250);",
      "}"
    ),
    tab
  )
}

#' Build a cicerone guide object from the step specs
#'
#' @param i18n A translator from [get_i18n()].
#' @param max_parcels Integer for the map step.
#' @param project_status Current `app_state$project_status`, forwarded to
#'   [build_tour_steps()] so restricted tabs are skipped.
#' @return A `cicerone::Cicerone` R6 object with every step chained, or
#'   NULL when cicerone is unavailable.
#' @noRd
build_tour_guide <- function(i18n, max_parcels = 30L, project_status = NULL) {
  if (!requireNamespace("cicerone", quietly = TRUE)) return(NULL)
  steps <- build_tour_steps(i18n, max_parcels = max_parcels,
                            project_status = project_status)
  guide <- cicerone::Cicerone$new()
  for (s in steps) {
    # `is_id = TRUE` (cicerone default) -> el is treated as an #id.
    # Tab switching is done client-side via on_highlight_started (see
    # .tour_switch_tab_js) - PAS via tab/tab_id (binding casse sous bslib).
    guide$step(
      el                  = s$el,
      title               = s$title,
      description         = s$description,
      # `position` est optionnel : NULL laisse driver.js choisir.
      position            = s$position,
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

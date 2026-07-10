#' nemetonApp UI
#'
#' @description
#' Main UI function for the nemetonApp Shiny application.
#' Uses bslib page_navbar with a forest theme.
#'
#' @param request Internal parameter for bookmarking.
#' @return A Shiny UI definition.
#'
#' @noRd
app_ui <- function(request) {
  # Get current language
 opts <- get_app_options()
  lang <- opts$language

  # Get translations
  i18n <- get_i18n(lang)

  # Disable Shiny 1.8+ / bslib 0.6+ busy indicators (white overlays)
  busy_indicator_tag <- tryCatch(
    shiny::useBusyIndicators(spinners = FALSE, pulse = FALSE),
    error = function(e) NULL
  )

  htmltools::tagList(
    # Add external resources (CSS, JS)
    app_add_external_resources(),

    # Disable busy indicators (safe — returns NULL if function doesn't exist)
    busy_indicator_tag,

    # Cicerone for guided tours
    if (requireNamespace("cicerone", quietly = TRUE)) cicerone::use_cicerone(),

    # shinyOAuth (si configure)
    if (is_oauth_configured() && requireNamespace("shinyOAuth", quietly = TRUE))
      shinyOAuth::use_shinyOAuth(),

    # Main page with navbar
    bslib::page_navbar(
      id = "main_nav",
      title = htmltools::img(
        src = "www/img/logo.svg",
        height = "80px",
        alt = "N\u00e9m\u00e9ton logo"
      ),
      window_title = i18n$t("app_title"),
      theme = nemeton_theme(),
      fillable = TRUE,
      navbar_options = bslib::navbar_options(bg = "#1B6B1B"),

      # Bandeau lecture seule (verrou projet) — sous la navbar, au-dessus des
      # onglets, visible partout tant qu'un projet est ouvert en lecture seule.
      header = shiny::uiOutput("lock_banner"),

      # === Tab 1: Selection (includes cadastral map, tenement map, UG table) ===
      bslib::nav_panel(
        title = i18n$t("tab_selection"),
        value = "selection",
        icon = bsicons::bs_icon("map"),
        mod_home_ui("home")
      ),

      # === Tab 2: Synthesis ===
      bslib::nav_panel(
        title = i18n$t("tab_synthesis"),
        value = "synthesis",
        icon = bsicons::bs_icon("pie-chart"),
        mod_synthesis_ui("synthesis")
      ),

      # === Tab 2bis: Action Plan ===
      bslib::nav_panel(
        title = i18n$t("tab_action_plan"),
        value = "action_plan",
        icon = bsicons::bs_icon("clipboard-check"),
        mod_action_plan_ui("action_plan")
      ),

      # === Tab 3: Terrain (sub-tabs: Export / Import, E5.a + E5.b) ===
      bslib::nav_panel(
        title = i18n$t("tab_sampling"),
        value = "terrain",
        icon = bsicons::bs_icon("crosshair"),
        bslib::navset_card_underline(
          bslib::nav_panel(
            title = i18n$t("tab_terrain_export"),
            value = "sampling",
            icon = bsicons::bs_icon("download"),
            mod_sampling_ui("sampling")
          ),
          bslib::nav_panel(
            title = i18n$t("tab_terrain_import"),
            value = "field_ingest",
            icon = bsicons::bs_icon("upload"),
            mod_field_ingest_ui("field_ingest")
          )
        )
      ),

      # === Tab 4: Monitoring (E6.b — continuous Sentinel-2 NDVI/NBR) ===
      bslib::nav_panel(
        title = i18n$t("tab_monitoring"),
        value = "monitoring",
        icon = bsicons::bs_icon("activity"),
        mod_monitoring_ui("monitoring")
      ),

      bslib::nav_panel(
        title = i18n$t("regen_tab_title"),
        value = "regeneration",
        icon = bsicons::bs_icon("thermometer-sun"),
        mod_regeneration_ui("regeneration")
      ),

      # === Tabs 5-16: Indicator Families ===
      bslib::nav_menu(
        title = i18n$t("tab_families"),
        icon = bsicons::bs_icon("layers"),

        bslib::nav_panel(
          title = i18n$t("famille_carbone"),
          value = "famille_carbone",
          mod_family_ui("famille_carbone", "C")
        ),
        bslib::nav_panel(
          title = i18n$t("famille_biodiversite"),
          value = "famille_biodiversite",
          mod_family_ui("famille_biodiversite", "B")
        ),
        bslib::nav_panel(
          title = i18n$t("famille_eau"),
          value = "famille_eau",
          mod_family_ui("famille_eau", "W")
        ),
        bslib::nav_panel(
          title = i18n$t("famille_air"),
          value = "famille_air",
          mod_family_ui("famille_air", "A")
        ),
        bslib::nav_panel(
          title = i18n$t("famille_sol"),
          value = "famille_sol",
          mod_family_ui("famille_sol", "F")
        ),
        bslib::nav_panel(
          title = i18n$t("famille_paysage"),
          value = "famille_paysage",
          mod_family_ui("famille_paysage", "L")
        ),
        bslib::nav_panel(
          title = i18n$t("famille_temporel"),
          value = "famille_temporel",
          mod_family_ui("famille_temporel", "T")
        ),
        bslib::nav_panel(
          title = i18n$t("famille_risque"),
          value = "famille_risque",
          mod_family_ui("famille_risque", "R")
        ),
        bslib::nav_panel(
          title = i18n$t("famille_social"),
          value = "famille_social",
          mod_family_ui("famille_social", "S")
        ),
        bslib::nav_panel(
          title = i18n$t("famille_production"),
          value = "famille_production",
          mod_family_ui("famille_production", "P")
        ),
        bslib::nav_panel(
          title = i18n$t("famille_energie"),
          value = "famille_energie",
          mod_family_ui("famille_energie", "E")
        ),
        bslib::nav_panel(
          title = i18n$t("famille_naturalite"),
          value = "famille_naturalite",
          mod_family_ui("famille_naturalite", "N")
        )
      ),

      # === Navbar items (right side) ===
      bslib::nav_spacer(),

      # Language selector
      bslib::nav_item(
        shiny::selectInput(
          inputId = "app_language",
          label = NULL,
          choices = c("FR" = "fr", "EN" = "en"),
          selected = lang,
          width = "80px"
        )
      ),

      # Theia / DATA TERRA configuration
      mod_theia_config_ui("theia_config"),

      # Help button
      bslib::nav_item(
        shiny::actionLink(
          inputId = "show_help",
          label = NULL,
          icon = bsicons::bs_icon("question-circle"),
          class = "nav-link",
          title = i18n$t("help")
        )
      )
    )
  )
}


#' Add external resources to the app
#'
#' @description
#' Adds CSS, JavaScript, and other external resources to the app.
#'
#' @return A tagList of HTML dependencies.
#' @noRd
app_add_external_resources <- function() {
  # Add resource path for static files (www folder)
  www_path <- system.file("app/www", package = "nemetonshiny")
  if (www_path != "") {
    shiny::addResourcePath("www", www_path)
  }

  htmltools::tagList(
    # Add CSS
    htmltools::tags$head(
      # Critical inline CSS: loads BEFORE external stylesheets to prevent
      # flash of white background while custom.css is being fetched.
      # Also nudges the Shiny notification panel 13px off the bottom-right
      # corner so toasts don't touch the viewport edges (FORDEAD phase
      # toasts, v0.32.0). Shiny defaults to bottom:0/right:0 already, the
      # offset just gives the cards a small breathing margin.
      htmltools::tags$style(htmltools::HTML("
        html, body { background-color: #f0f0f0 !important; }
        .bslib-page-fill, .html-fill-container { background-color: #f0f0f0 !important; }
        .card, .bslib-card { background-color: #ffffff !important; }
        .bslib-page-fill::after, .bslib-page-fill::before,
        .html-fill-container::after, .html-fill-container::before {
          content: none !important; display: none !important;
        }
        #shiny-notification-panel { bottom: 13px; right: 13px; }
      ")),
      # Custom CSS - minified for performance (cache-busting to ensure latest version)
      htmltools::tags$link(
        rel = "stylesheet",
        type = "text/css",
        href = paste0("www/css/custom.min.css?v=", as.integer(Sys.time()))
      ),
      # Favicon
      htmltools::tags$link(
        rel = "icon",
        type = "image/svg+xml",
        href = "www/img/logo.svg"
      ),
      # Meta tags for mobile
      htmltools::tags$meta(
        name = "viewport",
        content = "width=device-width, initial-scale=1"
      ),
      htmltools::tags$meta(
        name = "theme-color",
        content = "#1B6B1B"
      )
    ),

    # SortableJS for the Plan d'actions Kanban drag-and-drop. Vendored
    # in inst/app/www/js/ to avoid an external runtime dependency.
    htmltools::tags$script(src = "www/js/Sortable-1.15.6.min.js"),
    htmltools::tags$script(
      src = paste0("www/js/action_plan_kanban.js?v=",
                   as.integer(Sys.time()))
    ),

    # Custom JS - minified for performance (cache-busting to ensure latest version)
    htmltools::tags$script(
      src = paste0("www/js/custom.min.js?v=", as.integer(Sys.time()))
    )
  )
}


#' Placeholder for mod_home_ui
#'
#' @param id Module ID
#' @return UI elements
#' @noRd
mod_home_ui <- function(id) {
  ns <- shiny::NS(id)
  opts <- get_app_options()
  i18n <- get_i18n(opts$language)

  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      id = ns("sidebar"),
      title = i18n$t("search_title"),
      width = 350,

      # Department filter
      shiny::selectInput(
        inputId = ns("departement"),
        label = i18n$t("department"),
        choices = NULL,
        selected = NULL
      ),

      # Commune search
      shiny::selectizeInput(
        inputId = ns("commune"),
        label = i18n$t("commune"),
        choices = NULL,
        options = list(
          placeholder = i18n$t("search_commune"),
          maxOptions = 100
        )
      ),

      # Postal code
      shiny::textInput(
        inputId = ns("code_postal"),
        label = i18n$t("postal_code"),
        placeholder = "75001"
      ),

      shiny::hr(),

      # Selection info
      shiny::h5(i18n$t("selected_parcels")),
      shiny::uiOutput(ns("selection_info")),

      shiny::hr(),

      # Project form
      shiny::h5(i18n$t("project_info")),
      shiny::textInput(
        inputId = ns("project_name"),
        label = i18n$t("project_name"),
        placeholder = i18n$t("project_name_placeholder")
      ),
      shiny::textAreaInput(
        inputId = ns("project_description"),
        label = i18n$t("project_description"),
        placeholder = i18n$t("project_description_placeholder"),
        rows = 2
      ),
      shiny::textInput(
        inputId = ns("project_owner"),
        label = i18n$t("project_owner"),
        placeholder = i18n$t("project_owner_placeholder")
      ),

      shiny::hr(),

      # Action buttons
      shiny::actionButton(
        inputId = ns("btn_compute"),
        label = i18n$t("compute_button"),
        icon = shiny::icon("play"),
        class = "btn-primary btn-lg w-100",
        disabled = TRUE
      )
    ),

    # Main content: Map
    bslib::card(
      full_screen = TRUE,
      bslib::card_header(
        class = "d-flex justify-content-between align-items-center",
        htmltools::span(i18n$t("map_title")),
        htmltools::div(
          class = "btn-group",
          role = "group",
          shiny::actionButton(
            ns("basemap_osm"),
            "OSM",
            class = "btn btn-sm btn-outline-secondary active"
          ),
          shiny::actionButton(
            ns("basemap_satellite"),
            "Satellite",
            class = "btn btn-sm btn-outline-secondary"
          )
        )
      ),
      bslib::card_body(
        padding = 0,
        # Placeholder for leaflet map
        shiny::div(
          id = ns("map_container"),
          style = "height: 100%; min-height: 500px;",
          shiny::uiOutput(ns("map_placeholder"))
        )
      )
    )
  )
}


#' Placeholder for mod_synthesis_ui
#'
#' @param id Module ID
#' @return UI elements
#' @noRd
mod_synthesis_ui <- function(id) {
  ns <- shiny::NS(id)
  opts <- get_app_options()
  i18n <- get_i18n(opts$language)

  bslib::layout_columns(
    col_widths = c(12),
    # La carte « Synthèse du projet » (1re rangée) ne porte que quelques
    # lignes : sans contrainte, la grille fillable du nav_panel étire ses
    # deux rangées à parts égales et laisse un grand vide sous l'en-tête.
    # On force la 1re rangée à la hauteur de son contenu (auto) et on
    # laisse la 2e (radar / récap / commentaires) prendre le reste (1fr).
    row_heights = c("auto", 1),

    # Top row: Downloads, summary, and global score
    bslib::card(
      bslib::card_header(i18n$t("synthesis_title")),
      bslib::card_body(
        bslib::layout_columns(
          col_widths = c(4, 4, 4),

          # Project summary
          shiny::uiOutput(ns("project_summary")),

          # Download buttons
          htmltools::div(
            class = "d-grid gap-2",
            shiny::downloadButton(
              ns("download_pdf"),
              label = i18n$t("download_pdf"),
              icon = shiny::icon("file-pdf"),
              class = "btn-success btn-lg"
            ),
            shiny::downloadButton(
              ns("download_gpkg"),
              label = i18n$t("download_gpkg"),
              icon = shiny::icon("database"),
              class = "btn-primary btn-lg"
            ),
            # Cover image upload for PDF
            #
            # v0.52.3 — La légende « Taille image Max 5 Mo, PNG/JPG »
            # est à DROITE du fileInput, alignée verticalement au
            # NIVEAU DU BOUTON (et donc de la ligne des badges
            # `NDP / Hauteur LiDAR / Inventaire ML` de la colonne de
            # droite). Pourquoi `align-items: flex-start` + un
            # `padding-top` plutôt que `align-items: center` :
            #   * Le `shiny::fileInput` rend bouton + placeholder +
            #     barre de progression. La barre de progression
            #     s'affiche dès qu'un upload est en cours/complet,
            #     ce qui change la hauteur totale du fileInput.
            #   * `align-items: center` aurait centré le texte sur la
            #     hauteur totale (incluant la barre quand visible),
            #     donc plus bas que le bouton dès qu'on a chargé une
            #     image — exactement le décalage qu'on essaie de
            #     supprimer.
            #   * `flex-start` ancre le texte en haut du flex (donc
            #     au niveau du haut du bouton), et un `padding-top`
            #     de ~0.55rem (≈ la moitié de la hauteur du bouton
            #     38px) le descend pile au centre du bouton.
            htmltools::div(
              class = "mt-2 d-flex gap-2",
              style = "align-items: flex-start;",
              htmltools::div(
                style = "flex: 1;",
                shiny::fileInput(
                  ns("cover_image"),
                  label = NULL,
                  accept = c("image/png", "image/jpeg", "image/jpg"),
                  buttonLabel = htmltools::tagList(
                    shiny::icon("image"),
                    if (i18n$language == "fr") " Image de couverture" else " Cover image"
                  ),
                  placeholder = if (i18n$language == "fr") "Aucune image" else "No image"
                )
              ),
              htmltools::span(
                class = "text-muted small",
                # v0.52.6 — padding-top ajusté de 0.55rem (centre du
                # bouton 38px) à 1rem. v0.52.3 avait calé le centre
                # vertical du texte sur le centre du bouton ; mais la
                # ligne des badges (`NDP / Hauteur LiDAR / Inventaire
                # ML`) à droite tombe en réalité ~8 px plus bas que le
                # centre du bouton, parce que le flux de la colonne
                # droite (`Score global` → `54.8` → `/100 (12
                # familles)`) ne donne pas exactement la même hauteur
                # cumulée que les 2 boutons PDF/GeoPackage de la
                # colonne du milieu. 1rem (≈ 16 px) descend le texte
                # juste au niveau du centre des badges, ce que voulait
                # l'utilisateur.
                style = "white-space: nowrap; padding-top: 1rem;",
                if (i18n$language == "fr") "Taille image Max 5 Mo, PNG/JPG" else "Max 5 MB, PNG/JPG"
              )
            )
          ),

          # Global score
          shiny::uiOutput(ns("global_score"))
        )
      )
    ),

    # Radar + Summary table + Comments (3 columns)
    bslib::layout_columns(
      col_widths = c(4, 4, 4),
      bslib::card(
        bslib::card_header(
          class = "d-flex align-items-center gap-2",
          i18n$t("radar_title"),
          bslib::popover(
            htmltools::tags$span(
              class = "text-info",
              style = "cursor: help;",
              shiny::icon("circle-info", class = "fa-sm")
            ),
            htmltools::div(
              style = "max-height: 450px; overflow-y: auto; font-size: 0.85rem; line-height: 1.5;",
              # Title
              htmltools::div(
                style = "border-left: 4px solid #4a7c3f; padding: 10px 12px; background: #f4f8f2; margin-bottom: 10px;",
                htmltools::tags$div(
                  style = "font-weight: bold; font-size: 1rem; color: #3a6330; margin-bottom: 2px;",
                  htmltools::HTML("&#9670; "), i18n$t("radar_tip_title")
                ),
                htmltools::tags$em(style = "color: #555;", i18n$t("radar_tip_subtitle"))
              ),
              # Intro
              htmltools::p(
                style = "margin: 8px 0; text-align: justify;",
                htmltools::HTML(i18n$t("radar_tip_intro"))
              ),
              # Section: Ce que le radar ne dit pas
              htmltools::div(
                style = "background: #eaf1e6; padding: 5px 10px; margin: 10px 0 6px 0; border-radius: 3px; font-weight: bold; color: #3a6330;",
                htmltools::HTML("&#10006; &nbsp;"), i18n$t("radar_tip_not_title")
              ),
              htmltools::p(
                style = "margin: 0 0 8px 0; text-align: justify;",
                i18n$t("radar_tip_not_text")
              ),
              # Section: Ce que le radar dit
              htmltools::div(
                style = "background: #eaf1e6; padding: 5px 10px; margin: 10px 0 6px 0; border-radius: 3px; font-weight: bold; color: #3a6330;",
                htmltools::HTML("&#10004; &nbsp;"), i18n$t("radar_tip_yes_title")
              ),
              htmltools::p(
                style = "margin: 0 0 8px 0; text-align: justify;",
                htmltools::HTML(i18n$t("radar_tip_yes_text"))
              ),
              # Section: Comment l'utiliser
              htmltools::div(
                style = "background: #eaf1e6; padding: 5px 10px; margin: 10px 0 6px 0; border-radius: 3px; font-weight: bold; color: #3a6330;",
                htmltools::HTML("&#9658; &nbsp;"), i18n$t("radar_tip_how_title")
              ),
              htmltools::p(
                style = "margin: 0 0 10px 0; text-align: justify;",
                htmltools::HTML(i18n$t("radar_tip_how_text"))
              ),
              # Conclusion
              htmltools::tags$p(
                style = "font-style: italic; color: #555; margin: 10px 0 0 0; padding-top: 8px; border-top: 1px solid #ddd; text-align: justify;",
                htmltools::HTML(i18n$t("radar_tip_conclusion"))
              )
            ),
            options = list(customClass = "popover-lg"),
            title = NULL
          )
        ),
        bslib::card_body(
          shiny::plotOutput(ns("radar_plot"), height = "500px")
        )
      ),
      bslib::card(
        bslib::card_header(
          class = "d-flex align-items-center gap-2",
          i18n$t("summary_table_title"),
          bslib::popover(
            htmltools::tags$span(
              class = "text-info",
              style = "cursor: help;",
              shiny::icon("circle-info", class = "fa-sm")
            ),
            htmltools::div(
              style = "max-height: 450px; overflow-y: auto; font-size: 0.85rem; line-height: 1.5;",
              # Title
              htmltools::div(
                style = "border-left: 4px solid #4a7c3f; padding: 10px 12px; background: #f4f8f2; margin-bottom: 10px;",
                htmltools::tags$div(
                  style = "font-weight: bold; font-size: 1rem; color: #3a6330; margin-bottom: 2px; text-transform: uppercase;",
                  htmltools::HTML("&#9670; "), i18n$t("summary_tip_title")
                ),
                htmltools::tags$em(style = "color: #555;", i18n$t("summary_tip_subtitle"))
              ),
              # Intro
              htmltools::p(
                style = "margin: 8px 0; text-align: justify;",
                htmltools::HTML(i18n$t("summary_tip_intro"))
              ),
              # Section: Carre de base
              htmltools::div(
                style = "background: #eaf1e6; padding: 5px 10px; margin: 10px 0 4px 0; border-radius: 3px; font-weight: bold; color: #3a6330;",
                i18n$t("summary_tip_base_title")
              ),
              htmltools::p(style = "margin: 0 0 4px 0;", htmltools::HTML(i18n$t("summary_tip_base_text"))),
              # Section: Supports du vivant
              htmltools::div(
                style = "background: #eaf1e6; padding: 5px 10px; margin: 6px 0 4px 0; border-radius: 3px; font-weight: bold; color: #3a6330;",
                i18n$t("summary_tip_support_title")
              ),
              htmltools::p(style = "margin: 0 0 4px 0;", htmltools::HTML(i18n$t("summary_tip_support_text"))),
              # Section: Dimensions humaines
              htmltools::div(
                style = "background: #eaf1e6; padding: 5px 10px; margin: 6px 0 4px 0; border-radius: 3px; font-weight: bold; color: #3a6330;",
                i18n$t("summary_tip_human_title")
              ),
              htmltools::p(style = "margin: 0 0 8px 0;", htmltools::HTML(i18n$t("summary_tip_human_text"))),
              # Section: Acrostiche VIVREENFORET
              htmltools::div(
                style = "background: #eaf1e6; padding: 5px 10px; margin: 10px 0 6px 0; border-radius: 3px; font-weight: bold; color: #3a6330;",
                htmltools::HTML("&#9733; &nbsp;"), i18n$t("summary_tip_acrostic_title")
              ),
              htmltools::p(
                style = "margin: 0 0 10px 0; line-height: 1.7;",
                htmltools::HTML(i18n$t("summary_tip_acrostic"))
              ),
              # Conclusion
              htmltools::tags$p(
                style = "font-style: italic; color: #555; margin: 10px 0 0 0; padding-top: 8px; border-top: 1px solid #ddd; text-align: justify;",
                i18n$t("summary_tip_conclusion")
              )
            ),
            options = list(customClass = "popover-lg"),
            title = NULL
          )
        ),
        bslib::card_body(
          shiny::tableOutput(ns("summary_table"))
        )
      ),
      bslib::card(
        bslib::card_header(
          class = "d-flex justify-content-between align-items-center flex-wrap gap-2",
          htmltools::div(
            class = "d-flex align-items-center gap-2",
            i18n$t("comments_title"),
            bslib::popover(
              htmltools::tags$span(
                class = "text-info",
                style = "cursor: help;",
                shiny::icon("circle-info", class = "fa-sm")
              ),
              htmltools::HTML(i18n$t("comments_markdown_tooltip")),
              title = i18n$t("comments_markdown_tooltip_title")
            ),
            bslib::tooltip(
              shiny::tags$div(
                class = "form-check form-switch mb-0 ms-1",
                shiny::tags$input(type = "checkbox", class = "form-check-input",
                                  id = ns("fill_all_comments"), role = "switch"),
                shiny::tags$label(class = "form-check-label small text-muted",
                                  `for` = ns("fill_all_comments"),
                                  i18n$t("fill_all_label"))
              ),
              i18n$t("fill_all_tooltip")
            )
          ),
          htmltools::div(
            class = "d-flex align-items-center gap-2",
            htmltools::div(
              class = "expert-select-inline",
              shiny::selectInput(
                ns("expert_profile"),
                label = NULL,
                choices = get_expert_choices(opts$language),
                selected = "generalist",
                width = "auto"
              )
            ),
            shiny::actionButton(
              ns("ai_generate"),
              label = i18n$t("ai_generate"),
              icon = shiny::icon("robot"),
              class = "btn-outline-primary btn-sm"
            )
          )
        ),
        bslib::card_body(
          shiny::textAreaInput(
            ns("synthesis_comments"),
            label = NULL,
            placeholder = i18n$t("synthesis_comments_placeholder"),
            rows = 12,
            width = "100%"
          ),
          # v0.56.0 — RAG « perspectives sourcées » : bloc « Sources
          # documentaires » rendu en dessous du commentaire IA quand
          # la récupération corpus a renvoyé ≥ 1 chunk. Le format
          # markdown vient de `nemeton::format_citations()` (titre i18n
          # géré côté cœur). uiOutput vide quand pas de sources →
          # zéro impact visuel sur le layout existant.
          shiny::uiOutput(ns("ai_sources"))
        )
      )
    )
  )
}


#' Placeholder for mod_family_ui
#'
#' @param id Module ID
#' @param family_code Character. Family code (C, B, W, etc.)
#' @return UI elements
#' @noRd
mod_family_ui <- function(id, family_code) {
  ns <- shiny::NS(id)
  opts <- get_app_options()
  i18n <- get_i18n(opts$language)
  family <- get_family_config(family_code)

  if (is.null(family)) {
    return(htmltools::div("Unknown family"))
  }

  family_name <- if (opts$language == "fr") family$name_fr else family$name_en

  htmltools::tagList(
    # Family header
    htmltools::div(
      class = "d-flex align-items-center mb-3",
      bsicons::bs_icon(family$icon, class = "me-2"),
      htmltools::span(family_name, class = "fw-bold me-2"),
      htmltools::span(
        class = "text-muted",
        paste0("\u2014 ", i18n$t(paste0(get_famille_col(family_code), "_desc")))
      )
    ),

    # Maps (dynamic: adapts to number of indicators)
    shiny::uiOutput(ns("maps_row")),

    # Data table + Statistics + Comments (3 columns)
    bslib::layout_columns(
      col_widths = c(4, 4, 4),
      bslib::card(
        bslib::card_header(i18n$t("data_table")),
        bslib::card_body(
          DT::dataTableOutput(ns("indicator_table"))
        )
      ),
      bslib::card(
        bslib::card_header(i18n$t("statistics_title")),
        bslib::card_body(
          shiny::uiOutput(ns("analysis_stats"))
        )
      ),
      bslib::card(
        bslib::card_header(
          class = "d-flex justify-content-between align-items-center flex-wrap gap-2",
          i18n$t("comments_title"),
          htmltools::div(
            class = "d-flex align-items-center gap-2",
            htmltools::div(
              class = "expert-select-inline",
              shiny::selectInput(
                ns("expert_profile"),
                label = NULL,
                choices = get_expert_choices(opts$language),
                selected = "generalist",
                width = "auto"
              )
            ),
            shiny::actionButton(
              ns("ai_generate"),
              label = i18n$t("ai_generate"),
              icon = shiny::icon("robot"),
              class = "btn-outline-primary btn-sm"
            )
          )
        ),
        bslib::card_body(
          shiny::textAreaInput(
            ns("analysis_comments"),
            label = NULL,
            placeholder = i18n$t("analysis_comments_placeholder"),
            rows = 12,
            width = "100%"
          ),
          # Bloc « Sources documentaires » de la famille : liste les sources
          # documentaires citées dans le commentaire (mêmes données RAG que la
          # synthèse). uiOutput vide quand le commentaire ne cite aucune source.
          shiny::uiOutput(ns("ai_sources"))
        )
      )
    ),

    # Missing indicators warning
    shiny::uiOutput(ns("missing_warning"))
  )
}

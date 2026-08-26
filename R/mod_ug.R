#' UG (Management Units) Module
#'
#' @description
#' Shiny module for managing Unites de Gestion (UG).
#' Boucle 1: table + leaflet map with tenement selection, merge/split/rename,
#' groupe d'amenagement assignment, and color-coded map display.
#'
#' @name mod_ug
#' @keywords internal
NULL


#' UG Module UI
#'
#' @param id Character. Module namespace ID.
#'
#' @return Shiny UI tag list.
#' @noRd
mod_ug_ui <- function(id) {
  # Composite UI (layout_sidebar) -- used when the UG module has its own tab.
  # When embedded in the Selection tab, use the helpers below instead:
  #   mod_ug_actions_bar(id), mod_ug_map_panel(id), mod_ug_table_panel(id)
  ns <- shiny::NS(id)

  opts <- get_app_options()
  lang <- opts$language
  i18n <- get_i18n(lang)

  bslib::layout_sidebar(
    fillable = TRUE,
    sidebar = bslib::sidebar(
      title = i18n$t("ug_title"),
      width = 350,
      mod_ug_actions_bar(id)
    ),
    bslib::navset_card_tab(
      bslib::nav_panel(
        title = i18n$t("ug_map_tab"),
        icon = bsicons::bs_icon("map"),
        mod_ug_map_panel(id)
      ),
      bslib::nav_panel(
        title = i18n$t("ug_table_tab"),
        icon = bsicons::bs_icon("table"),
        mod_ug_table_panel(id)
      )
    )
  )
}


#' UG map panel (leaflet output)
#'
#' @description
#' Lightweight UI helper producing just the tenement leaflet output.
#' Uses the given \code{id} as the module namespace.
#'
#' @param id Character. Module namespace ID (typically "ug").
#' @return Shiny tag with the leaflet output.
#' @noRd
mod_ug_map_panel <- function(id) {
  ns <- shiny::NS(id)
  opts <- get_app_options()
  i18n <- get_i18n(opts$language %||% "fr")

  htmltools::tagList(
    # Header bar with title + basemap toggles (same pattern as mod_map)
    bslib::card_header(
      class = "d-flex justify-content-between align-items-center py-2",

      # Title
      htmltools::span(
        bsicons::bs_icon("diagram-3"),
        i18n$t("ug_map_card_title"),
        class = "fw-semibold"
      ),

      # Controls
      htmltools::div(
        class = "d-flex gap-2 align-items-center",
        htmltools::div(
          class = "btn-group btn-group-sm",
          role = "group",
          `aria-label` = "Basemap selection",
          htmltools::tags$button(
            id = ns("basemap_osm"),
            type = "button",
            class = "btn action-button basemap-btn basemap-btn-active",
            `data-val` = 0,
            "OSM"
          ),
          htmltools::tags$button(
            id = ns("basemap_satellite"),
            type = "button",
            class = "btn action-button basemap-btn",
            `data-val` = 0,
            "Satellite"
          )
        )
      )
    ),

    # Body: leaflet map
    bslib::card_body(
      padding = 0,
      class = "p-0",
      htmltools::div(
        id = ns("ug_map_container"),
        style = "height: 100%; min-height: 500px; position: relative;",
        leaflet::leafletOutput(ns("ug_map"), height = "100%")
      )
    ),

    # Footer: tenement count + surface ratio
    bslib::card_footer(
      class = "py-2",
      htmltools::div(
        class = "d-flex justify-content-between align-items-center text-muted small",
        shiny::textOutput(ns("ug_map_count"), inline = TRUE),
        shiny::textOutput(ns("ug_map_surface"), inline = TRUE)
      )
    )
  )
}


#' Replace the current project with a freshly built one
#'
#' @description
#' An import REPLACES: the previous project is deleted with all its
#' components - parcels, UGFs, indicators, comments, exports - and the new one
#' takes its place in `app_state`.
#'
#' Two invariants live here rather than in the caller.
#'
#' **Nothing is destroyed without a replacement.** The guard returns early on a
#' project that carries no id, so a half-finished import can never leave the
#' user with nothing. The caller reinforces it by calling this only once the
#' new project is created, loaded and crossed.
#'
#' **`project_id` moves with `current_project`.** It carries the project-lock
#' life cycle (`app_server.R` observes it) and it is what `save_comments()`
#' reads in `mod_synthesis` / `mod_family`. Setting one without the other left
#' the previous project locked and wrote the new project's comments into the
#' old project's directory.
#'
#' @param app_state Reactive values. Application state.
#' @param charge List. The freshly loaded project that takes over.
#'
#' @return The new project id, invisibly; `NULL` when nothing was done.
#' @noRd
.remplacer_projet_courant <- function(app_state, charge) {
  pid <- charge$id %||% charge$metadata$id
  if (is.null(pid) || !nzchar(pid)) return(invisible(NULL))

  ancien <- shiny::isolate(app_state$project_id) %||%
    shiny::isolate(app_state$current_project$id)
  if (!is.null(ancien) && nzchar(ancien) && !identical(ancien, pid)) {
    try(delete_project(ancien), silent = TRUE)
  }

  app_state$project_id <- pid
  app_state$current_project <- charge

  # Les composantes de session qui appartenaient au projet detruit. Sans cela
  # ses commentaires seraient re-sauves dans le nouveau a la premiere edition.
  app_state$family_comments <- list()
  app_state$clear_all_comments <- Sys.time()
  app_state$comments_refresh <-
    (shiny::isolate(app_state$comments_refresh) %||% 0L) + 1L

  # Calcul en cours, minuteur, cartes de progression : locaux a mod_home, d'ou
  # ce signal plutot qu'un appel direct.
  app_state$project_replaced <- Sys.time()
  app_state$refresh_projects <- Sys.time()

  invisible(pid)
}


#' UG table panel (DT output)
#'
#' @param id Character. Module namespace ID.
#' @return Shiny tag with the UG data table.
#' @noRd
mod_ug_table_panel <- function(id) {
  ns <- shiny::NS(id)
  opts <- get_app_options()
  i18n <- get_i18n(opts$language %||% "fr")

  bslib::card_body(
    htmltools::div(
      class = "d-flex justify-content-between align-items-center mb-2",
      shiny::h5(i18n$t("ug_table_title"), class = "mb-0"),
      shiny::textOutput(ns("ug_summary"), inline = TRUE)
    ),
    DT::dataTableOutput(ns("ug_table"))
  )
}


#' UG map-actions bar (map-based + global actions)
#'
#' @description
#' Buttons that operate on the map selection or are global to the
#' project (create UG from map selection, move selection to UG,
#' recompute, import/undo split). Map selection info.
#'
#' @param id Character. Module namespace ID.
#' @return Shiny tag list.
#' @noRd
mod_ug_map_actions_bar <- function(id) {
  ns <- shiny::NS(id)
  opts <- get_app_options()
  i18n <- get_i18n(opts$language %||% "fr")

  htmltools::tagList(
    htmltools::div(
      class = "d-grid gap-2 mb-3",

      shiny::actionButton(
        ns("btn_create_from_map"),
        label = i18n$t("ug_create_from_map"),
        icon = shiny::icon("plus-circle"),
        class = "btn-success",
        width = "100%"
      ),

      shiny::actionButton(
        ns("btn_move_to_ug"),
        label = i18n$t("ug_move_to"),
        icon = shiny::icon("arrow-right-arrow-left"),
        class = "btn-success",
        width = "100%"
      ),

      shiny::actionButton(
        ns("btn_import_split"),
        label = i18n$t("ug_import_split"),
        icon = shiny::icon("file-import"),
        class = "btn-outline-info",
        width = "100%"
      ),

      shiny::downloadButton(
        ns("btn_export_split"),
        label = i18n$t("ug_export_split"),
        icon = shiny::icon("file-export"),
        class = "btn-outline-info w-100"
      ),

      shiny::actionButton(
        ns("btn_undo_split"),
        label = i18n$t("ug_undo_split"),
        icon = shiny::icon("rotate-left"),
        class = "btn-outline-secondary",
        width = "100%"
      )
    ),

    shiny::hr(),

    # ---- Parcellaire forestier ONF (spec 046) ---------------------------
    # En foret publique la parcelle CADASTRALE n'est pas l'unite de gestion :
    # la parcelle FORESTIERE l'est. UNE seule action : croiser le parcellaire
    # forestier avec les parcelles du projet, ce qui GARDE ces dernieres (le
    # bien de l'utilisateur) et decrit chaque UGF comme les morceaux de
    # parcelles dont elle est faite.
    #
    # v0.130.0.9001 - un second bouton " Importer le parcellaire ONF " a
    # existe, qui REMPLACAIT les parcelles du projet. Retire : il partait de la
    # MEME emprise et produisait les MEMES UGF, en jetant la composition
    # cadastrale (donc `part_ugf`, le " vous ne detenez que 40 % de cette
    # parcelle forestiere "). Un cas degrade du croisement, destructif de
    # surcroit, qui coutait un bouton et une modale de confirmation.
    htmltools::div(
      class = "mb-2",
      htmltools::tags$strong(i18n$t("onf_section")),
      htmltools::tags$p(
        class = "text-muted small mb-2",
        i18n$t("onf_grain_parcelle")
      ),
      # Domanialite, purge et seuil ont quitte cette barre pour
      # « Parametres > Sources & parametres » : ce sont des CALIBRAGES, regles
      # une fois par massif, alors que le bouton ci-dessous est un geste qu'on
      # repete. Meme mouvement que les seuils FAST (v0.126.2) et les calibrages
      # de quatre onglets (v0.128.0). Le rappel des valeurs en vigueur reste
      # ici, avec le chemin pour les changer.
      shiny::uiOutput(ns("onf_params_rappel")),
      # La note de calage vit dans un " i " a cote du bouton plutot qu'en
      # paragraphe permanent : c'est une explication qu'on lit une fois, pas une
      # valeur qu'on surveille. Le paragraphe repoussait le bouton vers le bas.
      htmltools::div(
        class = "d-flex align-items-center gap-2 mb-2",
        htmltools::div(
          class = "flex-grow-1",
          shiny::actionButton(
            ns("btn_onf_croise"),
            label = i18n$t("onf_croise_btn"),
            icon = shiny::icon("code-branch"),
            class = "btn-primary",
            width = "100%"
          )
        ),
        info_popover(
          htmltools::tagList(
            htmltools::tags$p(i18n$t("onf_auto_select_note")),
            htmltools::tags$p(class = "mb-0", i18n$t("onf_caler_note"))
          ),
          placement = "left"
        )
      ),
      htmltools::tags$p(
        class = "text-muted small fst-italic mt-1 mb-0",
        i18n$t("onf_source_note")
      )
    ),

    shiny::hr(),

    # Map selection info
    shiny::uiOutput(ns("map_selection_info"))
  )
}


#' UG table-actions bar (buttons that act on the DT table selection)
#'
#' @description
#' Buttons that operate on the current DT table row selection: merge,
#' split, rename UGs. Also the groupe selector and UG detail panel.
#'
#' @param id Character. Module namespace ID.
#' @return Shiny tag list.
#' @noRd
mod_ug_table_actions_bar <- function(id) {
  ns <- shiny::NS(id)
  opts <- get_app_options()
  i18n <- get_i18n(opts$language %||% "fr")

  htmltools::tagList(
    # Import d'une liste de parcelles cadastrales (CSV). En TETE, et separe des
    # trois actions qui suivent : celles-la operent sur les lignes SELECTIONNEES
    # du tableau, celle-ci cree un PROJET entier et remplace le courant. Le
    # separateur et la mention de portee disent cette difference - sans quoi le
    # bouton se lit comme une quatrieme action de selection.
    htmltools::div(
      class = "d-grid gap-1 mb-2",
      shiny::actionButton(
        ns("btn_import_csv"),
        label = i18n$t("csv_import_btn"),
        icon = bsicons::bs_icon("filetype-csv"),
        class = "btn-outline-primary",
        width = "100%"
      ),
      htmltools::div(
        class = "text-muted small",
        i18n$t("csv_import_scope_hint")
      )
    ),

    shiny::hr(),

    htmltools::div(
      class = "d-grid gap-2 mb-3",

      shiny::actionButton(
        ns("btn_merge"),
        label = i18n$t("ug_merge"),
        icon = shiny::icon("object-group"),
        class = "btn-success",
        width = "100%"
      ),

      shiny::actionButton(
        ns("btn_split"),
        label = i18n$t("ug_split"),
        icon = shiny::icon("scissors"),
        class = "btn-warning",
        width = "100%"
      ),

      shiny::actionButton(
        ns("btn_rename"),
        label = i18n$t("ug_rename"),
        icon = shiny::icon("pen"),
        class = "btn-outline-secondary",
        width = "100%"
      )
    ),

    shiny::hr(),

    # Groupe d'amenagement selector
    htmltools::div(
      class = "mb-3",
      shiny::selectInput(
        ns("sel_groupe"),
        label = i18n$t("ug_group"),
        choices = c(
          "---" = "",
          "AMETS" = "AMETS", "AMER" = "AMER", "IRR" = "IRR",
          "TSF" = "TSF", "REGT" = "REGT", "REGF" = "REGF",
          "HSN" = "HSN", "HSY" = "HSY", "PROT" = "PROT", "ACC" = "ACC"
        ),
        selected = ""
      ),
      shiny::actionButton(
        ns("btn_set_groupe"),
        label = i18n$t("ug_apply_group"),
        icon = shiny::icon("tag"),
        class = "btn-outline-primary",
        width = "100%"
      )
    ),

    shiny::hr(),

    # UG detail panel
    htmltools::div(
      id = ns("detail_panel"),
      shiny::h6(i18n$t("ug_composition")),
      shiny::uiOutput(ns("ug_detail"))
    )
  )
}


#' UG actions bar (combined wrapper -- map + table)
#'
#' @description
#' Backward-compatible wrapper that includes both the map-action bar
#' and the table-action bar. Used by the legacy single-sidebar layout.
#'
#' @param id Character. Module namespace ID.
#' @return Shiny tag list with all UG actions.
#' @noRd
mod_ug_actions_bar <- function(id) {
  htmltools::tagList(
    mod_ug_map_actions_bar(id),
    shiny::hr(),
    mod_ug_table_actions_bar(id)
  )
}


#' UG Module Server
#'
#' @param id Character. Module namespace ID.
#' @param app_state reactiveValues. Application state.
#'
#' @return NULL (called for side effects).
#' @noRd
mod_ug_server <- function(id, app_state) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    lang <- shiny::reactive(app_state$language %||% "fr")
    i18n <- shiny::reactive(get_i18n(lang()))

    # ================================================================
    # REACTIVE: UG state (projet with tenements/ugs)
    # ================================================================
    rv <- shiny::reactiveValues(
      projet_ug = NULL,          # projet list with $tenements, $ugs, $parcels
      selected_tenement_ids = character(0),  # tenements selected on the map
      map_needs_zoom = FALSE,    # flag: zoom to bounds on next map update
      pending_bbox = NULL,       # stored bbox for deferred zoom
      redraw_counter = 0L,       # incremented to force map polygon redraw
      onf_preview = NULL         # parcellaire ONF affiche en surcouche
    )

    # Current classification profile (ONF / CRPF / OFB / ...) driven by the
    # project metadata. Falls back to the config default. Determines the
    # dropdown label (e.g. "Groupe d'amenagement" / "Groupe" / "Zone"),
    # the available codes and the map legend.
    profile_key <- shiny::reactive({
      pk <- rv$projet_ug$metadata$groupes_profile
      if (is.null(pk) || !nzchar(pk)) {
        return(tryCatch(get_default_groupes_profile(),
                        error = function(e) "onf"))
      }
      pk
    })

    # Update the sidebar groupe selector (label + choices) when the profile
    # changes -- i.e. when a project with a different profile is loaded.
    shiny::observe({
      pk <- profile_key()
      shiny::updateSelectInput(
        session,
        "sel_groupe",
        label = get_groupes_field_label(pk, lang = lang()),
        choices = get_groupes_choices(pk, lang = lang(), include_empty = TRUE)
      )
    })

    # Helper: translate known English cli_abort messages emitted by the
    # domain/split functions into user-language text. Falls back to the
    # raw message when no match is found.
    translate_split_error <- function(msg) {
      if (is.null(msg) || length(msg) == 0) return("")
      lg <- lang()
      is_fr <- identical(lg, "fr")
      patterns <- list(
        list(re = "does not intersect any tenement",
             fr = "Le polygone ne recouvre aucun t\u00e8nement.",
             en = "The drawn polygon does not intersect any tenement."),
        list(re = "did not split any tenement",
             fr = "Le polygone n'a d\u00e9coup\u00e9 aucun t\u00e8nement (contacts de bord uniquement ou chevauchement trop faible).",
             en = "The drawn polygon did not split any tenement."),
        list(re = "does not cross any tenement",
             fr = "La ligne ne traverse aucun t\u00e8nement.",
             en = "The drawn line does not cross any tenement."),
        list(re = "did not split any tenement \\(only edge touches\\)",
             fr = "La ligne n'a d\u00e9coup\u00e9 aucun t\u00e8nement (contacts de bord uniquement).",
             en = "The drawn line did not split any tenement (only edge touches)."),
        list(re = "No polygon features found",
             fr = "Aucun polygone trouv\u00e9 dans la forme import\u00e9e.",
             en = "No polygon features found."),
        list(re = "No line features found",
             fr = "Aucune ligne trouv\u00e9e dans la forme import\u00e9e.",
             en = "No line features found."),
        list(re = "Tiling invariant violated",
             fr = "Les polygones import\u00e9s ne couvrent pas exactement la parcelle (\u00e9cart de surface d\u00e9tect\u00e9).",
             en = "Imported polygons do not cover the parcel exactly (area mismatch)."),
        list(re = "Invalid GeoJSON",
             fr = "Fichier GeoJSON invalide.",
             en = "Invalid GeoJSON."),
        list(re = "Project must have UG data",
             fr = "Le projet ne contient pas de donn\u00e9es UGF.",
             en = "Project must have UG data."),
        list(re = "Parcel not found",
             fr = "Parcelle introuvable.",
             en = "Parcel not found.")
      )
      for (p in patterns) {
        if (grepl(p$re, msg, ignore.case = TRUE)) {
          return(if (is_fr) p$fr else p$en)
        }
      }
      # Fallback: return the raw message
      msg
    }

    # Initialize UG data when project loads
    shiny::observe({
      project <- app_state$current_project
      if (is.null(project) || is.null(project$parcels)) {
        rv$projet_ug <- NULL
        rv$selected_tenement_ids <- character(0)
        rv$map_needs_zoom <- FALSE
        return()
      }

      # Reset map zoom flag for new project
      rv$map_needs_zoom <- TRUE
      rv$selected_tenement_ids <- character(0)

      # Try to load existing UG data
      if (has_ug_data(project)) {
        rv$projet_ug <- project
      } else if (!is.null(project$metadata$id)) {
        projet <- ensure_project_migrated(project$metadata$id, project)
        rv$projet_ug <- projet
        app_state$current_project$tenements <- projet$tenements
        app_state$current_project$ugs <- projet$ugs
      }
    }) |> shiny::bindEvent(app_state$current_project)

    # ================================================================
    # REACTIVE: UG listing for the table
    # ================================================================
    ug_listing <- shiny::reactive({
      projet <- rv$projet_ug
      rv$redraw_counter  # also invalidate when redraw is forced
      if (is.null(projet) || !has_ug_data(projet)) return(NULL)
      ug_list(projet)
    })

    # ================================================================
    # MAP: Render leaflet
    # ================================================================
    # Basemap state
    rv$basemap <- "osm"

    output$ug_map <- leaflet::renderLeaflet({
      m <- leaflet::leaflet() |>
        leaflet::addProviderTiles(
          leaflet::providers$OpenStreetMap,
          group = "basemap",
          layerId = "basemap_tiles"
        ) |>
        leaflet::addLayersControl(
          # "Selection" is an internal-only visual overlay: we still draw
          # the orange highlight via addPolygons(group = "Selection") and
          # wipe it with clearGroup("Selection"), but it doesn't belong
          # in the user-facing layers control (toggling it off wouldn't
          # actually deselect tenements -- the IDs live in rv$selected_tenement_ids).
          overlayGroups = c("UGF", "Tenements", "Dessin", "Parcellaire ONF"),
          options = leaflet::layersControlOptions(collapsed = FALSE)
        ) |>
        leaflet::setView(lng = 2.5, lat = 46.5, zoom = 6)

      # Add draw toolbar if leaflet.extras is available
      if (requireNamespace("leaflet.extras", quietly = TRUE)) {
        m <- m |>
          leaflet.extras::addDrawToolbar(
            targetGroup = "Dessin",
            polylineOptions = leaflet.extras::drawPolylineOptions(
              shapeOptions = leaflet.extras::drawShapeOptions(
                color = "#FF0000",
                weight = 3
              )
            ),
            circleOptions = FALSE,
            circleMarkerOptions = FALSE,
            markerOptions = FALSE,
            rectangleOptions = FALSE,
            polygonOptions = leaflet.extras::drawPolygonOptions(
              shapeOptions = leaflet.extras::drawShapeOptions(
                color = "#FF4500",
                fillColor = "#FF6347",
                fillOpacity = 0.3,
                weight = 2
              )
            ),
            editOptions = leaflet.extras::editToolbarOptions(
              edit = TRUE,
              remove = TRUE
            )
          )
      }

      m
    })

    # ================================================================
    # MAP: Basemap toggle (OSM / Satellite)
    # ================================================================
    shiny::observeEvent(input$basemap_osm, {
      rv$basemap <- "osm"
      leaflet::leafletProxy(ns("ug_map")) |>
        leaflet::clearGroup("basemap") |>
        leaflet::addProviderTiles(
          leaflet::providers$OpenStreetMap,
          group = "basemap",
          layerId = "basemap_tiles"
        )
      session$sendCustomMessage("toggleBasemapButtons", list(
        osmId = ns("basemap_osm"),
        satId = ns("basemap_satellite"),
        active = "osm"
      ))
    })

    shiny::observeEvent(input$basemap_satellite, {
      rv$basemap <- "satellite"
      leaflet::leafletProxy(ns("ug_map")) |>
        leaflet::clearGroup("basemap") |>
        leaflet::addProviderTiles(
          leaflet::providers$Esri.WorldImagery,
          group = "basemap",
          layerId = "basemap_tiles"
        )
      session$sendCustomMessage("toggleBasemapButtons", list(
        osmId = ns("basemap_osm"),
        satId = ns("basemap_satellite"),
        active = "satellite"
      ))
    })

    # ================================================================
    # MAP FOOTER: tenement count and surface summary
    # ================================================================
    output$ug_map_count <- shiny::renderText({
      projet <- rv$projet_ug
      if (is.null(projet) || !has_ug_data(projet)) return("")
      n <- nrow(projet$tenements)
      sprintf(i18n()$t("ug_map_summary_count"), n)
    })

    output$ug_map_surface <- shiny::renderText({
      projet <- rv$projet_ug
      if (is.null(projet) || !has_ug_data(projet)) return("")

      tenements <- projet$tenements
      # Cadastral surface (authoritative, from contenance)
      surf_cadastrale_ha <- sum(tenements$surface_m2, na.rm = TRUE) / 10000
      # SIG surface (geometric, via st_area) -- fallback to st_area if column missing
      surf_sig_m2 <- if (!is.null(tenements$surface_sig_m2)) {
        tenements$surface_sig_m2
      } else {
        as.numeric(sf::st_area(tenements))
      }
      surf_sig_ha <- sum(surf_sig_m2, na.rm = TRUE) / 10000

      sprintf(
        i18n()$t("ug_map_summary_surface"),
        format(round(surf_cadastrale_ha, 2), nsmall = 2),
        format(round(surf_sig_ha, 2), nsmall = 2)
      )
    })

    # ================================================================
    # MAP: Update polygons when UG data changes OR tab becomes visible
    # ================================================================
    shiny::observe({
      # Dependencies: data change AND explicit redraw requests
      projet <- rv$projet_ug
      rv$redraw_counter  # invalidate on redraw trigger
      if (is.null(projet) || !has_ug_data(projet)) return()

      .t_map0 <- Sys.time()  # PERF - chrono rendu carte UGF (cf. NEMETON_PERF_TRACE)

      tenements <- projet$tenements
      ugs <- projet$ugs

      # Ensure WGS84 for leaflet
      if (!is.na(sf::st_crs(tenements)) && sf::st_crs(tenements)$epsg != 4326L) {
        tenements <- sf::st_transform(tenements, 4326)
      }

      # Sort tenements by descending SIG surface so larger polygons
      # are drawn FIRST and smaller ones (typically nested inclusions
      # the user added after the initial split -- e.g. a clearing
      # carved out of a large tenement) land ON TOP and stay visible.
      if ("surface_sig_m2" %in% names(tenements) && nrow(tenements) > 1) {
        tenements <- tenements[order(-tenements$surface_sig_m2), , drop = FALSE]
      }

      # Compute fill colors per tenement (based on UG groupe or index).
      # Uses the project's classification profile (ONF / CRPF / OFB / ...)
      # to resolve groupe -> color from the YAML config.
      pk <- profile_key()
      ug_index_map <- stats::setNames(seq_len(nrow(ugs)), ugs$ug_id)
      fill_colors <- vapply(seq_len(nrow(tenements)), function(i) {
        uid <- tenements$ug_id[i]
        ug_row <- ugs[ugs$ug_id == uid, ]
        if (nrow(ug_row) == 0) return("#CCCCCC")
        idx <- ug_index_map[[uid]]
        ug_color(ug_row$groupe[1], idx, profile_key = pk)
      }, character(1))

      # Labels for hover
      atom_labels <- vapply(seq_len(nrow(tenements)), function(i) {
        uid <- tenements$ug_id[i]
        ug_row <- ugs[ugs$ug_id == uid, ]
        ug_label <- if (nrow(ug_row) > 0) ug_row$label[1] else "?"
        groupe_str <- if (nrow(ug_row) > 0 && !is.na(ug_row$groupe[1])) {
          paste0(" [", ug_row$groupe[1], "]")
        } else {
          ""
        }
        sprintf(
          "<b>%s</b>%s<br>Tenement: %s<br>Surface: %s m\u00b2",
          ug_label, groupe_str,
          tenements$tenement_id[i],
          format(round(tenements$surface_m2[i]), big.mark = " ")
        )
      }, character(1))

      # Clear and redraw tenements (selection is handled by separate overlay)
      proxy <- leaflet::leafletProxy(ns("ug_map"))
      proxy |>
        leaflet::clearGroup("Tenements") |>
        leaflet::clearGroup("UGF") |>
        leaflet::clearGroup("Selection") |>
        leaflet::addPolygons(
          data = tenements,
          group = "Tenements",
          layerId = tenements$tenement_id,
          fillColor = fill_colors,
          fillOpacity = 0.5,
          color = "#333333",
          weight = 1,
          label = lapply(atom_labels, htmltools::HTML),
          # Taille laissee a `.leaflet-tooltip` (custom.css), regle unique de
          # l'app : un `textsize` ou un `font-size` inline la surpasserait.
          labelOptions = leaflet::labelOptions(
            style = list("background" = "white")
          ),
          highlightOptions = leaflet::highlightOptions(
            weight = 3,
            fillOpacity = 0.7,
            bringToFront = TRUE
          )
        )

      # Clear selection state when tenements are redrawn (new project)
      rv$selected_tenement_ids <- character(0)

      # Add UG dissolved boundaries as an overlay
      tryCatch({
        # PERF - reutiliser la geometrie UGF deja dissoute par
        # attach_indicators_sf (projet$indicators_sf) plutot que de
        # relancer un ug_build_sf() complet (un st_union() par UGF). Ce
        # cache porte les memes colonnes que ug_build_sf (label, groupe,
        # n_tenements, surface_m2, geometrie) puisqu'indicators_sf en
        # derive par merge. Fallback sur ug_build_sf() tant que l'attache
        # differee n'a pas encore tourne (1er rendu avant le later()).
        ug_sf <- if (!is.null(projet$indicators_sf) &&
                     inherits(projet$indicators_sf, "sf") &&
                     nrow(projet$indicators_sf) > 0 &&
                     all(c("label", "groupe", "n_tenements", "surface_m2") %in%
                         names(projet$indicators_sf))) {
          .perf_time("ug_sf (cache indicators_sf, mod_ug)", projet$indicators_sf)
        } else {
          .perf_time("ug_build_sf (rendu carte mod_ug, fallback)", ug_build_sf(projet))
        }
        if (!is.null(ug_sf) && nrow(ug_sf) > 0) {
          if (!is.na(sf::st_crs(ug_sf)) && sf::st_crs(ug_sf)$epsg != 4326L) {
            ug_sf <- sf::st_transform(ug_sf, 4326)
          }

          ug_colors <- vapply(seq_len(nrow(ug_sf)), function(i) {
            ug_color(ug_sf$groupe[i], i, profile_key = pk)
          }, character(1))

          ug_labels <- vapply(seq_len(nrow(ug_sf)), function(i) {
            groupe_str <- if (!is.na(ug_sf$groupe[i])) {
              paste0(" [", ug_sf$groupe[i], "]")
            } else {
              ""
            }
            sprintf(
              "<b>%s</b>%s<br>%d tenement(s) | %s ha",
              ug_sf$label[i], groupe_str,
              ug_sf$n_tenements[i],
              format(round(ug_sf$surface_m2[i] / 10000, 2), nsmall = 2)
            )
          }, character(1))

          proxy |>
            leaflet::addPolygons(
              data = ug_sf,
              group = "UGF",
              fillColor = ug_colors,
              fillOpacity = 0.5,
              color = ug_colors,
              weight = 1,
              label = lapply(ug_labels, htmltools::HTML),
              # Gras conserve (c'est l'etiquette mise en avant) ; la TAILLE, elle,
              # vient de `.leaflet-tooltip` (custom.css), regle unique de l'app.
              labelOptions = leaflet::labelOptions(
                style = list("font-weight" = "bold")
              )
            )
        }
      }, error = function(e) {
        cli::cli_warn("Failed to render UG boundaries: {e$message}")
      })

      # Store bbox and attempt zoom (may fail if tab not visible)
      if (isTRUE(rv$map_needs_zoom)) {
        bbox <- sf::st_bbox(tenements)
        rv$pending_bbox <- bbox
        proxy |> leaflet::fitBounds(
          lng1 = bbox[["xmin"]], lat1 = bbox[["ymin"]],
          lng2 = bbox[["xmax"]], lat2 = bbox[["ymax"]]
        )
        rv$map_needs_zoom <- FALSE
      }

      # Add legend + re-add layers control (clearControls also removes it).
      # Then explicitly show every overlay group so leaflet doesn't keep
      # them hidden after the control re-creation.
      # Build the legend from the active profile (ONF / CRPF / OFB / ...).
      profile_colors <- get_groupes_colors(pk)
      groupe_vals <- ugs$groupe[!is.na(ugs$groupe) & nzchar(ugs$groupe)]
      used_codes <- intersect(names(profile_colors), unique(groupe_vals))
      legend_title <- get_groupes_field_label(pk, lang = lang())

      proxy |>
        leaflet::clearControls() |>
        leaflet::addLayersControl(
          overlayGroups = c("UGF", "Tenements", "Dessin", "Parcellaire ONF"),
          options = leaflet::layersControlOptions(collapsed = FALSE)
        ) |>
        leaflet::showGroup("UGF") |>
        leaflet::showGroup("Tenements") |>
        leaflet::showGroup("Dessin")

      if (length(used_codes) > 0) {
        legend_labels <- vapply(used_codes, function(c) {
          sprintf("%s - %s", c, get_groupe_label(c, pk, lang = lang()))
        }, character(1))
        proxy |>
          leaflet::addLegend(
            position = "bottomright",
            colors = unname(profile_colors[used_codes]),
            labels = legend_labels,
            title = legend_title,
            opacity = 0.8
          )
      }

      if (.perf_trace_on()) {
        .dt_map <- as.numeric(difftime(Sys.time(), .t_map0, units = "secs")) * 1000
        cli::cli_inform(c("v" = "\u23f1 [perf] mod_ug rendu carte UGF TOTAL ({nrow(tenements)} tenements): {sprintf('%.0f', .dt_map)} ms"))
      }
    })

    # ================================================================
    # MAP: Re-zoom when the tenement map becomes visible
    # ================================================================
    # Leaflet fitBounds fails silently when the map container has 0 size
    # (hidden tab). The tenement map lives in a sub-tab of "Selection"
    # (home-main_tabs = "tenements"). Every time the user navigates to
    # that sub-tab, we:
    #  1. Recompute the bbox from the current rv$projet_ug (always fresh)
    #  2. Ask leaflet to invalidate its size (redetect container dims)
    #  3. Apply fitBounds with a small delay to let the DOM settle
    shiny::observe({
      root_session <- session$userData$root_session
      if (is.null(root_session)) return()

      top_nav <- root_session$input$main_nav
      sub_nav <- root_session$input[["home-main_tabs"]]

      if (is.null(top_nav) || top_nav != "selection") return()
      if (is.null(sub_nav) || sub_nav != "tenements") return()

      projet <- shiny::isolate(rv$projet_ug)
      if (is.null(projet) || !has_ug_data(projet)) return()

      # Always recompute bbox from current data (not cached)
      tenements <- projet$tenements
      if (!is.na(sf::st_crs(tenements)) && sf::st_crs(tenements)$epsg != 4326L) {
        tenements <- sf::st_transform(tenements, 4326)
      }
      bbox <- tryCatch(sf::st_bbox(tenements), error = function(e) NULL)
      if (is.null(bbox)) return()

      # Force re-drawing the polygons -- they may have been issued while the
      # tab was hidden (map not in DOM) and silently dropped by leaflet.
      rv$redraw_counter <- shiny::isolate(rv$redraw_counter) + 1L

      # Force leaflet to re-detect its container size (critical for hidden tabs)
      later::later(function() {
        proxy <- leaflet::leafletProxy(ns("ug_map"), session = session)
        # Trigger invalidateSize via custom message to the map
        session$sendCustomMessage("leafletInvalidateSize", list(
          id = ns("ug_map")
        ))
        proxy |> leaflet::fitBounds(
          lng1 = bbox[["xmin"]], lat1 = bbox[["ymin"]],
          lng2 = bbox[["xmax"]], lat2 = bbox[["ymax"]]
        )
      }, delay = 0.3)

      # Consume pending_bbox if any (kept for compat)
      rv$pending_bbox <- NULL
    })

    # ================================================================
    # TABLE: force redraw when the "Tableau" sub-tab becomes visible
    # ================================================================
    shiny::observe({
      root_session <- session$userData$root_session
      if (is.null(root_session)) return()

      top_nav <- root_session$input$main_nav
      sub_nav <- root_session$input[["home-main_tabs"]]

      if (is.null(top_nav) || top_nav != "selection") return()
      if (is.null(sub_nav) || sub_nav != "table_ug") return()

      projet <- shiny::isolate(rv$projet_ug)
      if (is.null(projet) || !has_ug_data(projet)) return()

      # Bump counter to invalidate ug_listing reactive
      rv$redraw_counter <- shiny::isolate(rv$redraw_counter) + 1L
    })

    # ================================================================
    # MAP: Click handler for tenement selection (same pattern as mod_map.R)
    # ================================================================
    shiny::observeEvent(input$ug_map_shape_click, {
      click <- input$ug_map_shape_click
      if (is.null(click) || is.null(click$id)) return()

      tenement_id <- click$id
      projet <- rv$projet_ug
      if (is.null(projet)) return()

      # Only handle clicks on tenements (not UG overlay)
      if (!tenement_id %in% projet$tenements$tenement_id) return()

      if (tenement_id %in% rv$selected_tenement_ids) {
        # Deselect: remove from selection overlay
        rv$selected_tenement_ids <- setdiff(rv$selected_tenement_ids, tenement_id)
        leaflet::leafletProxy(ns("ug_map")) |>
          leaflet::removeShape(paste0("sel_", tenement_id))
      } else {
        # Select: add to selection overlay
        rv$selected_tenement_ids <- c(rv$selected_tenement_ids, tenement_id)
        update_tenement_selection_style(tenement_id, selected = TRUE)
      }
    })

    # Helper: add/remove selection overlay for a single tenement
    update_tenement_selection_style <- function(tenement_id, selected) {
      projet <- rv$projet_ug
      if (is.null(projet)) return()

      if (selected) {
        tenements <- projet$tenements
        tenement <- tenements[tenements$tenement_id == tenement_id, ]
        if (nrow(tenement) == 0) return()

        # Ensure WGS84
        if (!is.na(sf::st_crs(tenement)) && sf::st_crs(tenement)$epsg != 4326L) {
          tenement <- sf::st_transform(tenement, 4326)
        }

        leaflet::leafletProxy(ns("ug_map")) |>
          leaflet::addPolygons(
            data = tenement,
            layerId = paste0("sel_", tenement_id),
            group = "Selection",
            color = "#FF4500",
            weight = 3,
            fillColor = "#FF6347",
            fillOpacity = 0.4,
            options = leaflet::pathOptions(interactive = FALSE)
          )
      } else {
        leaflet::leafletProxy(ns("ug_map")) |>
          leaflet::removeShape(paste0("sel_", tenement_id))
      }
    }

    # Clear all selection overlays (used by clear button and project load)
    clear_tenement_selection <- function() {
      leaflet::leafletProxy(ns("ug_map")) |>
        leaflet::clearGroup("Selection")
      rv$selected_tenement_ids <- character(0)
    }

    # ================================================================
    # ================================================================
    # MAP: Handle drawn shapes (interactive split)
    # ================================================================
    # When the user finishes drawing on the map:
    #   - LINESTRING -> split tenements crossed by that line
    #   - POLYGON / RECTANGLE -> split tenements crossed by that polygon
    # We key off input$ug_map_draw_new_feature (fires with ONLY the
    # latest drawn shape) -- draw_all_features accumulates every shape
    # ever drawn, which caused a polygon drawn earlier to "win" over a
    # polyline drawn afterwards.
    shiny::observeEvent(input$ug_map_draw_new_feature, {
      feat <- input$ug_map_draw_new_feature
      if (is.null(feat) || is.null(feat$geometry)) return()

      projet <- rv$projet_ug
      if (is.null(projet)) return()

      geom_type <- feat$geometry$type %||% ""
      is_line  <- geom_type %in% c("LineString", "MultiLineString")
      is_poly  <- geom_type %in% c("Polygon", "MultiPolygon")
      if (!is_line && !is_poly) return()

      # Wrap the single feature in a FeatureCollection for sf::st_read
      fc <- list(type = "FeatureCollection", features = list(feat))
      rv$drawn_geojson <- jsonlite::toJSON(fc, auto_unbox = TRUE)

      n_lines <- if (is_line) 1L else 0L
      n_polys <- if (is_poly) 1L else 0L
      tenements <- projet$tenements

      cli::cli_h2("Nouveau trac\u00e9 : {geom_type}")
      cli::cli_alert_info("Nombre de t\u00e8nements dans le projet : {nrow(tenements)}")

      # ----- Case 1: LINE drawn -> auto-split all crossed tenements -----
      if (is_line) {
        cli::cli_alert_info("Type : POLYLIGNE \u2014 analyse du trac\u00e9...")
        # Preview: planar GEOS on Lambert 93 (matches the backend
        # which must use GEOS because lwgeom::st_split has no S2 mode).
        n_affected <- tryCatch({
          line_sf <- sf::st_read(rv$drawn_geojson, quiet = TRUE)
          if (is.na(sf::st_crs(line_sf))) {
            sf::st_crs(line_sf) <- 4326
          }
          if (!is.na(sf::st_crs(tenements)) &&
              sf::st_crs(line_sf) != sf::st_crs(tenements)) {
            line_sf <- sf::st_transform(line_sf, sf::st_crs(tenements))
          }
          prev_s2 <- sf::sf_use_s2()
          sf::sf_use_s2(FALSE)
          on.exit(sf::sf_use_s2(prev_s2), add = TRUE)
          tn_work <- tenements
          if (!is.na(sf::st_crs(tenements)) &&
              isTRUE(sf::st_is_longlat(tenements))) {
            tn_work  <- sf::st_transform(tenements, 2154)
            line_sf  <- sf::st_transform(line_sf, 2154)
          }
          cutting_line <- sf::st_union(sf::st_geometry(line_sf))
          n <- sum(sf::st_intersects(sf::st_geometry(tn_work), cutting_line,
                                     sparse = FALSE)[, 1])
          cli::cli_alert_success(
            "POLYLIGNE : traverse {n} t\u00e8nement{?s} (sur {nrow(tenements)})"
          )
          n
        }, error = function(e) {
          cli::cli_alert_danger(
            "POLYLIGNE : \u00e9chec du preview ({e$message})"
          )
          0L
        })

        if (n_affected == 0) {
          shiny::showNotification(
            i18n()$t("ug_line_split_no_hit"),
            type = "warning",
            duration = 8
          )
          return()
        }

        shiny::showModal(shiny::modalDialog(
          title = i18n()$t("ug_line_split_title"),
          shiny::p(sprintf(i18n()$t("ug_line_split_desc"), n_affected)),
          footer = htmltools::tagList(
            shiny::modalButton(i18n()$t("cancel")),
            shiny::actionButton(
              ns("confirm_line_split"),
              i18n()$t("ug_split_apply"),
              class = "btn-info",
              icon = shiny::icon("scissors")
            )
          )
        ))
        return()
      }

      # ----- Case 2: POLYGON drawn -> auto-split all crossed tenements -----
      if (!is_poly) return()
      cli::cli_alert_info("Type : POLYGONE \u2014 analyse du trac\u00e9...")
      # Quick preview: how many tenements does the polygon cross?
      # Mirror the backend: S2 spherical intersection on 4326 when
      # possible, fall back to planar on Lambert 93 if S2 rejects.
      n_affected <- tryCatch({
        polys_sf <- sf::st_read(rv$drawn_geojson, quiet = TRUE)
        if (is.na(sf::st_crs(polys_sf))) {
          sf::st_crs(polys_sf) <- 4326
        }
        if (!is.na(sf::st_crs(tenements)) &&
            sf::st_crs(polys_sf) != sf::st_crs(tenements)) {
          polys_sf <- sf::st_transform(polys_sf, sf::st_crs(tenements))
        }
        prev_s2 <- sf::sf_use_s2()
        sf::sf_use_s2(TRUE)
        on.exit(sf::sf_use_s2(prev_s2), add = TRUE)
        polys_sf <- tryCatch(sf::st_make_valid(polys_sf),
                             error = function(e) polys_sf)
        cutter <- sf::st_union(sf::st_geometry(polys_sf))
        tryCatch({
          n <- sum(sf::st_intersects(sf::st_geometry(tenements), cutter,
                                     sparse = FALSE)[, 1])
          cli::cli_alert_success(
            "POLYGONE (S2/4326) : traverse {n} t\u00e8nement{?s} (sur {nrow(tenements)})"
          )
          n
        }, error = function(e) {
          cli::cli_alert_warning(
            "POLYGONE : S2 a \u00e9chou\u00e9, repli sur 2154 ({e$message})"
          )
          sf::sf_use_s2(FALSE)
          tn_m <- if (isTRUE(sf::st_is_longlat(tenements))) {
            sf::st_transform(tenements, 2154)
          } else tenements
          cu_m <- if (isTRUE(sf::st_is_longlat(cutter))) {
            sf::st_transform(cutter, 2154)
          } else cutter
          n <- sum(sf::st_intersects(sf::st_geometry(tn_m), cu_m,
                                     sparse = FALSE)[, 1])
          cli::cli_alert_success(
            "POLYGONE (GEOS/2154) : traverse {n} t\u00e8nement{?s} (sur {nrow(tenements)})"
          )
          n
        })
      }, error = function(e) {
        cli::cli_alert_danger(
          "POLYGONE : \u00e9chec du preview ({e$message})"
        )
        0L
      })

      if (n_affected == 0) {
        shiny::showNotification(
          i18n()$t("ug_poly_split_no_hit"),
          type = "warning",
          duration = 8
        )
        return()
      }

      shiny::showModal(shiny::modalDialog(
        title = i18n()$t("ug_poly_split_title"),
        shiny::p(sprintf(i18n()$t("ug_poly_split_desc"), n_affected)),
        footer = htmltools::tagList(
          shiny::modalButton(i18n()$t("cancel")),
          shiny::actionButton(
            ns("confirm_draw_split"),
            i18n()$t("ug_split_apply"),
            class = "btn-info",
            icon = shiny::icon("scissors")
          )
        )
      ))
    })

    # Confirm POLYGON split (auto-split all tenements crossed by polygon)
    shiny::observeEvent(input$confirm_draw_split, {
      if (deny_if_readonly(app_state)) return()
      shiny::removeModal()

      geojson <- rv$drawn_geojson
      if (is.null(geojson)) return()

      tryCatch({
        projet <- rv$projet_ug
        projet <- tenement_split_by_drawn_polygon(projet, geojson)

        if (!is.null(projet$metadata$id)) {
          save_ug_data(projet$metadata$id, projet)
        }
        rv$projet_ug <- projet
        rv$redraw_counter <- shiny::isolate(rv$redraw_counter) + 1L
        rv$drawn_geojson <- NULL
        clear_tenement_selection()
        app_state$current_project$tenements <- projet$tenements
        app_state$current_project$ugs <- projet$ugs

        # Clear drawn shapes from the map (leaflet group + JS fallback).
        leaflet::leafletProxy(ns("ug_map")) |>
          leaflet::clearGroup("Dessin")
        session$sendCustomMessage("leafletClearDrawn", list(id = ns("ug_map")))

        shiny::showNotification(
          i18n()$t("ug_poly_split_success"),
          type = "message"
        )
      }, error = function(e) {
        shiny::showNotification(
          paste(i18n()$t("ug_split_error"), translate_split_error(e$message)),
          type = "error",
          duration = 10
        )
      })
    })

    # Confirm LINE split (auto-split all tenements crossed by the line)
    shiny::observeEvent(input$confirm_line_split, {
      if (deny_if_readonly(app_state)) return()
      shiny::removeModal()

      geojson <- rv$drawn_geojson
      if (is.null(geojson)) return()

      tryCatch({
        projet <- rv$projet_ug
        projet <- tenement_split_by_drawn_line(projet, geojson)

        if (!is.null(projet$metadata$id)) {
          save_ug_data(projet$metadata$id, projet)
        }
        rv$projet_ug <- projet
        rv$redraw_counter <- shiny::isolate(rv$redraw_counter) + 1L
        rv$drawn_geojson <- NULL
        clear_tenement_selection()
        app_state$current_project$tenements <- projet$tenements
        app_state$current_project$ugs <- projet$ugs

        # Clear drawn shapes from the map (leaflet group + JS fallback).
        leaflet::leafletProxy(ns("ug_map")) |>
          leaflet::clearGroup("Dessin")
        session$sendCustomMessage("leafletClearDrawn", list(id = ns("ug_map")))

        shiny::showNotification(
          i18n()$t("ug_line_split_success"),
          type = "message"
        )
      }, error = function(e) {
        shiny::showNotification(
          paste(i18n()$t("ug_split_error"), translate_split_error(e$message)),
          type = "error",
          duration = 10
        )
      })
    })

    # ================================================================
    # OUTPUT: Map selection info
    # ================================================================
    output$map_selection_info <- shiny::renderUI({
      n_sel <- length(rv$selected_tenement_ids)
      if (n_sel == 0) {
        return(shiny::p(
          class = "text-muted small",
          i18n()$t("ug_map_click_hint")
        ))
      }

      projet <- rv$projet_ug
      if (is.null(projet)) return(NULL)

      # Find which UGs are involved
      sel_tenements <- projet$tenements[projet$tenements$tenement_id %in% rv$selected_tenement_ids, ]
      ug_ids_involved <- unique(sel_tenements$ug_id)
      ug_labels <- projet$ugs$label[projet$ugs$ug_id %in% ug_ids_involved]

      total_surface <- sum(sel_tenements$surface_m2, na.rm = TRUE)

      htmltools::tagList(
        shiny::tags$span(
          class = "badge bg-warning",
          sprintf("%d tenement(s)", n_sel)
        ),
        shiny::br(),
        shiny::tags$small(
          class = "text-muted",
          sprintf("%s ha | UG: %s",
                  format(round(total_surface / 10000, 2), nsmall = 2),
                  paste(ug_labels, collapse = ", "))
        ),
        shiny::br(),
        shiny::actionButton(
          ns("btn_clear_map_sel"),
          label = i18n()$t("ug_clear_selection"),
          icon = shiny::icon("xmark"),
          class = "btn-outline-secondary btn-sm mt-1",
          width = "100%"
        )
      )
    })

    # Clear map selection
    # Clear map selection
    shiny::observeEvent(input$btn_clear_map_sel, {
      clear_tenement_selection()
    })

    # ================================================================
    # ACTION: Create UG from map-selected tenements
    # ================================================================
    shiny::observeEvent(input$btn_create_from_map, {
      if (deny_if_readonly(app_state)) return()
      sel_ids <- rv$selected_tenement_ids
      if (length(sel_ids) == 0) {
        shiny::showNotification(
          i18n()$t("ug_map_select_tenements_first"),
          type = "warning"
        )
        return()
      }

      shiny::showModal(shiny::modalDialog(
        title = i18n()$t("ug_create_from_map"),
        shiny::textInput(
          ns("create_map_label"),
          label = i18n()$t("ug_label_prompt"),
          value = sprintf("UG-%03d", nrow(rv$projet_ug$ugs) + 1)
        ),
        shiny::selectInput(
          ns("create_map_groupe"),
          label = get_groupes_field_label(profile_key(), lang = lang()),
          choices = get_groupes_choices(profile_key(), lang = lang(),
                                        include_empty = TRUE),
          selected = ""
        ),
        shiny::p(
          class = "text-muted",
          sprintf(i18n()$t("ug_create_confirm"), length(sel_ids))
        ),
        footer = htmltools::tagList(
          shiny::modalButton(i18n()$t("cancel")),
          shiny::actionButton(
            ns("confirm_create_from_map"),
            i18n()$t("ug_create_btn"),
            class = "btn-success"
          )
        )
      ))
    })

    shiny::observeEvent(input$confirm_create_from_map, {
      if (deny_if_readonly(app_state)) return()
      shiny::removeModal()

      sel_ids <- rv$selected_tenement_ids
      if (length(sel_ids) == 0) return()

      label <- trimws(input$create_map_label)
      groupe <- input$create_map_groupe
      if (nchar(label) == 0) {
        shiny::showNotification(i18n()$t("ug_label_required"), type = "warning")
        return()
      }
      groupe_val <- if (nchar(groupe) == 0) NA_character_ else groupe

      tryCatch({
        projet <- rv$projet_ug
        projet <- ug_create(projet, sel_ids, label, groupe_val)

        if (!is.null(projet$metadata$id)) {
          save_ug_data(projet$metadata$id, projet)
        }
        rv$projet_ug <- projet
        rv$selected_tenement_ids <- character(0)
        app_state$current_project$tenements <- projet$tenements
        app_state$current_project$ugs <- projet$ugs

        shiny::showNotification(
          sprintf("UG \u00ab %s \u00bb cr\u00e9\u00e9e avec %d tenement(s)", label, length(sel_ids)),
          type = "message"
        )
      }, error = function(e) {
        shiny::showNotification(e$message, type = "error")
      })
    })

    # ================================================================
    # ACTION: Move selected tenements to an existing UG
    # ================================================================
    shiny::observeEvent(input$btn_move_to_ug, {
      if (deny_if_readonly(app_state)) return()
      sel_ids <- rv$selected_tenement_ids
      if (length(sel_ids) == 0) {
        shiny::showNotification(
          i18n()$t("ug_map_select_tenements_first"),
          type = "warning"
        )
        return()
      }

      projet <- rv$projet_ug
      if (is.null(projet) || !has_ug_data(projet)) return()

      # Build UG choices from the freshest projet_ug state.
      # Only include UGs that actually have at least one tenement.
      ugs <- projet$ugs
      tenements <- projet$tenements
      has_tenement <- vapply(ugs$ug_id, function(uid) {
        any(tenements$ug_id == uid, na.rm = TRUE)
      }, logical(1))
      ugs <- ugs[has_tenement, , drop = FALSE]

      if (nrow(ugs) == 0) {
        shiny::showNotification(
          i18n()$t("ug_no_data"),
          type = "warning"
        )
        return()
      }

      # Disambiguate duplicate labels by appending a short suffix of the ID
      label_counts <- table(ugs$label)
      display_labels <- ifelse(
        label_counts[ugs$label] > 1L,
        paste0(ugs$label, " \u00b7 ", substr(ugs$ug_id, 1, 6)),
        ugs$label
      )
      ug_choices <- stats::setNames(ugs$ug_id, display_labels)

      shiny::showModal(shiny::modalDialog(
        title = i18n()$t("ug_move_to"),
        shiny::p(sprintf(i18n()$t("ug_move_desc"), length(sel_ids))),
        shiny::selectInput(
          ns("move_target_ug"),
          label = i18n()$t("ug_move_target"),
          choices = ug_choices
        ),
        footer = htmltools::tagList(
          shiny::modalButton(i18n()$t("cancel")),
          shiny::actionButton(
            ns("confirm_move_to_ug"),
            i18n()$t("ug_move_confirm"),
            class = "btn-success",
            icon = shiny::icon("arrow-right-arrow-left")
          )
        )
      ))
    })

    shiny::observeEvent(input$confirm_move_to_ug, {
      if (deny_if_readonly(app_state)) return()
      shiny::removeModal()

      sel_ids <- rv$selected_tenement_ids
      target_ug_id <- input$move_target_ug
      if (length(sel_ids) == 0 || is.null(target_ug_id)) return()

      tryCatch({
        projet <- rv$projet_ug

        # Move each tenement to the target UG
        for (tenement_id in sel_ids) {
          projet <- ug_assign_tenement(projet, tenement_id, target_ug_id)
        }

        if (!is.null(projet$metadata$id)) {
          save_ug_data(projet$metadata$id, projet)
        }
        rv$projet_ug <- projet
        clear_tenement_selection()
        app_state$current_project$tenements <- projet$tenements
        app_state$current_project$ugs <- projet$ugs

        target_label <- projet$ugs$label[projet$ugs$ug_id == target_ug_id]
        shiny::showNotification(
          sprintf(i18n()$t("ug_move_success"), length(sel_ids), target_label),
          type = "message"
        )
      }, error = function(e) {
        shiny::showNotification(e$message, type = "error")
      })
    })

    # ================================================================
    # OUTPUT: UG table
    # ================================================================
    output$ug_table <- DT::renderDataTable({
      listing <- ug_listing()
      if (is.null(listing) || nrow(listing) == 0) {
        return(DT::datatable(
          data.frame(Message = i18n()$t("ug_no_data")),
          options = list(dom = "t"),
          rownames = FALSE
        ))
      }

      # Build display table (show both cadastral and SIG surfaces)
      sig_col <- if (!is.null(listing$surface_sig_m2)) {
        round(listing$surface_sig_m2 / 10000, 2)
      } else {
        round(listing$surface_m2 / 10000, 2)
      }
      # Column header for the groupe column matches the profile's
      # field_label (e.g. "Groupe d'amenagement" / "Groupe" / "Zone").
      groupe_col <- get_groupes_field_label(profile_key(), lang = lang())

      # Build a pale background tint from each profile color (85% white
      # mixed with 15% of the map color). Used by DT::formatStyle below.
      lighten <- function(hex, mix = 0.85) {
        tryCatch({
          rgb <- grDevices::col2rgb(hex)[, 1]
          r <- round(rgb["red"]   * (1 - mix) + 255 * mix)
          g <- round(rgb["green"] * (1 - mix) + 255 * mix)
          b <- round(rgb["blue"]  * (1 - mix) + 255 * mix)
          sprintf("#%02X%02X%02X", r, g, b)
        }, error = function(e) "#ffffff")
      }
      profile_colors_full <- get_groupes_colors(profile_key())
      groupe_bg_colors <- vapply(profile_colors_full, lighten, character(1))
      display_df <- data.frame(
        `Label UGF` = listing$label,
        `__groupe__` = ifelse(is.na(listing$groupe), "---", listing$groupe),
        Tenements = listing$n_tenements,
        `Surface cadastrale (ha)` = round(listing$surface_m2 / 10000, 2),
        `Surface SIG (ha)` = sig_col,
        check.names = FALSE,
        stringsAsFactors = FALSE
      )
      names(display_df)[names(display_df) == "__groupe__"] <- groupe_col

      # Get cadastral refs for each UG
      projet <- rv$projet_ug
      refs <- vapply(listing$ug_id, function(uid) {
        r <- ug_cadastral_refs(projet, uid)
        if (nrow(r) == 0) return("")
        paste(r$geo_parcelle, collapse = ", ")
      }, character(1))
      display_df[["Refs cadastrales"]] <- refs

      DT::datatable(
        display_df,
        selection = "multiple",
        rownames = FALSE,
        options = list(
          pageLength = 25,
          dom = "frtip",
          language = if (lang() == "fr") {
            list(
              search = "Rechercher :",
              info = "_TOTAL_ UG",
              lengthMenu = "Afficher _MENU_ UG"
            )
          } else {
            list()
          }
        )
      ) |>
        DT::formatStyle(
          groupe_col,
          backgroundColor = DT::styleEqual(
            names(groupe_bg_colors),
            unname(groupe_bg_colors)
          )
        )
    })

    # Render the table even when the sub-tab is hidden. Otherwise Shiny
    # suspends the output and the post-split data update is only picked
    # up the next time the user navigates to "Tableau" -- which sometimes
    # doesn't trigger a refresh because the cached value looks the same.
    shiny::outputOptions(output, "ug_table", suspendWhenHidden = FALSE)
    shiny::outputOptions(output, "ug_map_count", suspendWhenHidden = FALSE)
    shiny::outputOptions(output, "ug_map_surface", suspendWhenHidden = FALSE)

    # ================================================================
    # OUTPUT: Summary text
    # ================================================================
    output$ug_summary <- shiny::renderText({
      listing <- ug_listing()
      if (is.null(listing)) return("")
      total_ha <- round(sum(listing$surface_m2, na.rm = TRUE) / 10000, 2)
      sprintf("%d UG | %s ha", nrow(listing), total_ha)
    })

    # ================================================================
    # OUTPUT: UG detail panel
    # ================================================================
    output$ug_detail <- shiny::renderUI({
      sel <- input$ug_table_rows_selected
      listing <- ug_listing()
      projet <- rv$projet_ug

      if (is.null(sel) || length(sel) == 0 || is.null(listing)) {
        return(shiny::p(
          class = "text-muted",
          i18n()$t("ug_select_hint")
        ))
      }

      if (length(sel) == 1) {
        uid <- listing$ug_id[sel]
        refs <- ug_cadastral_refs(projet, uid)
        surface_ha <- round(ug_surface(projet, uid) / 10000, 2)
        color <- ug_color(listing$groupe[sel], sel, profile_key = profile_key())

        htmltools::tagList(
          htmltools::div(
            style = sprintf("border-left: 4px solid %s; padding-left: 8px;", color),
            shiny::tags$strong(listing$label[sel]),
            shiny::br(),
            shiny::tags$span(
              class = "text-muted",
              sprintf("%s ha | %d tenement(s)", surface_ha, listing$n_tenements[sel])
            ),
            if (!is.na(listing$groupe[sel])) {
              shiny::tags$span(
                class = "badge ms-1",
                style = sprintf("background-color: %s;", color),
                listing$groupe[sel]
              )
            }
          ),
          shiny::hr(),
          if (nrow(refs) > 0) {
            shiny::tags$ul(
              class = "list-unstyled small",
              lapply(seq_len(nrow(refs)), function(i) {
                shiny::tags$li(
                  shiny::icon("map-pin", class = "text-muted me-1"),
                  sprintf(
                    "%s (sect. %s, n\u00b0%s) \u2014 %s m\u00b2",
                    refs$geo_parcelle[i],
                    refs$section[i],
                    refs$numero[i],
                    format(refs$surface_m2[i], big.mark = " ")
                  )
                )
              })
            )
          } else {
            shiny::p(class = "text-muted", "Aucune r\u00e9f\u00e9rence")
          }
        )
      } else {
        htmltools::tagList(
          shiny::p(sprintf("%d UG s\u00e9lectionn\u00e9es", length(sel))),
          shiny::p(
            class = "text-muted",
            sprintf(
              "Surface totale : %s ha",
              round(sum(listing$surface_m2[sel], na.rm = TRUE) / 10000, 2)
            )
          )
        )
      }
    })

    # ================================================================
    # ACTION: Merge UGs (from table selection)
    # ================================================================
    shiny::observeEvent(input$btn_merge, {
      if (deny_if_readonly(app_state)) return()
      sel <- input$ug_table_rows_selected
      listing <- ug_listing()

      if (is.null(sel) || length(sel) < 2 || is.null(listing)) {
        shiny::showNotification(
          i18n()$t("ug_select_at_least_2"),
          type = "warning"
        )
        return()
      }

      ug_ids_to_merge <- listing$ug_id[sel]

      shiny::showModal(shiny::modalDialog(
        title = i18n()$t("ug_merge"),
        shiny::textInput(
          ns("merge_label"),
          label = i18n()$t("ug_label_prompt"),
          value = paste(listing$label[sel[1]], "+", length(sel) - 1)
        ),
        shiny::p(
          class = "text-muted",
          sprintf(i18n()$t("ug_confirm_merge"), length(sel))
        ),
        footer = htmltools::tagList(
          shiny::modalButton(i18n()$t("cancel")),
          shiny::actionButton(
            ns("confirm_merge"),
            i18n()$t("ug_merge"),
            class = "btn-success"
          )
        )
      ))
    })

    shiny::observeEvent(input$confirm_merge, {
      if (deny_if_readonly(app_state)) return()
      shiny::removeModal()

      sel <- input$ug_table_rows_selected
      listing <- ug_listing()
      if (is.null(sel) || length(sel) < 2 || is.null(listing)) return()

      ug_ids <- listing$ug_id[sel]
      label <- input$merge_label

      tryCatch({
        projet <- rv$projet_ug
        projet <- ug_merge(projet, ug_ids, label)

        if (!is.null(projet$metadata$id)) {
          save_ug_data(projet$metadata$id, projet)
        }
        rv$projet_ug <- projet
        app_state$current_project$tenements <- projet$tenements
        app_state$current_project$ugs <- projet$ugs

        shiny::showNotification(
          sprintf("%s : %d UG \u2192 1", i18n()$t("ug_merge"), length(ug_ids)),
          type = "message"
        )
      }, error = function(e) {
        shiny::showNotification(e$message, type = "error")
      })
    })

    # ================================================================
    # ACTION: Split UG (restore 1 UG per tenement)
    # ================================================================
    shiny::observeEvent(input$btn_split, {
      if (deny_if_readonly(app_state)) return()
      sel <- input$ug_table_rows_selected
      listing <- ug_listing()

      if (is.null(sel) || length(sel) != 1 || is.null(listing)) {
        shiny::showNotification(i18n()$t("ug_select_one_to_split"), type = "warning")
        return()
      }

      uid <- listing$ug_id[sel]
      n_tenements <- listing$n_tenements[sel]

      if (n_tenements < 2) {
        shiny::showNotification(i18n()$t("ug_cannot_split_single"), type = "warning")
        return()
      }

      tryCatch({
        projet <- rv$projet_ug
        projet <- ug_split(projet, uid)

        if (!is.null(projet$metadata$id)) {
          save_ug_data(projet$metadata$id, projet)
        }
        rv$projet_ug <- projet
        app_state$current_project$tenements <- projet$tenements
        app_state$current_project$ugs <- projet$ugs

        shiny::showNotification(
          sprintf("UG dissoci\u00e9e en %d UG", n_tenements),
          type = "message"
        )
      }, error = function(e) {
        shiny::showNotification(e$message, type = "error")
      })
    })

    # ================================================================
    # ACTION: Rename UG
    # ================================================================
    shiny::observeEvent(input$btn_rename, {
      if (deny_if_readonly(app_state)) return()
      sel <- input$ug_table_rows_selected
      listing <- ug_listing()

      if (is.null(sel) || length(sel) != 1 || is.null(listing)) {
        shiny::showNotification(i18n()$t("ug_select_one"), type = "warning")
        return()
      }

      shiny::showModal(shiny::modalDialog(
        title = i18n()$t("ug_rename"),
        shiny::textInput(
          ns("rename_label"),
          label = i18n()$t("ug_label_prompt"),
          value = listing$label[sel]
        ),
        footer = htmltools::tagList(
          shiny::modalButton(i18n()$t("cancel")),
          shiny::actionButton(
            ns("confirm_rename"),
            i18n()$t("ug_rename"),
            class = "btn-primary"
          )
        )
      ))
    })

    shiny::observeEvent(input$confirm_rename, {
      if (deny_if_readonly(app_state)) return()
      shiny::removeModal()

      sel <- input$ug_table_rows_selected
      listing <- ug_listing()
      if (is.null(sel) || length(sel) != 1 || is.null(listing)) return()

      uid <- listing$ug_id[sel]
      new_label <- trimws(input$rename_label)

      if (nchar(new_label) == 0) {
        shiny::showNotification(i18n()$t("ug_label_required"), type = "warning")
        return()
      }

      projet <- rv$projet_ug
      projet$ugs$label[projet$ugs$ug_id == uid] <- new_label

      if (!is.null(projet$metadata$id)) {
        save_ug_data(projet$metadata$id, projet)
      }
      rv$projet_ug <- projet
      app_state$current_project$ugs <- projet$ugs

      shiny::showNotification(
        sprintf("UG renomm\u00e9e : %s", new_label),
        type = "message"
      )
    })

    # ================================================================
    # ACTION: Set groupe d'amenagement
    # ================================================================
    shiny::observeEvent(input$btn_set_groupe, {
      if (deny_if_readonly(app_state)) return()
      sel <- input$ug_table_rows_selected
      listing <- ug_listing()
      groupe <- input$sel_groupe

      if (is.null(sel) || length(sel) == 0 || is.null(listing)) {
        shiny::showNotification(i18n()$t("ug_select_one"), type = "warning")
        return()
      }

      projet <- rv$projet_ug
      for (idx in sel) {
        uid <- listing$ug_id[idx]
        groupe_val <- if (nchar(groupe) == 0) NA_character_ else groupe
        projet <- ug_set_groupe(projet, uid, groupe_val)
      }

      if (!is.null(projet$metadata$id)) {
        save_ug_data(projet$metadata$id, projet)
      }
      rv$projet_ug <- projet
      app_state$current_project$ugs <- projet$ugs

      shiny::showNotification(
        sprintf("Groupe mis \u00e0 jour pour %d UG", length(sel)),
        type = "message"
      )
    })

    # ================================================================
    # ACTION: Export current tenement split as GPKG
    # ================================================================
    # Users can edit the file externally and re-import via btn_import_split.
    # The exported GPKG carries all relevant columns as a template.
    output$btn_export_split <- shiny::downloadHandler(
      filename = function() {
        meta <- rv$projet_ug$metadata
        # Sanitize project name for use as filename prefix
        pname <- meta$name %||% meta$id %||% "projet"
        pname <- gsub("[^A-Za-z0-9_\\-]", "_", pname)
        pname <- gsub("_+", "_", pname)
        pname <- gsub("^_|_$", "", pname)
        if (nchar(pname) == 0) pname <- "projet"
        sprintf("%s_tenements_%s.gpkg", pname, format(Sys.time(), "%Y%m%d_%H%M%S"))
      },
      content = function(file) {
        projet <- rv$projet_ug
        if (is.null(projet) || !has_ug_data(projet)) {
          shiny::showNotification(i18n()$t("ug_no_data"), type = "warning")
          return()
        }

        tryCatch({
          tenements <- projet$tenements

          # Enrich with UG label and groupe (joined from projet$ugs)
          ugs <- projet$ugs
          if (!is.null(ugs) && "ug_id" %in% names(ugs)) {
            idx <- match(tenements$ug_id, ugs$ug_id)
            tenements$ug_label <- ugs$label[idx]
            tenements$ug_groupe <- ugs$groupe[idx]
          }

          # Ensure SIG surface exists
          if (!"surface_sig_m2" %in% names(tenements)) {
            tenements$surface_sig_m2 <- as.numeric(sf::st_area(tenements))
          }

          sf::st_write(tenements, file, driver = "GPKG", quiet = TRUE,
                       delete_dsn = TRUE)
        }, error = function(e) {
          shiny::showNotification(
            paste(i18n()$t("ug_split_error"), translate_split_error(e$message)),
            type = "error",
            duration = 10
          )
        })
      }
    )

    # ================================================================
    # ACTION: Import split (GeoJSON/Shapefile)
    # ================================================================
    shiny::observeEvent(input$btn_import_split, {
      if (deny_if_readonly(app_state)) return()
      projet <- rv$projet_ug
      if (is.null(projet) || !has_ug_data(projet)) {
        shiny::showNotification(i18n()$t("ug_no_data"), type = "warning")
        return()
      }

      shiny::showModal(shiny::modalDialog(
        title = i18n()$t("ug_import_split"),
        size = "l",
        shiny::fileInput(
          ns("split_file"),
          label = i18n()$t("ug_split_file"),
          accept = c(".geojson", ".json", ".shp", ".gpkg"),
          placeholder = "GeoJSON / Shapefile / GeoPackage"
        ),
        shiny::p(
          class = "text-muted small",
          htmltools::HTML(i18n()$t("ug_split_hint"))
        ),
        footer = htmltools::tagList(
          shiny::modalButton(i18n()$t("cancel")),
          shiny::actionButton(
            ns("confirm_import_split"),
            i18n()$t("ug_split_apply"),
            class = "btn-info",
            icon = shiny::icon("scissors")
          )
        )
      ))
    })

    shiny::observeEvent(input$confirm_import_split, {
      if (deny_if_readonly(app_state)) return()
      file_info <- input$split_file

      if (is.null(file_info)) {
        shiny::showNotification(i18n()$t("ug_split_no_file"), type = "warning")
        return()
      }

      shiny::removeModal()

      # Persistent spinner notification while the import runs. The
      # spatial joins + area recomputation can take a few seconds on
      # large projects, so give the user immediate visual feedback.
      # Same visual pattern as the project-loading notification in
      # mod_home.R (spinner icon, persistent, no close button).
      notif_id <- "ug_import_loading"
      shiny::showNotification(
        htmltools::tagList(
          shiny::icon("spinner", class = "fa-spin me-2"),
          sprintf("%s...", i18n()$t("ug_import_running"))
        ),
        type = "message",
        duration = NULL,
        closeButton = FALSE,
        id = notif_id,
        session = session
      )

      # Schedule the actual work via later::later so the spinner gets
      # painted before we block on the heavy computation. Without this
      # step, Shiny batches UI updates at the end of the reactive
      # context and the user would never see the spinner.
      #
      # The later callback runs OUTSIDE any reactive context, so ALL
      # reactive reads must be isolated. We snapshot the things we need
      # now, then pass them in by closure -- simpler and faster than
      # calling shiny::isolate() on every single line.
      datapath       <- file_info$datapath
      i18n_snap      <- shiny::isolate(i18n())
      lang_snap      <- shiny::isolate(lang())
      translate_err  <- function(msg) {
        if (is.null(msg) || length(msg) == 0) return("")
        is_fr <- identical(lang_snap, "fr")
        patterns <- list(
          list(re = "does not intersect any tenement",
               fr = "Le polygone ne recouvre aucun t\u00e8nement.",
               en = "The drawn polygon does not intersect any tenement."),
          list(re = "Project must have UG data",
               fr = "Le projet ne contient pas de donn\u00e9es UGF.",
               en = "Project must have UG data."),
          list(re = "Invalid GeoJSON",
               fr = "Fichier GeoJSON invalide.",
               en = "Invalid GeoJSON.")
        )
        for (p in patterns) {
          if (grepl(p$re, msg, ignore.case = TRUE)) {
            return(if (is_fr) p$fr else p$en)
          }
        }
        msg
      }

      later::later(function() {
        tryCatch({
          # Read the imported file
          sf_polygones <- sf::st_read(datapath, quiet = TRUE)

          if (nrow(sf_polygones) == 0) {
            shiny::removeNotification(notif_id, session = session)
            shiny::showNotification(i18n_snap$t("ug_split_empty_file"), type = "error")
            return()
          }

          # Apply as a full layout replacement. The imported file is the
          # NEW tenement configuration -- not a cutter. Handles:
          #  - QGIS "Separer les parties" (multipart -> singleparts)
          #  - QGIS "Separer l'entite"   (new rows appear in the file)
          #  - reshape / merge / delete in any GIS tool
          projet <- shiny::isolate(rv$projet_ug)
          projet <- tenement_import_replace(projet, sf_polygones)

          if (!is.null(projet$metadata$id)) {
            save_ug_data(projet$metadata$id, projet)
            # The new layout may introduce brand-new ug_ids (either from
            # label_ugf or from the tenement-id regeneration). Any cached
            # indicators.parquet still references the OLD ug_ids, which
            # would cause compute_all_indicators() to skip everything
            # ("already computed") and leave the fresh UGFs unpopulated.
            # Drop the cache so the next "Lancer les calculs" starts from
            # scratch on the new layout.
            invalidate_indicators(projet$metadata$id)
          }

          # Writes to reactiveValues do not require a reactive context.
          # Deep assignments on app_state require an isolated read first
          # because `x$a$b <- y` reads x$a under the hood.
          rv$projet_ug <- projet
          rv$redraw_counter <- shiny::isolate(rv$redraw_counter) + 1L

          # clear selection -- its leafletProxy() call needs a reactive
          # domain, so we pass session explicitly.
          leaflet::leafletProxy("ug_map", session = session) |>
            leaflet::clearGroup("Selection")
          rv$selected_tenement_ids <- character(0)

          cur_proj <- shiny::isolate(app_state$current_project)
          if (!is.null(cur_proj)) {
            cur_proj$tenements <- projet$tenements
            cur_proj$ugs       <- projet$ugs
            # Mirror invalidate_indicators() in memory: the cached
            # indicator columns would otherwise render on top of the
            # new UGF layout with mismatched ug_ids, and also prevent
            # the "Lancer les calculs" button from showing up again.
            cur_proj$indicators <- NULL
            cur_proj$indicators_sf <- NULL
            if (!is.null(cur_proj$metadata)) {
              cur_proj$metadata$indicators_computed <- FALSE
              cur_proj$metadata$status <- "draft"
            }
            app_state$current_project <- cur_proj
          }

          shiny::removeNotification(notif_id, session = session)
          shiny::showNotification(
            i18n_snap$t("ug_poly_split_success"),
            type = "message",
            session = session
          )
        }, error = function(e) {
          shiny::removeNotification(notif_id, session = session)
          shiny::showNotification(
            paste(i18n_snap$t("ug_split_error"), translate_err(e$message)),
            type = "error",
            duration = 10,
            session = session
          )
        })
      }, delay = 0.05)
    })

    # ================================================================
    # ACTION: parcellaire forestier ONF (spec 046)
    # ================================================================
    #
    # Les deux actions partagent le meme squelette : appel reseau au coeur
    # (~6 s mesurees sur la foret domaniale de Chaux), donc spinner immediat
    # + `later::later()` pour que Shiny le peigne AVANT de bloquer, exactement
    # comme l'import de decoupage ci-dessus. Le callback `later` tourne HORS
    # contexte reactif : tout ce qui vient du reactif est capture avant.
    #
    # Toute la logique metier vit dans `R/service_onf.R` (regle #2).

    # Ecrit le projet issu d'une action ONF : disque, cache d'indicateurs,
    # etat du module, carte, et miroir dans `app_state$current_project`.
    # `with_parcels = TRUE` quand les PARCELLES elles-memes ont change
    # (import ONF), pas seulement le decoupage.
    .onf_commit <- function(projet, with_parcels = FALSE) {
      if (!is.null(projet$metadata$id)) {
        # `save_ug_data()` n'ecrit QUE tenements.gpkg et ugs.json. Quand les
        # PARCELLES elles-memes ont change (purge des parcelles hors foret),
        # il faut aussi reecrire parcels.gpkg - sinon elles reapparaitraient au
        # prochain chargement du projet, sans leurs tenements.
        if (isTRUE(with_parcels) && !is.null(projet$parcels)) {
          save_parcels(projet$metadata$id, projet$parcels)
        }
        save_ug_data(projet$metadata$id, projet)
        # Les ug_id sont neufs : un indicators.parquet cache pointerait sur les
        # ANCIENS, et compute_all_indicators() sauterait tout en croyant avoir
        # deja calcule. Meme raisonnement que l'import de decoupage.
        invalidate_indicators(projet$metadata$id)
      }
      rv$projet_ug <- projet
      rv$redraw_counter <- shiny::isolate(rv$redraw_counter) + 1L

      leaflet::leafletProxy("ug_map", session = session) |>
        leaflet::clearGroup("Selection")
      rv$selected_tenement_ids <- character(0)

      cur_proj <- shiny::isolate(app_state$current_project)
      if (!is.null(cur_proj)) {
        if (isTRUE(with_parcels)) {
          cur_proj$parcels <- projet$parcels
          # Signale a l'onglet Selection que les parcelles ont change
          # (spec 001-app). Sans lui, la Selection continuerait d'afficher -
          # et de compter comme selectionnees - des parcelles retirees du
          # projet : elle ne lit pas `current_project`, elle tient son propre
          # etat, alimente par un signal.
          app_state$parcels_changed <- list(
            parcels   = projet$parcels,
            timestamp = Sys.time()
          )
        }
        cur_proj$tenements <- projet$tenements
        cur_proj$ugs       <- projet$ugs
        cur_proj$indicators <- NULL
        cur_proj$indicators_sf <- NULL
        if (!is.null(cur_proj$metadata)) {
          cur_proj$metadata$indicators_computed <- FALSE
          cur_proj$metadata$status <- "draft"
        }
        app_state$current_project <- cur_proj
      }
      invisible(TRUE)
    }

    # Spinner persistant pendant l'appel WFS + le croisement.
    .onf_notif_id <- "ug_onf_loading"
    .onf_spinner_on <- function(i18n_snap) {
      shiny::showNotification(
        htmltools::tagList(
          shiny::icon("spinner", class = "fa-spin me-2"),
          sprintf("%s...", i18n_snap$t("onf_running"))
        ),
        type = "message", duration = NULL, closeButton = FALSE,
        id = .onf_notif_id, session = session
      )
    }

    # Traduit les deux issues " non-erreur " du service en message adapte :
    # service muet (NULL) et emprise sans foret publique (0 ligne) ne sont pas
    # le meme evenement, et aucun des deux n'est une erreur.
    .onf_notify_status <- function(status, i18n_snap) {
      key <- switch(status,
        unavailable     = "onf_unavailable",
        empty           = "onf_no_public_forest",
        no_aoi          = "onf_need_aoi",
        no_overlap      = "onf_no_overlap",
        no_domanialite  = "onf_need_domanialite",
        NULL)
      if (is.null(key)) return(FALSE)
      shiny::showNotification(
        i18n_snap$t(key),
        type = if (identical(status, "unavailable")) "error" else "warning",
        session = session
      )
      TRUE
    }

    # Rend compte de la purge - LES DEUX chemins qui croisent le parcellaire
    # (bouton ONF et import CSV) passent par ici, sinon ils divergent : le
    # bouton expliquait ce qui restait, le CSV se taisait.
    #
    # Deux messages, parce qu'ils repondent a deux questions. Le premier dit ce
    # qui a ete RETIRE ; le second dit pourquoi une ligne " Hors foret
    # publique " SUBSISTE malgre la demande. Les fondre laisserait
    # l'utilisateur devant une purge apparemment en panne.
    #
    # Le seuil est passe au message : il est parametrable depuis
    # Parametres > Sources & parametres et vaut 0 par defaut, alors que le
    # texte annoncait " 10 % " en dur - la valeur d'un defaut qui a change.
    .onf_notify_purge <- function(n_purgees, seuil_foret, i18n_snap) {
      pct <- format(round(100 * (seuil_foret %||% 0), 1), trim = TRUE)
      shiny::showNotification(
        if (n_purgees > 0L) {
          sprintf(i18n_snap$t("onf_purge_hors_fmt"), n_purgees, pct)
        } else sprintf(i18n_snap$t("onf_purge_hors_aucune_fmt"), pct),
        type = if (n_purgees > 0L) "warning" else "message",
        duration = 10, session = session)
    }

    # Surcouche " Parcellaire ONF " : montre CE QUI VA ETRE IMPORTE avant de
    # toucher au projet. Sans elle, l'utilisateur valide un remplacement de ses
    # parcelles sans avoir vu ce qui les remplace.
    #
    # Rendu volontairement leger : sur une emprise large la couche peut compter
    # plusieurs centaines de polygones, donc pas de label permanent - le
    # `nom_ugf` et la surface vivent dans un popup, au clic.
    shiny::observeEvent(rv$onf_preview, ignoreNULL = FALSE, {
      proxy <- leaflet::leafletProxy("ug_map", session = session)
      proxy |> leaflet::clearGroup("Parcellaire ONF")

      pv <- rv$onf_preview
      if (is.null(pv) || !inherits(pv, "sf") || nrow(pv) == 0L) return()

      pv <- tryCatch(sf::st_transform(pv, 4326), error = function(e) NULL)
      if (is.null(pv)) return()

      # La couleur encode la DOMANIALITE : domaniale et communale ne relevent
      # pas du meme gestionnaire, et c'est la premiere lecture qu'on en fait.
      dom <- .isTRUE_vec(pv$domaniale)
      couleur <- ifelse(dom, "#1B6B1B", "#B8860B")
      surface <- suppressWarnings(as.numeric(pv$surface_ha))
      popup <- sprintf(
        "<strong>%s</strong><br/>%.2f ha",
        htmltools::htmlEscape(as.character(pv$nom_ugf)),
        ifelse(is.finite(surface), surface, NA_real_)
      )

      proxy |>
        leaflet::addPolygons(
          data = pv,
          group = "Parcellaire ONF",
          color = couleur, weight = 2, opacity = 0.9,
          fillColor = couleur, fillOpacity = 0.15,
          popup = popup,
          options = leaflet::pathOptions(pane = "overlayPane")
        ) |>
        leaflet::showGroup("Parcellaire ONF")
    })

    # ---- Croiser : GARDE les parcelles du projet -----------------------
    shiny::observeEvent(input$btn_onf_croise, {
      if (deny_if_readonly(app_state)) return()
      projet <- rv$projet_ug
      if (is.null(projet) || !has_ug_data(projet)) {
        shiny::showNotification(i18n()$t("ug_no_data"), type = "warning")
        return()
      }
      if (is.null(projet$parcels) || nrow(projet$parcels) == 0L) {
        shiny::showNotification(i18n()$t("onf_need_selection"), type = "warning")
        return()
      }

      i18n_snap <- shiny::isolate(i18n())
      # Vecteur des coches. `onf_load_parcelles()` le traduit en argument coeur
      # et rend le statut " no_domanialite " si aucune n'est cochee.
      # Les reglages viennent des parametres du projet, plus de la barre : ils
      # y sont persistes, donc ils survivent au rechargement et sont les memes
      # pour tous ceux qui ouvrent le projet.
      cfg       <- project_onf_params(shiny::isolate(app_state$current_project)$metadata)
      dom       <- cfg$domanialite
      purger    <- isTRUE(cfg$purger)
      .onf_spinner_on(i18n_snap)

      later::later(function() {
        tryCatch({
          # UN SEUL appel WFS, sur l'emprise de toute la selection (le brief
          # interdit explicitement un appel par parcelle).
          res <- onf_load_parcelles(projet$parcels, domanialite = dom,
                                    clip_cadastre = cfg$clip_cadastre)
          if (!identical(res$status, "ok")) {
            shiny::removeNotification(.onf_notif_id, session = session)
            .onf_notify_status(res$status, i18n_snap)
            return()
          }

          # Surcouche montrant le parcellaire interroge, AVANT que le
          # croisement n'ait produit les UGF.
          rv$onf_preview <- res$parcelles

          # Calage systematique (cf. UI ci-dessus) : plus de choix a lire.
          out <- onf_projet_croise(projet, res$parcelles, i18n = i18n_snap)
          if (!identical(out$status, "ok")) {
            shiny::removeNotification(.onf_notif_id, session = session)
            .onf_notify_status(out$status, i18n_snap)
            return()
          }

          # Purge optionnelle, APRES le croisement. Elle lit la part forestiere
          # RELEVEE PAR le croisement (`out$part_foret`) et non plus l'UGF
          # « Hors foret publique » : celle-ci n'existe plus, chaque bout ayant
          # rejoint son voisin. Ce chemin-ci est le SEUL qui la propose - une
          # selection faite a la main peut deborder, un CSV ne le peut pas.
          projet_final <- out$projet
          n_purgees <- 0L
          if (purger) {
            purge <- onf_purger_hors_foret(projet_final, out$part_foret,
                                           seuil_foret = cfg$seuil_foret)
            projet_final <- purge$projet
            n_purgees <- purge$n_supprimees
          }

          # `with_parcels` : la purge retire des parcelles du projet, il faut
          # donc les persister ET les refleter dans app_state, sinon l'onglet
          # Selection continuerait d'afficher des parcelles disparues.
          .onf_commit(projet_final, with_parcels = purger)

          # La surcouche a joue son role : les UGF qui viennent d'etre creees
          # SONT ce parcellaire. La laisser superposait un calque orange
          # permanent au resultat - d'autant plus trompeur apres une purge,
          # puisqu'elle continue de montrer un parcellaire que le projet ne
          # contient plus.
          rv$onf_preview <- NULL

          shiny::removeNotification(.onf_notif_id, session = session)

          # Tout est lu dans le retour du coeur, rien n'est recalcule.
          r <- onf_croise_resume(out$tenements)
          shiny::showNotification(
            sprintf(i18n_snap$t("onf_croise_success_fmt"), r$n_ugf, r$n_parcelles),
            type = "message", duration = 10, session = session
          )
          # Ce que l'auto-selection a retenu : sans ce chiffre, l'utilisateur ne
          # sait pas sur quelle part de son cadastre le calcul a porte.
          if (!is.null(out$n_total) && out$n_total > 0L) {
            shiny::showNotification(
              sprintf(i18n_snap$t("onf_auto_select_fmt"),
                      out$n_retenues, out$n_total),
              type = "message", duration = 10, session = session)
          }
          if (r$n_multi > 0L) {
            shiny::showNotification(
              sprintf(i18n_snap$t("onf_croise_multi_fmt"), r$n_multi),
              type = "message", duration = 10, session = session)
          }
          if (length(r$partielles) > 0L) {
            shiny::showNotification(
              sprintf(i18n_snap$t("onf_croise_partielle_fmt"), length(r$partielles)),
              type = "warning", duration = 10, session = session)
          }
          if (r$surface_hors_ha > 0 && !purger) {
            shiny::showNotification(
              sprintf(i18n_snap$t("onf_croise_hors_fmt"), r$surface_hors_ha),
              type = "message", duration = 10, session = session)
          }
          if (purger) {
            .onf_notify_purge(n_purgees, cfg$seuil_foret, i18n_snap)
          }
        }, error = function(e) {
          shiny::removeNotification(.onf_notif_id, session = session)
          shiny::showNotification(
            paste(i18n_snap$t("ug_split_error"), conditionMessage(e)),
            type = "error", duration = 10, session = session)
        })
      }, delay = 0.05)
    })

    # ================================================================
    # ACTION: import d'une liste de parcelles cadastrales (CSV)
    # ================================================================
    #
    # Cree un PROJET ENTIER a partir d'un fichier : lecture du CSV, resolution
    # des references contre le cadastre de la commune, creation du projet,
    # croisement ONF optionnel, puis rafraichissement de TOUS les sous-onglets
    # de Selection.
    #
    # Le rafraichissement passe par `app_state$restore_project` - et cette fois
    # c'est le bon signal, contrairement au cas de la purge (spec 001-app) :
    # on charge REELLEMENT un nouveau projet, donc `mod_search` doit bien
    # recuperer la geometrie de la commune. C'est exactement ce que ce signal
    # existe pour faire.

    shiny::observeEvent(input$btn_import_csv, {
      if (deny_if_readonly(app_state)) return()

      # L'import REMPLACE le projet courant : l'ancien est supprime, toutes
      # composantes comprises. Un geste destructif se dit AVANT, dans la
      # modale, et son bouton passe au rouge - la regle des couleurs reserve
      # `btn-danger` a ce qui detruit des donnees. Sans projet ouvert il n'y a
      # rien a detruire : ni avertissement, ni rouge, sinon l'alerte crie au
      # loup des le premier import et cesse d'etre lue.
      remplace <- !is.null(shiny::isolate(app_state$current_project))
      i18n_m <- i18n()

      shiny::showModal(shiny::modalDialog(
        title = i18n_m$t("csv_import_title"),
        size = "l",
        shiny::fileInput(ns("csv_file"), i18n_m$t("csv_import_file"),
                         accept = c(".csv", "text/csv"),
                         placeholder = "commune-code_insee.csv"),
        shiny::p(class = "text-muted small",
                 htmltools::HTML(i18n_m$t("csv_import_help"))),
        shiny::checkboxInput(ns("csv_cross_onf"),
                             i18n_m$t("csv_import_cross"), value = TRUE),
        if (remplace) {
          htmltools::div(
            class = "alert alert-danger py-2 mb-0 small",
            bsicons::bs_icon("exclamation-triangle-fill", class = "me-2"),
            htmltools::HTML(sprintf(
              i18n_m$t("csv_import_replace_warn"),
              shiny::isolate(app_state$current_project$metadata$name) %||%
                shiny::isolate(app_state$current_project$id)))
          )
        },
        footer = htmltools::tagList(
          shiny::modalButton(i18n_m$t("cancel")),
          shiny::actionButton(ns("confirm_import_csv"),
                              i18n_m$t("csv_import_apply"),
                              class = if (remplace) "btn-danger" else "btn-primary",
                              icon = bsicons::bs_icon("filetype-csv"))
        )
      ))
    })

    shiny::observeEvent(input$confirm_import_csv, {
      if (deny_if_readonly(app_state)) return()
      fi <- input$csv_file
      if (is.null(fi)) {
        shiny::showNotification(i18n()$t("csv_import_no_file"), type = "warning")
        return()
      }
      shiny::removeModal()

      # `fileInput` renomme le fichier telecharge (`0.csv`) : la convention de
      # nommage vit dans `fi$name`, pas dans `fi$datapath`. On recopie sous le
      # vrai nom, sans quoi la commune serait illisible.
      tmp_dir <- file.path(tempdir(), paste0("csv_", as.integer(Sys.time())))
      dir.create(tmp_dir, recursive = TRUE, showWarnings = FALSE)
      chemin <- file.path(tmp_dir, basename(fi$name))
      ok_copie <- tryCatch(file.copy(fi$datapath, chemin, overwrite = TRUE),
                           error = function(e) FALSE)
      if (!isTRUE(ok_copie)) chemin <- fi$datapath

      i18n_snap <- shiny::isolate(i18n())
      croiser <- isTRUE(shiny::isolate(input$csv_cross_onf))
      notif_id <- "csv_import_loading"
      shiny::showNotification(
        htmltools::tagList(
          shiny::icon("spinner", class = "fa-spin me-2"),
          sprintf("%s...", i18n_snap$t("csv_import_running"))),
        type = "message", duration = NULL, closeButton = FALSE,
        id = notif_id, session = session)

      later::later(function() {
        tryCatch({
          res <- importer_parcelles_csv(chemin)
          shiny::removeNotification(notif_id, session = session)

          if (!identical(res$status, "ok")) {
            msg <- switch(res$status,
              bad_name  = i18n_snap$t("csv_err_bad_name"),
              no_refs   = i18n_snap$t("csv_err_no_refs"),
              cadastre  = sprintf(i18n_snap$t("csv_err_cadastre"), res$code_insee),
              no_match  = sprintf(i18n_snap$t("csv_err_no_match"),
                                  res$commune, res$code_insee),
              i18n_snap$t("error"))
            shiny::showNotification(msg, type = "error", duration = 12,
                                    session = session)
            return()
          }

          # Le cadastre rend `id`, `section`, `numero`, `contenance` : le
          # contrat qu'attendent `create_project()` et `ug_init_default()`.
          projet <- create_project(name = res$commune, parcels = res$parcelles)
          pid <- projet$id

          charge <- load_project(pid)
          if (is.null(charge)) {
            shiny::showNotification(i18n_snap$t("error"), type = "error",
                                    session = session)
            return()
          }

          # Croisement ONF optionnel, sur le projet frais. Les echecs du service
          # (pas de foret publique, WFS muet) ne doivent PAS annuler l'import :
          # le projet existe, il est simplement sans UGF forestieres.
          if (croiser) {
            cfg_csv <- project_onf_params(charge$metadata)
            onf <- onf_load_parcelles(charge$parcels,
                                      domanialite = cfg_csv$domanialite,
                                      clip_cadastre = cfg_csv$clip_cadastre)
            if (identical(onf$status, "ok")) {
              out <- tryCatch(
                onf_projet_croise(charge, onf$parcelles, i18n = i18n_snap),
                error = function(e) {
                  cli::cli_warn("Croisement ONF apres import CSV : {conditionMessage(e)}")
                  NULL
                })
              if (!is.null(out) && identical(out$status, "ok")) {
                # AUCUNE PURGE ICI, et c'est une decision, pas un oubli
                # (Pascal, 2026-08-26) : un CSV liste la foret. Ses parcelles
                # SONT la foret, toutes, et en supprimer contredirait le fichier
                # que l'utilisateur vient de fournir. Le reglage reste offert au
                # bouton ONF, ou la selection est faite a la main sur la carte
                # et peut deborder.
                #
                # Ce qui reglait le probleme d'origine - l'UGF « Hors foret
                # publique » survivant a l'import - n'est plus la purge mais le
                # RATTACHEMENT : chaque bout de parcelle cadastrale sans numero
                # forestier rejoint la parcelle voisine avec laquelle il partage
                # la plus longue frontiere. Rien n'est mis de cote, donc rien ne
                # reste a purger.
                .onf_commit(out$projet, with_parcels = FALSE)
                charge <- load_project(pid)
              } else {
                .onf_notify_status(out$status %||% "no_overlap", i18n_snap)
              }
            } else {
              .onf_notify_status(onf$status, i18n_snap)
            }
          }

          # Etat du module.
          rv$projet_ug <- charge
          rv$redraw_counter <- shiny::isolate(rv$redraw_counter) + 1L
          rv$selected_tenement_ids <- character(0)
          rv$map_needs_zoom <- TRUE

          # Remplacement : l'ancien projet n'est detruit QU'ICI, une fois le
          # nouveau complet (cree, charge, croise). Tous les chemins d'echec
          # ci-dessus repartent avant ce point, projet courant intact.
          .remplacer_projet_courant(app_state, charge)

          # Rafraichit TOUS les sous-onglets de Selection : la carte cadastrale
          # (via mod_map), la recherche de commune (via mod_search, qui rapatrie
          # la geometrie), et par ricochet la carte UGF et ce tableau.
          app_state$restore_project <- list(
            commune_code    = res$code_insee,
            department_code = substr(res$code_insee, 1, 2),
            parcels         = charge$parcels,
            geometry        = charge$commune_geometry,
            selected_ids    = charge$parcels$id,
            timestamp       = Sys.time()
          )

          surface <- sum(as.numeric(res$parcelles$contenance), na.rm = TRUE) / 1e4
          shiny::showNotification(
            sprintf(i18n_snap$t("csv_import_ok_fmt"), res$commune,
                    nrow(res$parcelles), surface),
            type = "message", duration = 10, session = session)

          # Une liste partiellement resolue reste un succes - une parcelle a pu
          # etre fusionnee ou renumerotee depuis. Mais il faut le DIRE, sinon la
          # surface obtenue passe pour la surface demandee.
          if (length(res$absentes) > 0L) {
            shiny::showNotification(
              sprintf(i18n_snap$t("csv_import_absentes_fmt"),
                      length(res$absentes),
                      paste(utils::head(res$absentes, 10), collapse = ", ")),
              type = "warning", duration = 15, session = session)
          }
        }, error = function(e) {
          shiny::removeNotification(notif_id, session = session)
          shiny::showNotification(
            paste(i18n_snap$t("error"), conditionMessage(e)),
            type = "error", duration = 12, session = session)
        })
      }, delay = 0.05)
    })

    # Rappel des calibrages ONF en vigueur, avec le chemin pour les changer.
    # Une sidebar qui perd ses reglages sans dire ou ils sont partis oblige a
    # les chercher : le rappel est ce qui rend le deplacement acceptable.
    output$onf_params_rappel <- shiny::renderUI({
      i18n <- i18n()
      cfg <- project_onf_params(app_state$current_project$metadata)
      dom <- paste(vapply(cfg$domanialite, function(d) {
        i18n$t(if (identical(d, "domaniale")) "onf_domanialite_domaniale"
               else "onf_domanialite_autre")
      }, character(1)), collapse = " + ")

      htmltools::div(
        class = "text-muted small mb-2",
        htmltools::tags$div(sprintf(i18n$t("onf_rappel_domanialite"), dom)),
        htmltools::tags$div(sprintf(
          i18n$t("onf_rappel_purge"),
          i18n$t(if (isTRUE(cfg$purger)) "yes" else "no"),
          round(100 * cfg$seuil_foret))),
        htmltools::tags$div(i18n$t("onf_rappel_ou"))
      )
    })

    # ================================================================
    # ACTION: Undo split (restore single tenement per parcel)
    # ================================================================
    shiny::observeEvent(input$btn_undo_split, {
      if (deny_if_readonly(app_state)) return()
      projet <- rv$projet_ug
      if (is.null(projet) || !has_ug_data(projet)) {
        shiny::showNotification(i18n()$t("ug_no_data"), type = "warning")
        return()
      }

      # Find parcels that have multiple tenements (i.e., have been split)
      atom_counts <- table(projet$tenements$parent_parcelle_id)
      split_parcels <- names(atom_counts[atom_counts > 1])

      if (length(split_parcels) == 0) {
        shiny::showNotification(i18n()$t("ug_no_split_to_undo"), type = "info")
        return()
      }

      # Build choices
      parcels <- projet$parcels
      id_col <- intersect(c("id", "nemeton_id", "geo_parcelle"), names(parcels))
      parcel_labels <- if ("geo_parcelle" %in% names(parcels)) {
        geo_refs <- as.character(parcels$geo_parcelle)
        ids <- as.character(parcels[[id_col[1]]])
        stats::setNames(ids, geo_refs)[split_parcels]
      } else {
        stats::setNames(split_parcels, split_parcels)
      }

      shiny::showModal(shiny::modalDialog(
        title = i18n()$t("ug_undo_split"),
        shiny::selectInput(
          ns("undo_split_parcelle"),
          label = i18n()$t("ug_split_select_parcel"),
          choices = parcel_labels
        ),
        shiny::p(
          class = "text-muted",
          i18n()$t("ug_undo_split_hint")
        ),
        footer = htmltools::tagList(
          shiny::modalButton(i18n()$t("cancel")),
          shiny::actionButton(
            ns("confirm_undo_split"),
            i18n()$t("ug_undo_split"),
            class = "btn-warning"
          )
        )
      ))
    })

    shiny::observeEvent(input$confirm_undo_split, {
      if (deny_if_readonly(app_state)) return()
      shiny::removeModal()
      parcelle_id <- input$undo_split_parcelle
      if (is.null(parcelle_id)) return()

      tryCatch({
        projet <- rv$projet_ug
        projet <- tenement_undo_split(projet, parcelle_id)

        if (!is.null(projet$metadata$id)) {
          save_ug_data(projet$metadata$id, projet)
        }
        rv$projet_ug <- projet
        app_state$current_project$tenements <- projet$tenements
        app_state$current_project$ugs <- projet$ugs

        shiny::showNotification(
          sprintf(i18n()$t("ug_undo_split_success"), parcelle_id),
          type = "message"
        )
      }, error = function(e) {
        shiny::showNotification(e$message, type = "error")
      })
    })

  })
}

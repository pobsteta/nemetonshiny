# ===========================================================================
# Module - Accessibilite forestiere (ForetAccess), sous-onglet de " Terrain "
# ===========================================================================
#
# Presentation Shiny du service `R/service_accessibility.R` (adaptateur autour
# de `foretaccess`). Aucune logique metier ici (regle 2) : le module orchestre
# l'UI, l'execution asynchrone (worker `future`) et le rendu carte/tableau.
#
# Premier increment : moteurs TERRESTRES (skidder, porteur, camion DFCI). Le
# calcul est long -> `ExtendedTask` + `future_promise`, meme patron que le moteur
# reGeneration (notif persistante bas-droite avec chrono, retour immediat).

# --- Palettes du comparateur de desserte -----------------------------------
# Deux legendes cohabitent a l'ecran : la CLASSE du troncon BD TOPO (couche de
# fond, trait fin) et la SOURCE du troncon corrige (par-dessus, trait epais).
# La contrainte n'est donc pas seulement de separer les classes entre elles,
# mais AUSSI de ne pas les confondre avec la palette de source d'a cote.
#
# Constantes de FICHIER et non locales au serveur : `test-acc_palettes.R` mesure
# leur separation, ce qu'il ne pourrait pas faire depuis une closure.

#' Colours of the BD TOPO road classes (background layer)
#'
#' Chosen under three constraints, verified by `test-acc_palettes.R`:
#' pairwise Lab distance >= 20 across the 6 intra-palette pairs AND the 12 pairs
#' against [DESS_SOURCE_COLS]; the same holds under simulated deutan / protan /
#' tritan vision; and each hue keeps a >= 3:1 contrast ratio against white (WCAG
#' non-text contrast) so it stays readable on the light RVT relief.
#'
#' The previous `route = "#37474F"` sat at Lab distance 8 from
#' `bdtopo = "#455A64"` - the same colour, to the eye, in two adjacent legends.
#' `piste` and `reseau_public` keep their hue family (brown, blue); only `route`
#' had to move.
#'
#' @noRd
DESS_CLASSE_COLS <- c(route = "#C62828", piste = "#3E2723",
                      reseau_public = "#1E88E5", hors_desserte = "#BDBDBD")

#' Colours of the corrected road sources (foreground layer)
#'
#' Red for BD TOPO, green for OSM: the point of this layer is to show at a
#' glance what the correction ADDED. Red is `#FF0000` and not a softer one
#' because the class legend next to it already carries a red (`route`,
#' `#C62828`): every usual red sits 7 to 20 Lab units from it — the very
#' collision fixed in 0.122.6 — while pure red keeps 34.7. Measured, and held by
#' `test-acc_palettes.R`, which refuses any cross-legend pair under 20.
#'
#' The corrected network KEEPS the whole BD TOPO and adds what OSM carries on
#' top; LiDAR detection will provide the third. Three plainly distinct hues,
#' distinct from [DESS_CLASSE_COLS] as well.
#'
#' @noRd
DESS_SOURCE_COLS <- c(bdtopo = "#FF0000", osm = "#2CA02C",
                      detectee = "#F9A825")


#' Name of the single relief overlay group of the Accessibility map
#'
#' There is ONE relief on this map, and one checkbox for it. Two code paths
#' paint it - the map render (semi-transparent backdrop over OSM/Satellite) and
#' the desserte comparator (opaque backdrop under the road lines) - but they
#' display the very same file: `.acc_cvat_overlay_raster()` and the comparator
#' both go through `generate_rvt()`.
#'
#' They used to paint into two different groups, only one of which was declared
#' in the `addLayersControl()` overlays. Unchecking "Relief CVAT" while the
#' comparator was on therefore hid a raster the user could not see and left the
#' visible one - the comparator's - on screen, with no checkbox of its own.
#'
#' @noRd
ACC_RELIEF_GROUP <- "Relief CVAT"


#' Names of the two comparator layers of the Accessibility map
#'
#' The desserte comparator paints two superposed layers: the BD TOPO network
#' coloured by class (below, thin) and the LiDAR-corrected network coloured by
#' source (above, thick). Both are declared in the `addLayersControl()` overlays
#' so each can be toggled - hiding the corrected one is precisely how you read
#' what it changed. Like the relief, they are declared unconditionally: they are
#' painted only while the comparator is selected, and a group with no layer in
#' it is an inert checkbox.
#'
#' @noRd
ACC_DESSERTE_CORR_GROUP <- "Desserte corrig\u00e9e"

#' @rdname ACC_DESSERTE_CORR_GROUP
#' @noRd
ACC_DESSERTE_ORIG_GROUP <- "Desserte origine"


#' Name of the computed-accessibility raster group
#'
#' These group names ARE the labels shown in leaflet's layer control - there is
#' no separate display string - so they carry their accents. Written `\uXXXX`
#' per the repo's source-encoding rule.
#'
#' @noRd
ACC_ACCESSIBILITE_GROUP <- "Accessibilit\u00e9"

#' Name of the log-landing group, shared by the Accessibility and Desserte maps
#'
#' Defined once and consumed by both modules: the two tabs show the same menu
#' entry, and two literals would let them drift apart - an accent added on one
#' side only is exactly how that starts.
#'
#' @noRd
PLACES_DEPOT_GROUP <- "Places de d\u00e9p\u00f4t"


#' Buffer around the forest AOI, in metres, from the sidebar input
#'
#' The input is expressed in METRES (default 250 m) - the services downstream
#' (`run_accessibility()`, `run_desserte_lidar_correction()`, `.acc_rvt_cvat()`)
#' all take `buffer_m`, so no unit conversion happens anywhere. A numeric input
#' cleared by the user yields `NA`, not `NULL`: `%||%` would let it through and
#' `NA` would then poison the cache key and the buffered geometry, hence the
#' explicit `is.finite()` guard.
#'
#' @param value Raw `input$buffer_m` (may be `NULL`, `NA` or a string).
#' @return A finite non-negative numeric, in metres.
#' @noRd
.acc_buffer_m <- function(value) {
  v <- suppressWarnings(as.numeric(value %||% NA))
  if (!is.finite(v)) return(250)
  max(0, v)
}

#' Semantic colour per accessibility class name
#'
#' The class colour must encode the class MEANING, not its position in the level
#' table: with a purely positional palette the DFCI raster painted
#' `inaccessible` bright green (it is level 1) - the exact opposite of what the
#' map should say. Keyed by the raw `foretaccess` class name so every engine
#' shares one convention: green = workable/close, orange->red = degraded/far,
#' grey/slate = out of reach, transparent = outside the forest mask.
#' @noRd
.ACC_CLASS_COLORS <- c(
  # Moteurs terrestres (skidder / porteur)
  parcourable          = "#2E7D32",
  accessible           = "#9CCC65",
  non_accessible       = "#C62828",
  # Moteur cable-mat
  accessible_cable     = "#2E7D32",
  # Communes
  inaccessible         = "#9E9E9E",
  inexploitable        = "#455A64",
  # Camion DFCI (c1 = le plus proche donc le mieux defendu)
  non_defendable_pente = "#78909C",
  defendable_c1        = "#2E7D32",
  defendable_c2        = "#FDD835",
  defendable_c3        = "#FB8C00",
  hors_foret           = "#FFFFFF00")

#' Colours for a categorical accessibility raster's levels
#'
#' Resolution order: (1) the raster's own colour table (`terra::coltab`) when it
#' carries a meaningful one - the `classes_debardage` raster ships a Sylvaccess
#' green->red distance ramp worth honouring; (2) `.ACC_CLASS_COLORS` keyed by
#' class name; (3) a positional qualitative palette for anything still unknown.
#' Returns one colour per `codes` entry.
#'
#' @param rast The categorical `SpatRaster`.
#' @param codes Numeric level codes.
#' @param labs Character class names, same length as `codes`.
#' @noRd
.acc_level_colors <- function(rast, codes, labs = NULL) {
  pal <- c("#2E7D32", "#9CCC65", "#FDD835", "#FB8C00", "#C62828",
           "#6D4C41", "#9E9E9E", "#455A64")
  ct <- tryCatch(terra::coltab(rast)[[1]], error = function(e) NULL)
  if (is.data.frame(ct) && all(c("red", "green", "blue") %in% names(ct))) {
    # terra nomme la 1re colonne " values " (ou " value " selon la version) : on
    # prend la colonne d'index par position pour etre robuste.
    idx <- match(codes, ct[[1]])
    a <- if ("alpha" %in% names(ct)) ct$alpha[idx] else rep(255L, length(idx))
    a[is.na(a)] <- 255L
    hex <- grDevices::rgb(ct$red[idx], ct$green[idx], ct$blue[idx],
                          alpha = a, maxColorValue = 255)
    # Ignorer une coltab degeneree (terra en pose parfois une toute noire).
    if (length(unique(hex[!is.na(hex)])) > 1L) return(hex)
  }
  out <- rep(NA_character_, length(codes))
  if (!is.null(labs)) out <- unname(.ACC_CLASS_COLORS[as.character(labs)])
  miss <- is.na(out)
  if (any(miss)) {
    out[miss] <- pal[((which(miss) - 1L) %% length(pal)) + 1L]
  }
  out
}

#' Mask the `hors_foret` cells of a class raster to NA
#'
#' Cells outside the forest mask must not paint over the basemap - and they are
#' the majority of the rectangular extent once a buffer widens it. We do NOT
#' rely on the palette's alpha: `colorFactor()` + `addRasterImage()` drop the
#' alpha channel of a `#RRGGBBAA`, which is what rendered `hors_foret` as opaque
#' WHITE. Masking the cells to NA instead routes them through
#' `na.color = "transparent"`, which is honoured. The class is located by its
#' LABEL (the code varies: 4 for skidder/porteur, 6 for DFCI, 9 for the skidding
#' classes), never hard-coded.
#'
#' **Only explicitly namespaced `terra::` calls here.** `terra` is in `Imports:`
#' but the NAMESPACE pulls in no terra S4 method, so inside this package
#' `rast %in% codes` dispatches to `base::%in%` - which returns a length-1
#' `FALSE` on a `SpatRaster` instead of a mask, making the whole masking a
#' silent no-op (the original cause of the opaque white). Same trap for the
#' `levels<-` replacement method. Going through `values()`/`setValues()` keeps
#' the operation immune to S4 dispatch, and yields a plain numeric raster -
#' which is exactly what `colorFactor(domain = codes)` expects downstream.
#'
#' @param rast The categorical `SpatRaster`.
#' @param codes Numeric level codes.
#' @param labs Character class names, same length as `codes`.
#' @return The raster with `hors_foret` cells set to NA (unchanged on failure).
#' @noRd
.acc_mask_hors_foret <- function(rast, codes, labs) {
  hf <- !is.na(labs) & labs == "hors_foret"
  if (!any(hf)) return(rast)
  tryCatch({
    v <- terra::values(rast)
    # `v` est ici un vecteur/matrice numerique de base : le `%in%` de base est
    # le bon operateur, et il compare les CODES (pas les libelles).
    v[v %in% codes[hf]] <- NA
    terra::setValues(terra::rast(rast), v)
  }, error = function(e) rast)
}

#' Human-friendly legend labels for accessibility classes
#'
#' Maps the raw `foretaccess` class names to display labels. The DFCI
#' `defendable_cN` classes become **distance bands** read from
#' `foretaccess_config()$dfci$classes_distance_m` (e.g. " 0 a 120 m defendable "),
#' so the legend shows the actual defence distances instead of opaque C1/C2/C3
#' codes. The other DFCI classes get plain-language labels. Unknown labels pass
#' through unchanged (skidder/forwarder classes, skidding-distance bands already
#' expressed as ranges).
#' @noRd
.acc_legend_labels <- function(labs, i18n) {
  b <- tryCatch(foretaccess::foretaccess_config()$dfci$classes_distance_m,
                error = function(e) c(0, 120, 280, 440))
  dfci_band <- function(n) {
    if (length(b) < n + 1L) return(NA_character_)
    sprintf(i18n$t("acc_dfci_defendable_fmt"), as.integer(b[n]), as.integer(b[n + 1L]))
  }
  map <- c(
    inaccessible         = i18n$t("acc_dfci_inaccessible"),
    non_defendable_pente = i18n$t("acc_dfci_non_defendable_pente"),
    defendable_c1        = dfci_band(1L),
    defendable_c2        = dfci_band(2L),
    defendable_c3        = dfci_band(3L),
    # Classes des moteurs terrestres : elles s'affichaient jusqu'ici en brut
    # (" parcourable ", " non_accessible ").
    parcourable          = i18n$t("acc_class_parcourable"),
    accessible           = i18n$t("acc_class_accessible"),
    non_accessible       = i18n$t("acc_class_non_accessible"),
    accessible_cable     = i18n$t("acc_class_accessible_cable"),
    inexploitable        = i18n$t("acc_class_inexploitable"))
  out <- unname(map[labs])
  out[is.na(out)] <- labs[is.na(out)]
  out
}

#' Explanatory text of one entry of the "displayed layer" selector
#'
#' The decisive thresholds (slope limits, winching and crane distances, DFCI
#' bands) are read from `foretaccess_config()` rather than written into the
#' translations: hard-coding them there would let them drift silently from the
#' engine the day the configuration changes. Same principle as the DFCI legend
#' in [.acc_legend_labels()], including the documented defaults as a fallback
#' when the configuration cannot be read.
#'
#' @param layer Layer id (`"skidder"`, `"porteur"`, `"camion_dfci"`, `"cable"`,
#'   `"classes_debardage"`, `"desserte_comparee"`).
#' @param i18n Translator.
#' @return A single string, or `NULL` when the layer has no description.
#' @noRd
.acc_layer_info <- function(layer, i18n) {
  cfg <- tryCatch(foretaccess::foretaccess_config(), error = function(e) list())
  n <- function(x, defaut) {
    v <- suppressWarnings(as.numeric(x))
    if (length(v) != 1L || !is.finite(v)) defaut else v
  }
  switch(
    layer,
    skidder = sprintf(
      i18n$t("acc_layer_info_skidder"),
      n(cfg$skidder$pente_skidder_max_pct, 30),
      n(cfg$skidder$debardage_amont_max_m, 50),
      n(cfg$skidder$debardage_aval_max_m, 100)),
    porteur = sprintf(
      i18n$t("acc_layer_info_porteur"),
      n(cfg$porteur$pente_travers_max_pct, 15),
      n(cfg$porteur$pente_montee_max_pct, 30),
      n(cfg$porteur$pente_descente_max_pct, 25),
      n(cfg$porteur$portee_grue_m, 8)),
    camion_dfci = sprintf(
      i18n$t("acc_layer_info_dfci"),
      n(cfg$dfci$distance_defense_max_m, 440),
      n(cfg$dfci$pente_defense_max_pct, 110)),
    cable = sprintf(
      i18n$t("acc_layer_info_cable"),
      n(cfg$cable$longueur_max_m, 750)),
    classes_debardage = i18n$t("acc_layer_info_debardage"),
    # Le comparateur a deja sa prose, affichee sous le selecteur quand il est
    # choisi : le " i " la reprend plutot que d'en inventer une seconde.
    desserte_comparee = i18n$t("acc_compare_hint"),
    NULL)
}


#' Paint one accessibility raster onto a leafletProxy in a dedicated map pane.
#'
#' Shared by the single-layer overlay and the ACCESSFOR swipe comparison. Applies
#' the raster's own coltab (falling back to `.acc_level_colors`), always renders
#' `hors_foret` transparent (mask to NA + alpha 00), and - when `legend_id` is
#' supplied - draws the legend without the transparent classes. Returns the proxy.
#'
#' @noRd
.acc_paint_raster <- function(proxy, rp, pane, group, op, i18n,
                              legend_id = NULL) {
  if (is.null(rp) || !file.exists(rp)) return(proxy)
  rast <- tryCatch(terra::rast(rp), error = function(e) NULL)
  if (is.null(rast)) return(proxy)
  lv <- tryCatch(terra::levels(rast)[[1]], error = function(e) NULL)
  if (is.data.frame(lv) && nrow(lv) > 0L) {
    codes <- as.numeric(lv[[1]]); labs <- as.character(lv[[2]])
    cols <- .acc_level_colors(rast, codes, labs)
    hf <- !is.na(labs) & labs == "hors_foret"
    if (any(hf)) cols[hf] <- "#FFFFFF00"
    rast <- .acc_mask_hors_foret(rast, codes, labs)
    cmap <- leaflet::colorFactor(cols, domain = codes, na.color = "transparent")
    proxy <- leaflet::addRasterImage(proxy, rast, colors = cmap, opacity = op,
      method = "ngb", group = group,
      options = leaflet::gridOptions(pane = pane))
    if (!is.null(legend_id)) {
      keep <- !is.na(cols) & substr(cols, 8L, 9L) != "00"
      proxy <- leaflet::addLegend(proxy, "bottomright", colors = cols[keep],
        labels = .acc_legend_labels(labs[keep], i18n),
        title = i18n$t("acc_legend_title"), layerId = legend_id, opacity = 0.8)
    }
  } else {
    proxy <- leaflet::addRasterImage(proxy, rast, opacity = op, method = "ngb",
      group = group, options = leaflet::gridOptions(pane = pane))
  }
  proxy
}

#' @noRd
mod_accessibility_ui <- function(id) {
  ns <- shiny::NS(id)
  i18n <- get_i18n(get_app_options()$language %||% "fr")

  bslib::layout_sidebar(
    # Barre laterale GAUCHE : selection des moteurs, lancement, export. Ce sont
    # les commandes du CALCUL.
    sidebar = bslib::sidebar(
      # `open = TRUE` et NON `"always"` : `"always"` supprime le chevron de
      # repli. La sidebar est retractable comme celle de l'onglet Export
      # terrain, pour rendre toute la largeur a la carte. Ouverte par defaut :
      # elle porte le bouton " Lancer l'analyse ".
      id = ns("sidebar"),
      width = 320, open = TRUE, position = "left",

      # Carte repliable, MEME structure que le bloc " Ingestion terrain " de
      # l'onglet Import terrain (mod_field_ingest) : en-tete vert cliquable,
      # icone de l'onglet, chevron, `collapse show`. Depliee par defaut - elle
      # porte le bouton " Lancer l'analyse ".
      htmltools::tags$div(
        class = "card mb-3",
        htmltools::tags$div(
          class = "card-header bg-success text-white py-2",
          style = "cursor: pointer;",
          `data-bs-toggle` = "collapse",
          `data-bs-target` = paste0("#", ns("acc_collapse")),
          `aria-expanded` = "true",
          `aria-controls` = ns("acc_collapse"),
          htmltools::div(
            class = "d-flex align-items-center justify-content-between",
            htmltools::div(
              class = "d-flex align-items-center",
              # Meme icone que l'onglet lui-meme (cf. app_ui.R).
              bsicons::bs_icon("signpost-split", class = "me-2"),
              i18n$t("tab_terrain_accessibilite")
            ),
            bsicons::bs_icon("chevron-down", class = "collapse-icon")
          )
        ),
        htmltools::tags$div(
          id = ns("acc_collapse"),
          class = "collapse show",
          htmltools::tags$div(
            class = "card-body",

            htmltools::tags$p(class = "text-muted small", i18n$t("acc_intro")),

            # --- Correction LiDAR de la desserte (NDP 1) - ETAPE DECOUPLEE ------
            # La qualification LiDAR (lourde : ~2-3 h, gros pic memoire) est un
            # geste SEPARE et ponctuel : ce bouton corrige la desserte UNE fois
            # (geometrie recalee + largeurs mesurees + troncons fantomes retires)
            # et la persiste sur disque. Les runs moteurs ci-dessous restent
            # LEGERS et la reutilisent via la case " Utiliser la desserte
            # corrigee ". Decoupler evite de relancer la qualif a chaque run et
            # d'etrangler la memoire pendant l'analyse.
            # La mise en garde sur la duree (~2-3 h) et la memoire passe dans un
            # " i " a cote du titre : elle occupait sept lignes de la barre
            # laterale en permanence alors qu'elle se lit UNE fois, avant de
            # cliquer. Le " i " est ici hors d'un <label>, donc `info_popover()`
            # suffit - pas besoin du variant qui neutralise l'activation.
            htmltools::div(
              class = "small d-block mb-1",
              htmltools::tags$strong(i18n$t("acc_correct_section")), " ",
              info_popover(i18n$t("acc_ndp1_duration_note"), placement = "right")),
            bslib::input_task_button(
              ns("correct_desserte"), i18n$t("acc_correct_run"),
              label_busy = i18n$t("acc_correct_running"),
              icon = bsicons::bs_icon("magic"),
              type = "secondary", class = "w-100 my-1 btn-sm"),
            shiny::uiOutput(ns("correct_status")),
            shiny::uiOutput(ns("use_corrected_ui")),
            htmltools::tags$hr(class = "my-2"),

            shiny::checkboxGroupInput(
              ns("engines"), i18n$t("acc_engines_label"),
              choices = stats::setNames(
                ACCESSIBILITY_ENGINES,
                c(i18n$t("acc_engine_skidder"),
                  i18n$t("acc_engine_porteur"),
                  i18n$t("acc_engine_dfci"),
                  i18n$t("acc_engine_cable"))),
              # Cable NON pre-coche : calcul long (balayage 360deg/pixel).
              selected = setdiff(ACCESSIBILITY_ENGINES, "cable")),

            bslib::input_task_button(
              ns("run"), i18n$t("acc_run"),
              label_busy = i18n$t("acc_running"),
              icon = bsicons::bs_icon("play-fill"),
              type = "primary", class = "w-100 mb-3"),
            # Roue dentee + chrono SOUS le bouton (parite FAST/FORDEAD/
            # RECONFORT) : le run peut durer, le toast bas-droite peut etre
            # manque ou ferme.
            shiny::uiOutput(ns("run_status"))
          )
        )
      )
    ),

    bslib::card(
      full_screen = TRUE,
      bslib::card_header(i18n$t("acc_map_title")),
      # Barre laterale DROITE (contre la carte) : les resultats et leur AFFICHAGE
      # - selecteur de raster calcule, zone tampon et opacite.
      bslib::layout_sidebar(
        fillable = TRUE,
        sidebar = bslib::sidebar(
          position = "right", open = "always", width = 280,
          htmltools::tags$strong(i18n$t("acc_layer_label")),
          shiny::uiOutput(ns("layer_ui")),
          # Aide contextuelle du comparateur : n'apparait que lorsque la
          # pseudo-couche " Desserte BD TOPO / corrigee " est selectionnee.
          shiny::uiOutput(ns("compare_hint_ui")),
          htmltools::tags$hr(class = "my-2"),
          shiny::numericInput(
            ns("buffer_m"), i18n$t("acc_buffer"),
            value = 250, min = 0, max = 20000, step = 50),
          htmltools::tags$p(class = "text-muted small", i18n$t("acc_buffer_help")),
          shiny::sliderInput(
            ns("opacity"), i18n$t("acc_opacity"),
            min = 0, max = 1, value = 0.7, step = 0.05, ticks = FALSE),
          htmltools::tags$hr(class = "my-2"),
          # Exports regroupes dans un accordeon repliable " Exports " (replie par
          # defaut), meme presentation que l'onglet reGeneration.
          # Le panneau " Validation ACCESSFOR (IGN) " (table d'accord classe par
          # classe + taux global) a ete retire : il n'etait pas utilise. La
          # comparaison elle-meme RESTE - le raster ACCESSFOR est toujours calcule
          # par le worker et reste consultable en volet swipe via la couche
          # " Classes de debardage/ACCESSFOR (IGN) " du selecteur.
          bslib::accordion(
            open = FALSE,
            bslib::accordion_panel(
              title = i18n$t("action_plan_section_exports"),
              icon = bsicons::bs_icon("box-arrow-up"),
              shiny::downloadButton(
                ns("export_gpkg"), i18n$t("acc_download_gpkg"),
                icon = shiny::icon("database"),
                class = "btn-outline-success btn-sm w-100")))
        ),
        # Badge de provenance DFCI (au-dessus de la carte) : n'apparait que
        # lorsque la couche " Camion DFCI " est affichee.
        shiny::uiOutput(ns("dfci_badge")),
        shiny::uiOutput(ns("desserte_badge")),
        leaflet::leafletOutput(ns("map"), height = "72vh")
      )
    )
  )
}

#' @noRd
mod_accessibility_server <- function(id, app_state) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    i18n <- get_i18n(get_app_options()$language %||% "fr")

    rv <- shiny::reactiveValues(result = NULL, running = FALSE, start = NULL,
                                profil = NULL)

    # Chemin du paquet en dev (pkgload) : rejoue dans le worker pour disposer des
    # fonctions internes `run_accessibility()`.
    .dev_pkg_path <- tryCatch(
      if (isTRUE(pkgload::is_dev_package("nemetonshiny")))
        find.package("nemetonshiny") else NULL,
      error = function(e) NULL)

    # AOI (foret) du projet en EPSG:2154 - repli indicators_sf -> UGF -> parcelles.
    units_sf <- shiny::reactive({
      .resolve_project_aoi_2154(app_state$current_project)
    })

    # --- Worker asynchrone : acquisition desserte + pretraitement + moteurs -----
    acc_task <- shiny::ExtendedTask$new(
      function(aoi_path, engines, cache_dir, buffer_m, dev_path, app_opts,
               use_corrected_desserte, project_path) {
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
          utils::getFromNamespace("run_accessibility", "nemetonshiny")(aoi_path, engines, cache_dir, buffer_m,
            use_corrected_desserte = use_corrected_desserte,
            project_path = project_path)
        }, seed = TRUE)
      })

    busy <- shiny::reactive(identical(acc_task$status(), "running"))

    # Lie le bouton a la tache : sans ce binding, bslib remet `input_task_button`
    # a l'etat " ready " au flush reactif suivant le clic - le bouton ne reste
    # donc PAS grise pendant tout le calcul async. Avec le binding, il affiche le
    # spinner + libelle " busy " tant que la tache tourne (comme dans reGeneration).
    bslib::bind_task_button(acc_task, "run")

    # --- Correction LiDAR de la desserte (NDP 1) - worker dedie, DECOUPLE --------
    # Produit `desserte_corrigee.gpkg` UNE fois (qualifier_desserte, lourd ~2-3 h) ;
    # les runs moteurs le reutilisent ensuite via la case " utiliser ". Meme patron
    # async que le moteur, mais tache separee : les runs moteurs ne portent plus le
    # cout (ni le pic memoire) de la qualif.
    correct_task <- shiny::ExtendedTask$new(
      function(aoi_path, cache_dir, buffer_m, dev_path, app_opts, project_path) {
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
          utils::getFromNamespace("run_desserte_lidar_correction", "nemetonshiny")(aoi_path, cache_dir,
            buffer_m, project_path)
        }, seed = TRUE)
      })
    bslib::bind_task_button(correct_task, "correct_desserte")

    # --- Worker asynchrone : generation du fond relief RVT/CVAT -----------------
    # `generate_rvt()` (vat_combined foretaccess) sur une mosaique LiDAR complete
    # coute ~1 min : le lancer synchrone dans l'observe du comparateur gelerait la
    # boucle Shiny. On l'execute donc dans un worker (le resultat est un CHEMIN,
    # serialisable) et l'observe dedie plus bas peint le fond quand il arrive. Le
    # cas peu couteux (cache/CVAT pre-calcule) reste synchrone (cf. .rvt_is_cheap).
    rvt_task <- shiny::ExtendedTask$new(
      function(mnt_path, dev_path, app_opts) {
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
          utils::getFromNamespace("generate_rvt", "nemetonshiny")(mnt_path)
        }, seed = TRUE)
      })
    # Memo du fond relief : `list(mnt = <MNT source>, out = <RVT calcule>)` pour
    # le dernier fond obtenu, et MNT dont le calcul async est en vol. Evite de
    # relancer le worker (~1 min) quand on revient sur le comparateur, et de
    # laisser la carte sans relief apres un re-dessin.
    rvt_ready <- shiny::reactiveVal(NULL)
    rvt_pending <- shiny::reactiveVal(NULL)

    # Disponibilite de la desserte corrigee (fichier sur disque) : rafraichie au
    # changement de projet + a la fin d'une correction. Pilote la case " utiliser ".
    correct_refresh <- shiny::reactiveVal(0L)
    corrected_available <- shiny::reactive({
      correct_refresh()
      pp <- tryCatch(app_state$current_project$path, error = function(e) NULL)
      if (is.null(pp) || !nzchar(pp)) return(FALSE)
      file.exists(.corrected_desserte_path(.accessibility_cache_dir(pp)))
    })
    shiny::observeEvent(app_state$current_project,
      correct_refresh(correct_refresh() + 1L), ignoreNULL = FALSE)

    # --- Pre-calcul du CVAT des qu'un projet avec MNT LiDAR est ouvert ----------
    # Materialise <base>_CVAT_8bit_foretaccess.tif en tache de fond (best-effort),
    # pour que le comparateur ET le fond de carte " Relief CVAT " soient
    # instantanes (via .rvt_precomputed). UNIQUEMENT sur le MNT LiDAR HD natif (pas
    # le repli WMS, dont le striping serait amplifie). Idempotent : rien si un CVAT
    # (foretaccess OU plugin) est deja present.
    # Depart du chrono de la notif CVAT. NULL = aucune tache en cours, ce qui
    # sert aussi de garde au tick 1 s (cf. plus bas).
    cvat_start <- shiny::reactiveVal(NULL)
    cvat_prebuild_task <- shiny::ExtendedTask$new(
      function(mnt_path, aoi, buffer_m, dev_path, app_opts) {
        if (requireNamespace("future", quietly = TRUE)) {
          pc <- class(future::plan())
          if (!any(c("multisession", "multicore", "cluster") %in% pc)) {
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
          utils::getFromNamespace("build_cvat_precomputed", "nemetonshiny")(mnt_path, aoi = aoi,
                                                buffer_m = buffer_m)
        }, seed = TRUE)
      })

    # Declenche UNIQUEMENT quand on est sur l'onglet Terrain > Accessibilite - a
    # l'arrivee sur l'onglet ET au changement de zone tampon (debounce) : un buffer
    # agrandi peut rendre le CVAT existant trop court, il faut alors le recalculer
    # pour couvrir la nouvelle emprise. On NE lance PAS ce worker lourd au simple
    # chargement d'un projet depuis un autre onglet.
    buffer_m_d <- shiny::debounce(
      shiny::reactive(.acc_buffer_m(input$buffer_m)), 600)
    shiny::observeEvent(
      list(app_state$active_main_tab, app_state$active_terrain_tab,
           app_state$current_project, buffer_m_d()), {
      on_tab <- identical(app_state$active_main_tab, "terrain") &&
        identical(app_state$active_terrain_tab, "accessibility")
      if (!on_tab) return()                    # pas sur l'onglet -> aucun pre-calcul
      pp <- tryCatch(app_state$current_project$path, error = function(e) NULL)
      mnt_path <- .acc_rvt_mnt_path(pp)
      if (is.null(mnt_path) ||
          !identical(basename(mnt_path), "lidar_mnt_mosaic.tif") ||
          !.rvt_cvat_available()) return()
      aoi <- tryCatch(units_sf(), error = function(e) NULL)
      buffer_m <- buffer_m_d()
      # Relancer si AUCUN CVAT, ou si le CVAT existant NE COUVRE PAS aoi+buffer
      # (buffer agrandi). Sinon rien : pas de worker inutile.
      #
      # Second critere `.cvat_built_for()` : sur une AOI dont la couverture LiDAR
      # HD s'arrete avant aoi+buffer, le CVAT produit ne satisfera JAMAIS
      # `.cvat_covers()` - mesure sur Dabo, 4454 x 4162 m produits pour
      # 4617 x 4381 m demandes. Sans ce garde, chaque entree dans l'onglet
      # relancait un calcul de plusieurs minutes voue au meme resultat.
      existing <- .rvt_precomputed_path(mnt_path)
      cvat_res <- suppressWarnings(as.numeric(APP_CONFIG$cvat_res_m))
      if (!isTRUE(is.finite(cvat_res)) || cvat_res <= 0) cvat_res <- 2
      # MEME plafond que le service, sinon ce garde raisonnerait sur un buffer
      # que la construction ne demandera jamais : il ne serait jamais satisfait
      # et relancerait un worker (plus son toast) a chaque entree dans l'onglet.
      buffer_m <- if (is.null(aoi)) buffer_m else {
        .cvat_buffer_plafonne(mnt_path, aoi, buffer_m)
      }
      if (!is.null(existing) &&
          (is.null(aoi) ||
           isTRUE(.cvat_covers(existing, aoi, buffer_m)) ||
           isTRUE(.cvat_built_for(existing, aoi, buffer_m, cvat_res)))) {
        return()
      }
      # Echec recent avec ces memes parametres : ne pas rejouer un calcul long
      # voue au meme sort (le service le refuserait, mais autant ne pas ouvrir
      # de worker ni afficher un toast pour rien).
      if (!is.null(aoi) &&
          isTRUE(.cvat_failed_for(existing %||% .rvt_cvat_out_path(mnt_path),
                                  aoi, buffer_m, cvat_res))) {
        return()
      }
      # Message bas-droite pendant TOUT le calcul (id stable -> retire a la fin),
      # dans le cadre unifie " engrenage qui tourne + chrono MM:SS " partage avec
      # les autres taches longues du module (acc_running, acc_correct_running).
      cvat_start(Sys.time())
      shiny::showNotification(
        .running_notif_content(i18n$t("acc_cvat_prebuild_running"), cvat_start()),
        duration = NULL, closeButton = FALSE, type = "message",
        id = session$ns("cvat_prebuild"))
      cvat_prebuild_task$invoke(mnt_path, aoi, buffer_m, .dev_pkg_path,
                                get_app_options())
    }, ignoreNULL = TRUE)

    # Tick 1 s : rafraichit le chrono de la notif CVAT (meme id -> Shiny remplace
    # le contenu en place) tant que la tache tourne.
    shiny::observe({
      if (is.null(cvat_start())) return()
      shiny::invalidateLater(1000)
      shiny::showNotification(
        .running_notif_content(i18n$t("acc_cvat_prebuild_running"),
                               shiny::isolate(cvat_start())),
        duration = NULL, closeButton = FALSE, type = "message",
        id = session$ns("cvat_prebuild"))
    })

    # Fin du pre-calcul CVAT : retire le message bas-droite + court toast.
    shiny::observeEvent(cvat_prebuild_task$status(), {
      st <- cvat_prebuild_task$status()
      if (st %in% c("success", "error")) {
        cvat_start(NULL)                 # arrete le tick du chrono
        shiny::removeNotification(session$ns("cvat_prebuild"))
        if (identical(st, "success")) {
          shiny::showNotification(i18n$t("acc_cvat_prebuild_done"),
                                  duration = 4, type = "message")
        } else {
          shiny::showNotification(i18n$t("acc_cvat_prebuild_failed"),
                                  duration = 6, type = "warning")
        }
      }
    })

    rv_correct <- shiny::reactiveVal(NULL)   # dernier resume de correction
    correct_start <- shiny::reactiveVal(NULL)   # horodatage de depart (chrono)
    shiny::observeEvent(input$correct_desserte, {
      if (deny_if_readonly(app_state, i18n)) {
        bslib::update_task_button("correct_desserte", state = "ready"); return()
      }
      # Symetrique du garde du run : pas de correction pendant une analyse.
      if (isTRUE(rv$running)) {
        bslib::update_task_button("correct_desserte", state = "ready")
        shiny::showNotification(i18n$t("acc_analysis_busy"), type = "warning",
                                duration = 5); return()
      }
      project_path <- tryCatch(app_state$current_project$path, error = function(e) NULL)
      aoi <- units_sf()
      if (is.null(aoi) || is.null(project_path)) {
        bslib::update_task_button("correct_desserte", state = "ready")
        shiny::showNotification(i18n$t("acc_need_project"), type = "warning"); return()
      }
      cache_dir <- .accessibility_cache_dir(project_path)
      dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
      aoi_path <- file.path(cache_dir, "aoi_input.gpkg")
      ok <- tryCatch({ sf::st_write(aoi, aoi_path, layer = "foret", quiet = TRUE,
                                    delete_dsn = TRUE); TRUE }, error = function(e) FALSE)
      if (!isTRUE(ok)) {
        bslib::update_task_button("correct_desserte", state = "ready")
        shiny::showNotification(i18n$t("acc_need_project"), type = "warning"); return()
      }
      rv_correct(list(status = "running"))
      correct_start(Sys.time())
      # Grise " Lancer l'analyse " pendant toute la correction (reactive a la fin).
      # PAS `updateActionButton()` : sur un `input_task_button`, il reecrit le
      # bouton et EFFACE son libelle (bouton vert vide). On bascule juste
      # l'attribut `disabled` cote client (handler `nemetonSetDisabled`).
      session$sendCustomMessage("nemetonSetDisabled",
        list(id = session$ns("run"), disabled = TRUE))
      # Toast bas-droite persistant avec chrono (parite avec l'analyse) : la
      # correction dure ~2-3 h, l'utilisateur doit voir qu'elle tourne.
      shiny::showNotification(
        .running_notif_content(i18n$t("acc_correct_running"), correct_start()),
        id = session$ns("correct_notif"), type = "message", duration = NULL)
      buffer_m <- .acc_buffer_m(input$buffer_m)
      tryCatch(
        correct_task$invoke(aoi_path, cache_dir, buffer_m, .dev_pkg_path,
                            get_app_options(), project_path),
        error = function(e) {
          bslib::update_task_button("correct_desserte", state = "ready")
          shiny::removeNotification(session$ns("correct_notif"))
          correct_start(NULL)
          rv_correct(list(status = "error", reason = "acc_correct_failed"))
        })
    })

    # Rafraichit le chrono du toast de correction toutes les secondes tant qu'elle
    # tourne (re-affiche la meme notif avec le temps ecoule mis a jour).
    shiny::observe({
      rc <- rv_correct()
      if (is.null(rc) || !identical(rc$status, "running")) return()
      shiny::invalidateLater(1000)
      shiny::showNotification(
        .running_notif_content(i18n$t("acc_correct_running"),
                               shiny::isolate(correct_start())),
        id = session$ns("correct_notif"), type = "message", duration = NULL)
    })

    shiny::observeEvent(correct_task$status(), {
      st <- correct_task$status()
      if (!identical(st, "success") && !identical(st, "error")) return()
      shiny::removeNotification(session$ns("correct_notif"))
      correct_start(NULL)
      session$sendCustomMessage("nemetonSetDisabled",       # re-active l'analyse
        list(id = session$ns("run"), disabled = FALSE))
      res <- tryCatch(correct_task$result(),
        error = function(e) list(status = "error", reason = "acc_correct_failed"))
      if (identical(st, "error") || !is.list(res)) {
        res <- list(status = "error", reason = "acc_correct_failed")
      }
      rv_correct(res)
      if (identical(res$status, "success")) {
        correct_refresh(correct_refresh() + 1L)   # la case " utiliser " apparait
        shiny::showNotification(
          sprintf(i18n$t("acc_correct_done_fmt"),
                  res$n_troncons %||% NA_integer_,
                  res$n_bdtopo %||% NA_integer_,
                  res$n_osm_ajoutes %||% 0L),
          type = "message", duration = 8)
        # OSM injoignable (bride Overpass) : la correction a bien eu lieu, mais
        # SANS complement. On le dit plutot que de laisser croire a un reseau
        # enrichi.
        if (!identical(res$osm_statut %||% "ok", "ok")) {
          shiny::showNotification(i18n$t("acc_correct_osm_indispo"),
                                  type = "warning", duration = 8)
        }
      } else {
        shiny::showNotification(i18n$t(res$reason %||% "acc_correct_failed"),
                                type = "error", duration = 8)
      }
    })

    output$correct_status <- shiny::renderUI({
      res <- rv_correct()
      if (is.null(res) || identical(res$status, "running")) return(NULL)
      if (identical(res$status, "success")) {
        htmltools::div(class = "alert alert-success py-1 px-2 my-1 small",
          role = "status", shiny::icon("check-circle"), " ",
          sprintf(i18n$t("acc_correct_done_fmt"),
                  res$n_troncons %||% NA_integer_,
                  res$n_bdtopo %||% NA_integer_,
                  res$n_osm_ajoutes %||% 0L))
      } else {
        htmltools::div(class = "alert alert-warning py-1 px-2 my-1 small",
          role = "status", shiny::icon("triangle-exclamation"), " ",
          i18n$t(res$reason %||% "acc_correct_failed"))
      }
    })

    output$use_corrected_ui <- shiny::renderUI({
      if (!isTRUE(corrected_available())) {
        return(htmltools::tags$p(class = "text-muted small mb-0",
                                 i18n$t("acc_correct_none")))
      }
      shiny::checkboxInput(ns("use_corrected"), i18n$t("acc_use_corrected"),
                           value = TRUE)
    })

    # --- Lancement -------------------------------------------------------------
    shiny::observeEvent(input$run, {
      if (isTRUE(rv$running)) {
        shiny::showNotification(i18n$t("acc_busy_already"), type = "warning",
                                duration = 5)
        return()
      }
      # Une correction LiDAR en cours mobilise deja un worker (lourd) : interdire
      # de lancer l'analyse par-dessus (double charge memoire, et l'analyse
      # n'utiliserait pas la correction en cours). Le bouton est aussi grise.
      if (identical(correct_task$status(), "running")) {
        bslib::update_task_button("run", state = "ready")
        shiny::showNotification(i18n$t("acc_correct_busy"), type = "warning",
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
        shiny::showNotification(i18n$t("acc_need_project"), type = "warning")
        return()
      }
      engines <- intersect(input$engines %||% character(0), ACCESSIBILITY_ENGINES)
      if (length(engines) == 0L) {
        bslib::update_task_button("run", state = "ready")
        shiny::showNotification(i18n$t("acc_need_engine"), type = "warning")
        return()
      }
      # L'AOI est passee au worker `future` PAR FICHIER, jamais comme `sf` vivant :
      # une geometrie sf peut porter un pointeur externe qui casse la
      # serialisation inter-process ("external pointer is not valid"). On l'ecrit
      # ici (process principal, pointeur valide) ; le worker la relit.
      cache_dir <- .accessibility_cache_dir(project_path)
      dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
      aoi_path <- file.path(cache_dir, "aoi_input.gpkg")
      ok <- tryCatch({
        sf::st_write(aoi, aoi_path, layer = "foret", quiet = TRUE,
                     delete_dsn = TRUE)
        TRUE
      }, error = function(e) FALSE)
      if (!isTRUE(ok)) {
        bslib::update_task_button("run", state = "ready")
        shiny::showNotification(i18n$t("acc_need_project"), type = "warning")
        return()
      }

      rv$running <- TRUE
      rv$start <- Sys.time()
      # Grise " Corriger la desserte " pendant l'analyse (reactive a la fin).
      session$sendCustomMessage("nemetonSetDisabled",
        list(id = session$ns("correct_desserte"), disabled = TRUE))
      shiny::showNotification(
        .running_notif_content(i18n$t("acc_running"), rv$start),
        id = session$ns("acc_notif"), type = "message", duration = NULL)
      # Garde-fou : un echec SYNCHRONE d'invoke (serialisation d'un argument) ne
      # doit pas laisser le bouton fige " busy " ni la notif collee.
      buffer_m <- .acc_buffer_m(input$buffer_m)
      use_corrected <- isTRUE(input$use_corrected)
      tryCatch(
        acc_task$invoke(aoi_path, engines, cache_dir, buffer_m,
                        .dev_pkg_path, get_app_options(),
                        use_corrected, project_path),
        error = function(e) {
          rv$running <- FALSE
          rv$start <- NULL
          shiny::removeNotification(session$ns("acc_notif"))
          bslib::update_task_button("run", state = "ready")
          session$sendCustomMessage("nemetonSetDisabled",
            list(id = session$ns("correct_desserte"), disabled = FALSE))
          shiny::showNotification(
            paste0(i18n$t("accessibility_engine_failed"), " \u2014 ",
                   .strip_ansi(conditionMessage(e))),
            type = "error", duration = NULL)
        })
    })

    # Rafraichit le chrono de la notif persistante tant que le worker tourne.
    shiny::observe({
      if (!isTRUE(rv$running)) return()
      shiny::invalidateLater(1000)
      shiny::showNotification(
        .running_notif_content(i18n$t("acc_running"), shiny::isolate(rv$start)),
        id = session$ns("acc_notif"), type = "message", duration = NULL)
    })

    # Message inline SOUS le bouton (roue dentee + chrono MM:SS), parite
    # FAST/FORDEAD/RECONFORT. Meme source (`rv$start`) que le toast bas-droite :
    # disparait tout seul en fin de run (rv$start remis a NULL). suspendWhenHidden
    # = FALSE pour que le chrono continue si l'onglet n'est pas au premier plan.
    output$run_status <- shiny::renderUI({
      if (!isTRUE(rv$running) || is.null(rv$start)) return(NULL)
      shiny::invalidateLater(1000)
      htmltools::div(
        class = "small text-info mt-1 text-center",
        .running_notif_content(i18n$t("acc_running"), rv$start))
    })
    shiny::outputOptions(output, "run_status", suspendWhenHidden = FALSE)

    # --- Fin de tache ----------------------------------------------------------
    shiny::observeEvent(acc_task$status(), {
      st <- acc_task$status()
      if (!identical(st, "success") && !identical(st, "error")) return()
      rv$running <- FALSE
      rv$start <- NULL
      session$sendCustomMessage("nemetonSetDisabled",
            list(id = session$ns("correct_desserte"), disabled = FALSE))
      shiny::removeNotification(session$ns("acc_notif"))

      res <- tryCatch(acc_task$result(), error = function(e) {
        list(status = "error", reason = "accessibility_engine_failed",
             detail = conditionMessage(e))
      })
      if (!is.list(res) || !identical(res$status, "success")) {
        reason <- tryCatch(res$reason, error = function(e) NULL) %||%
          "accessibility_engine_failed"
        msg <- i18n$t(reason)
        detail <- tryCatch(res$detail, error = function(e) NULL)
        if (!is.null(detail) && nzchar(detail)) {
          msg <- paste0(msg, " \u2014 ", .strip_ansi(as.character(detail)))
        }
        shiny::showNotification(msg, type = "error", duration = NULL)
        return()
      }
      # Recharger depuis le cache disque : le selecteur de couche liste ainsi
      # TOUS les rasters deja calcules du projet (tous moteurs + classes de
      # debardage), pas seulement ceux du run courant. Un run partiel (ex.
      # " porteur " seul) n'efface donc plus l'affichage des couches calculees
      # precedemment - elles restent selectionnables. Repli sur `res` si le
      # rechargement echoue.
      project_path <- tryCatch(app_state$current_project$path,
                               error = function(e) NULL)
      # Recharge l'union des rasters du cache, mais conserve la provenance DFCI
      # du run courant (`dfci_source`, non persistee sur disque) pour le badge.
      cached <- .load_cached_accessibility(project_path)
      if (!is.null(cached)) {
        cached$dfci_source <- res$dfci_source
      }
      rv$result <- cached %||% res
      shiny::showNotification(
        sprintf(i18n$t("acc_done_fmt"), length(res$engines)),
        type = "message", duration = 6)
    })

    # Restaure les rasters DEJA calcules depuis le cache disque - mais
    # PARESSEUSEMENT : le clic sur un projet recent doit rester rapide, donc on ne
    # lit le cache qu'au PREMIER affichage de l'onglet Accessibilite, une seule fois
    # par projet. Observer unique (main_nav + terrain_nav + projet) pour eviter toute
    # course d'ordre entre un reset et un chargement. `acc_loaded_for` memorise le
    # projet deja charge (chemin, "" si aucun).
    acc_loaded_for <- shiny::reactiveVal(NULL)
    shiny::observeEvent(
      list(app_state$active_main_tab, app_state$active_terrain_tab,
           app_state$current_project),
      {
        project_path <- tryCatch(app_state$current_project$path,
                                 error = function(e) NULL)
        key <- project_path %||% ""
        if (identical(acc_loaded_for(), key)) return()  # deja traite ce projet
        on_tab <- identical(app_state$active_main_tab, "terrain") &&
          identical(app_state$active_terrain_tab, "accessibility")
        if (!on_tab) {
          # Pas encore sur l'onglet : on efface l'ancien run (leger, pas d'IO) pour
          # ne pas montrer un resultat perime, sans charger le cache du nouveau.
          rv$result <- NULL
          return()
        }
        acc_loaded_for(key)
        cached <- tryCatch(.load_cached_accessibility(project_path),
                           error = function(e) NULL)
        rv$result <- cached
        if (!is.null(cached)) {
          shiny::showNotification(
            sprintf(i18n$t("acc_cache_loaded_fmt"), length(cached$raster_paths)),
            type = "message", duration = 5)
        }
      }, ignoreNULL = FALSE)

    # --- Selecteur de couche (raster affiche) : rendu apres un run -------------
    # Les choix sont les rasters disponibles : un par moteur (leurs classes
    # d'accessibilite) + " Classes de debardage " (bandes de distance Sylvaccess)
    # quand le skidder a tourne.
    output$layer_ui <- shiny::renderUI({
      res <- rv$result
      layers <- if (is.null(res)) NULL else names(res$raster_paths)
      # " Desserte BD TOPO / corrigee " : entree PSEUDO-couche du meme selecteur,
      # sur le modele de " Classes de debardage/ACCESSFOR " - la cocher active le
      # volet comparateur (fond relief + swipe). Disponible des qu'une correction
      # LiDAR existe, meme sans run moteur.
      compare_ok <- isTRUE(corrected_available())
      if ((is.null(layers) || length(layers) == 0L) && !compare_ok) {
        return(htmltools::tags$p(class = "text-muted small",
                                 i18n$t("acc_no_result_yet")))
      }
      # La couche " classes de debardage " se double d'ACCESSFOR (IGN) sous un volet
      # des que le raster ACCESSFOR est disponible (validation systematique) : le
      # libelle le reflete, sinon on garde " Classes de debardage " seul.
      has_accessfor <- {
        afp <- tryCatch(res$accessfor_raster_path, error = function(e) NULL)
        !is.null(afp) && file.exists(afp)
      }
      lyr_label <- c(
        skidder = i18n$t("acc_engine_skidder"),
        porteur = i18n$t("acc_engine_porteur"),
        camion_dfci = i18n$t("acc_engine_dfci"),
        cable = i18n$t("acc_engine_cable"),
        classes_debardage = if (has_accessfor)
          i18n$t("acc_layer_debardage_accessfor") else i18n$t("acc_layer_debardage"))
      labs <- unname(lyr_label[layers])
      labs[is.na(labs)] <- layers[is.na(labs)]
      choices <- stats::setNames(layers, labs)
      if (compare_ok) {
        choices <- c(choices,
                     stats::setNames("desserte_comparee",
                                     i18n$t("acc_layer_desserte_comparee")))
      }
      # Chaque entree porte le " i " de l'app, decrivant ce que le raster
      # montre. `info_popover_in_label()` et non `info_popover()` : le " i " vit
      # dans le <label> d'un radio, ou un clic selectionnerait la couche - et
      # selectionner ici coute une lecture de raster (cf. son roxygen).
      noms <- lapply(seq_along(choices), function(k) {
        info <- .acc_layer_info(unname(choices)[k], i18n)
        if (is.null(info)) return(names(choices)[k])
        htmltools::tagList(names(choices)[k], " ",
                           info_popover_in_label(info, placement = "left"))
      })
      shiny::radioButtons(
        ns("layer"), NULL,
        choiceNames  = noms,
        choiceValues = unname(choices),
        selected = if (length(layers) > 0L) layers[[1]] else "desserte_comparee")
    })

    # --- Badge de provenance DFCI (au-dessus de la carte) ----------------------
    # N'apparait que si la couche " Camion DFCI " est affichee. Avertissement
    # (jaune) quand les sources DFCI sont estimees par l'heuristique app
    # (aucune desserte taguee ref:FR:DFCI, ni repli geometrique) ; info (bleu)
    # quand le vrai reseau OSM ref:FR:DFCI a servi.
    output$dfci_badge <- shiny::renderUI({
      res <- rv$result
      layer <- input$layer %||%
        (if (!is.null(res)) names(res$raster_paths)[[1]] else NULL)
      if (is.null(res) || !identical(layer, "camion_dfci")) return(NULL)
      src <- tryCatch(res$dfci_source, error = function(e) NULL)
      if (identical(src, "heuristique")) {
        shiny::div(class = "alert alert-warning acc-dfci-badge py-2 mb-2 small",
          role = "status",
          shiny::icon("triangle-exclamation"), " ",
          i18n$t("acc_dfci_heuristic_badge"))
      } else if (identical(src, "osm")) {
        shiny::div(class = "alert alert-info acc-dfci-badge py-2 mb-2 small",
          role = "status",
          shiny::icon("circle-info"), " ", i18n$t("acc_dfci_osm_badge"))
      } else {
        NULL
      }
    })

    # --- Badge de provenance de la DESSERTE (au-dessus de la carte) -------------
    # Reflete la desserte UTILISEE par le run moteur courant : bleu = desserte
    # corrigee LiDAR (case " utiliser " cochee + fichier present), jaune = corrigee
    # demandee mais absente -> repli brut. Rien si desserte corrigee non demandee.
    output$desserte_badge <- shiny::renderUI({
      res <- rv$result
      src <- tryCatch(res$desserte_source, error = function(e) NULL)
      if (is.null(src) || is.na(src)) return(NULL)
      if (identical(src, "ndp1_lidar")) {
        nd <- tryCatch(res$n_departs, error = function(e) NA_integer_)
        extra_txt <- if (isTRUE(is.finite(nd)))
          paste0(" ", sprintf(i18n$t("acc_cable_departs_fmt"), nd)) else ""
        shiny::div(class = "alert alert-info py-2 mb-2 small", role = "status",
          shiny::icon("circle-info"), " ", i18n$t("acc_desserte_ndp1_badge"), extra_txt)
      } else {
        shiny::div(class = "alert alert-warning py-2 mb-2 small", role = "status",
          shiny::icon("triangle-exclamation"), " ", i18n$t("acc_desserte_ndp0_badge"))
      }
    })

    # --- Carte : fonds + UGF + raster de classes en overlay --------------------
    # Meme patron que la Carte " Alertes FAST " (mod_monitoring_fast_alerts) : la
    # carte de base (tuiles OSM/Satellite + UGF + fitBounds) est rendue UNE seule
    # fois ; le raster est ajoute/mis a jour via leafletProxy dans l'observe plus
    # bas. Le raster vit dans un MAP PANE dedie (zIndex fixe) : sans lui, changer
    # de fond (OSM <-> Satellite) fait disparaitre l'image raster et perd le zoom /
    # l'opacite. Le groupe " Accessibilite " est enregistre dans le LayersControl
    # des le rendu (l'utilisateur peut le decocher).
    output$map <- leaflet::renderLeaflet({
      aoi <- units_sf()
      geo <- if (!is.null(aoi)) {
        tryCatch(sf::st_transform(aoi, 4326), error = function(e) NULL)
      }
      # " Desserte " (routes/pistes DFCI ayant servi au calcul) est enregistree
      # dans le LayersControl SOUS " UGF " et " Accessibilite " ; l'observe plus
      # bas la dessine via leafletProxy (depend du run, pas de la carte de base).
      # Fond relief CVAT (overlay semi-transparent au-dessus d'OSM/Satellite),
      # propose dans le LayersControl quand un CVAT existe deja pour le projet.
      project_path <- tryCatch(app_state$current_project$path, error = function(e) NULL)
      cvat_bg <- .acc_cvat_overlay_raster(project_path)
      # Le relief est declare SYSTEMATIQUEMENT, meme sans CVAT pret a cet
      # instant : le comparateur de desserte peut en peindre un plus tard (son
      # worker async), et sans case declaree ici ce relief-la serait
      # inextinguible. Une case sans couche ne fait rien, c'est sans risque.
      # Meme raison pour les deux couches du comparateur : decocher la couche
      # corrigee est precisement la facon de lire ce qu'elle a change par
      # rapport a la BD TOPO en dessous. Elles se declarent donc ici, et non
      # au moment ou le comparateur les peint.
      overlays <- c(if (!is.null(geo)) "UGF" else NULL,
                    ACC_RELIEF_GROUP,
                    ACC_ACCESSIBILITE_GROUP, "Desserte", PLACES_DEPOT_GROUP,
                    ACC_DESSERTE_ORIG_GROUP, ACC_DESSERTE_CORR_GROUP)
      m <- leaflet::leaflet() |>
        leaflet::addProviderTiles("OpenStreetMap", group = "OSM") |>
        leaflet::addProviderTiles("Esri.WorldImagery", group = "Satellite") |>
        # Fond relief : juste au-dessus des tuiles (200), sous les donnees.
        leaflet::addMapPane("nemetonCvatBase", zIndex = 230) |>
        leaflet::addMapPane("nemetonAccRaster", zIndex = 250) |>
        # Panes gauche/droite pour la comparaison " swipe " ACCESSFOR : le volet
        # (nemeton_swipe.js) les clippe de part et d'autre du curseur.
        leaflet::addMapPane("nemetonAccSwipeL", zIndex = 250) |>
        leaflet::addMapPane("nemetonAccSwipeR", zIndex = 250) |>
        # Comparateur desserte (carte UNIQUE, plus de volet) : fond RVT sous les
        # dessertes, puis la BD TOPO, puis le statut de correction LiDAR par-dessus.
        leaflet::addMapPane("nemetonRvtFond", zIndex = 245) |>
        leaflet::addMapPane("nemetonDessBase", zIndex = 420) |>
        leaflet::addMapPane("nemetonDessCorr", zIndex = 430) |>
        leaflet::addLayersControl(
          baseGroups = c("OSM", "Satellite"),
          overlayGroups = overlays,
          options = leaflet::layersControlOptions(collapsed = TRUE))
      if (!is.null(cvat_bg)) {
        grey <- leaflet::colorNumeric(grDevices::grey.colors(64, 0, 1),
          domain = c(0, 1), na.color = "transparent")
        m <- leaflet::addRasterImage(m, cvat_bg, colors = grey, opacity = 0.6,
          group = ACC_RELIEF_GROUP, maxBytes = 16 * 1024^2,
          options = leaflet::gridOptions(pane = "nemetonCvatBase"))
      }
      if (!is.null(geo)) {
        m <- leaflet::addPolygons(m, data = geo, group = "UGF",
          color = "#1f78b4", weight = 2, opacity = 0.9, fillOpacity = 0)
        bb <- tryCatch(as.numeric(sf::st_bbox(geo)), error = function(e) NULL)
        if (!is.null(bb) && all(is.finite(bb))) {
          m <- leaflet::fitBounds(m, bb[1], bb[2], bb[3], bb[4])
        }
      }
      # Enregistre l'objet carte Leaflet pour nemeton_swipe.js (repli
      # `window.nemetonMaps[id]`, cf. findMap() du script).
      htmlwidgets::onRender(m, "function(el, x) {
        window.nemetonMaps = window.nemetonMaps || {};
        window.nemetonMaps[el.id] = this;
      }")
    })
    shiny::outputOptions(output, "map", suspendWhenHidden = FALSE)

    # Opacite du raster affiche : debouncee pour ne pas redessiner a chaque tick
    # du slider pendant un glissement.
    opacity_d <- shiny::debounce(
      shiny::reactive(suppressWarnings(as.numeric(input$opacity)) %||% 0.7), 250)

    # Overlay du raster de classes via leafletProxy : preserve le zoom et le fond
    # selectionne (OSM/Satellite). Le raster est peint dans le pane dedie
    # `nemetonAccRaster` (cf. renderLeaflet) - c'est ce qui le rend stable au
    # changement de fond. `method = "ngb"` : pas d'interpolation des codes de
    # classe (raster categoriel).
    #
    # Cas special " classes de debardage " : des qu'ACCESSFOR (IGN) est disponible
    # (validation systematique au run), cette couche s'affiche EN VIS-A-VIS sous un
    # volet vertical draggable - gauche = nos classes, droite = ACCESSFOR (memes
    # coltab + emprise -> lecture directe de l'ecart). Le volet (nemeton_swipe.js)
    # n'est (re)active qu'a l'ENTREE en mode volet (pas a chaque re-dessin), sinon
    # un changement d'opacite recentrerait le curseur.
    swipe_active <- shiny::reactiveVal(FALSE)
    # Le comparateur desserte (plus bas) est un MODE swipe distinct qui prend la
    # main : quand il est actif, le swipe ACCESSFOR se retire (un seul volet par
    # carte cote nemeton_swipe.js).
    compare_active <- shiny::reactiveVal(FALSE)
    shiny::observe({
      res <- rv$result
      first_layer <- if (!is.null(res)) names(res$raster_paths)[[1]] else NULL
      layer <- input$layer %||% first_layer
      op <- opacity_d()
      # Groupes overlay coches cote client, lus SANS creer de dependance :
      # leaflet renvoie `input$<id>_groups` a chaque ajout/retrait de groupe -
      # or cet observe en ajoute et en retire. Sans `isolate()`, chaque peinture
      # se re-declenche elle-meme (et declenche les autres observes de la carte,
      # qui partagent le meme input) : le raster se redessine plusieurs fois
      # avant de se stabiliser. La decoche utilisateur, elle, est geree
      # directement par le LayersControl cote client - pas besoin de repeindre.
      shown <- shiny::isolate(input$map_groups)
      mapid <- session$ns("map")
      proxy <- leaflet::leafletProxy("map") |>
        leaflet::clearGroup(ACC_ACCESSIBILITE_GROUP) |>
        leaflet::removeControl("acc_legend")
      # Pseudo-couche comparateur selectionnee : on ne peint pas le raster de
      # classes (il revient au changement de couche). Le comparateur desserte
      # n'utilise PLUS de volet : il faut donc RETIRER celui d'ACCESSFOR s'il
      # etait actif, sinon le curseur resterait affiche par-dessus la carte.
      if (identical(layer, "desserte_comparee")) {
        if (isTRUE(swipe_active())) {
          session$sendCustomMessage("nemetonSwipeOff", list(id = mapid))
          swipe_active(FALSE)
        }
        return()
      }
      if (is.null(res) || is.null(layer)) {
        if (isTRUE(swipe_active())) {
          session$sendCustomMessage("nemetonSwipeOff", list(id = mapid))
          swipe_active(FALSE)
        }
        return()
      }
      accessfor_rp <- tryCatch(res$accessfor_raster_path, error = function(e) NULL)
      swipe_mode <- identical(layer, "classes_debardage") &&
        !is.null(accessfor_rp) && file.exists(accessfor_rp)
      if (swipe_mode) {
        left_rp <- tryCatch(res$raster_paths[["classes_debardage"]],
                            error = function(e) NULL)
        proxy <- .acc_paint_raster(proxy, left_rp, "nemetonAccSwipeL",
          ACC_ACCESSIBILITE_GROUP, op, i18n, legend_id = "acc_legend")
        .acc_paint_raster(proxy, accessfor_rp, "nemetonAccSwipeR",
          ACC_ACCESSIBILITE_GROUP, op, i18n, legend_id = NULL)
        if (!isTRUE(swipe_active())) {
          session$sendCustomMessage("nemetonSwipeOn", list(
            id = mapid, left = "nemetonAccSwipeL", right = "nemetonAccSwipeR"))
          swipe_active(TRUE)
        }
      } else {
        if (isTRUE(swipe_active())) {
          session$sendCustomMessage("nemetonSwipeOff", list(id = mapid))
          swipe_active(FALSE)
        }
        rp <- tryCatch(res$raster_paths[[layer]], error = function(e) NULL)
        proxy <- .acc_paint_raster(proxy, rp, "nemetonAccRaster", ACC_ACCESSIBILITE_GROUP,
                                   op, i18n, legend_id = "acc_legend")
      }
      # Respecter la decoche du groupe " Accessibilite " apres re-dessin proxy.
      if (!is.null(shown) && !(ACC_ACCESSIBILITE_GROUP %in% shown)) {
        leaflet::hideGroup(proxy, ACC_ACCESSIBILITE_GROUP)
      }
    })

    # Overlay " Desserte " : les routes/pistes (sources DFCI) qui ont servi au
    # calcul, lues depuis la couche `desserte` du GeoPackage du run. Depend de
    # `rv$result` uniquement (pas de l'opacite ni du raster choisi) -> observe
    # dedie. Polylignes colorees par classe (route/piste), au-dessus du raster.
    shiny::observe({
      res <- rv$result
      shown <- shiny::isolate(input$map_groups)   # cf. observe raster : isolate
      proxy <- leaflet::leafletProxy("map") |> leaflet::clearGroup("Desserte")

      # Pas de desserte du RUN tant que le comparateur est selectionne : il
      # montre les memes troncons, en plus precis (classe BD TOPO + statut de
      # correction), et avec une AUTRE palette - superposer les deux mettrait a
      # l'ecran des troncons de meme couleur qui ne veulent pas dire la meme
      # chose, sans qu'aucune legende ne le signale. La lecture reactive de
      # `input$layer` est voulue : quitter le comparateur doit la faire revenir.
      if (identical(input$layer, "desserte_comparee")) return()

      gp <- tryCatch(res$gpkg_path, error = function(e) NULL)
      if (is.null(gp) || !file.exists(gp)) return()
      d <- tryCatch(sf::st_read(gp, layer = "desserte", quiet = TRUE),
                    error = function(e) NULL)
      if (!inherits(d, "sf") || nrow(d) == 0L) return()
      d <- tryCatch(sf::st_transform(d, 4326), error = function(e) d)
      cl <- tolower(as.character(d[["classe"]] %||% rep("", nrow(d))))
      cols <- ifelse(cl == "piste", "#8D6E63", "#37474F")   # piste brun / route gris
      proxy |>
        leaflet::addPolylines(data = d, group = "Desserte",
          color = cols, weight = 2, opacity = 0.9,
          label = ~ as.character(classe))
      # Respecter la decoche du groupe " Desserte " apres re-dessin proxy.
      if (!is.null(shown) && !("Desserte" %in% shown)) {
        leaflet::hideGroup(proxy, "Desserte")
      }
    })

    # Overlay " Places de depot " : les places de depot calculees par
    # `places_depot()` le long de la desserte (corrigee au LiDAR en NDP 1, cable),
    # lues depuis la couche `places_depot` du GeoPackage du run. Marqueurs (points
    # rouges) au-dessus du raster et de la desserte.
    shiny::observe({
      res <- rv$result
      shown <- shiny::isolate(input$map_groups)   # cf. observe raster : isolate
      proxy <- leaflet::leafletProxy("map") |>
        leaflet::clearGroup(PLACES_DEPOT_GROUP)
      gp <- tryCatch(res$gpkg_path, error = function(e) NULL)
      pd <- .acc_read_places_depot(gp)
      if (is.null(pd)) return()
      proxy |>
        leaflet::addCircleMarkers(data = pd, group = PLACES_DEPOT_GROUP,
          radius = 5, color = "#B71C1C", weight = 1, fillColor = "#E53935",
          fillOpacity = 0.85, label = i18n$t("acc_places_depot"))
      if (!is.null(shown) && !(PLACES_DEPOT_GROUP %in% shown)) {
        leaflet::hideGroup(proxy, PLACES_DEPOT_GROUP)
      }
    })

    # --- Comparateur swipe : desserte BD TOPO vs corrigee sur fond relief RVT ---
    # Pilote par la pseudo-couche `desserte_comparee` du selecteur (comme le volet
    # ACCESSFOR l'est par `classes_debardage`). L'aide contextuelle ci-dessous ne
    # s'affiche que lorsque cette couche est selectionnee.
    output$compare_hint_ui <- shiny::renderUI({
      if (!identical(input$layer, "desserte_comparee")) return(NULL)
      eng <- rvt_engine()
      relief_lbl <- switch(eng,
        cvat = i18n$t("acc_compare_relief_cvat"),
        vat  = i18n$t("acc_compare_relief_vat"),
        i18n$t("acc_compare_relief_hillshade"))
      htmltools::tags$p(class = "text-muted small mb-0 mt-1",
        sprintf("%s %s", i18n$t("acc_compare_hint"), relief_lbl))
    })

    # Peint le fond relief RVT (raster gris [0,1]) dans son pane non clippe.
    # Partage par le chemin synchrone (cache) et l'observe async (rvt_task).
    .paint_rvt_fond <- function(rvt_path) {
      if (is.null(rvt_path) || !file.exists(rvt_path)) return(invisible())
      rr <- tryCatch(terra::rast(rvt_path), error = function(e) NULL)
      if (is.null(rr)) return(invisible())
      # `addRasterImage` plafonne a 4 Mo : un CVAT sur MNT LiDAR 0,5 m
      # (4000x4000 = 16 M cellules -> ~17 Mo) le depasse et fait planter l'observe.
      # Le relief n'a pas besoin de 0,5 m a l'ecran : on agrege a ~2000 px de cote
      # (facteur entier) avant l'affichage. Filet `maxBytes` en complement.
      maxdim <- max(terra::nrow(rr), terra::ncol(rr))
      if (is.finite(maxdim) && maxdim > 2000L) {
        fact <- as.integer(ceiling(maxdim / 2000))
        rr <- tryCatch(terra::aggregate(rr, fact = fact, fun = "mean",
                                        na.rm = TRUE), error = function(e) rr)
      }
      grey <- leaflet::colorNumeric(grDevices::grey.colors(64, 0, 1),
        domain = c(0, 1), na.color = "transparent")
      proxy <- leaflet::leafletProxy("map") |>
        leaflet::clearGroup(ACC_RELIEF_GROUP) |>
        leaflet::addRasterImage(rr, colors = grey, opacity = 1,
          group = ACC_RELIEF_GROUP, maxBytes = 16 * 1024^2,
          options = leaflet::gridOptions(pane = "nemetonRvtFond"))
      # La case du LayersControl doit rester respectee : re-peindre un groupe
      # decoche ne doit pas le re-afficher. `isolate()` obligatoire - cet appel
      # vit dans un observe qui AJOUTE des groupes, une lecture reactive le
      # rendrait auto-declenchant (cf. v0.122.3/0.122.4).
      shown <- shiny::isolate(input$map_groups)
      if (!is.null(shown) && !(ACC_RELIEF_GROUP %in% shown)) {
        leaflet::hideGroup(proxy, ACC_RELIEF_GROUP)
      }
      invisible()
    }

    # Fond RVT calcule en async (cas couteux) : peint quand le worker rend le
    # chemin, si le comparateur est toujours actif.
    # Pilote par le STATUT et non par le resultat : un worker en echec doit lui
    # aussi liberer `rvt_pending`, sinon plus aucun calcul de relief ne serait
    # retente pour ce MNT de toute la session.
    shiny::observeEvent(rvt_task$status(), {
      st <- rvt_task$status()
      if (!identical(st, "success") && !identical(st, "error")) return()
      shiny::removeNotification(session$ns("rvt_notif"))
      mnt <- rvt_pending()
      rvt_pending(NULL)
      out <- if (identical(st, "success")) {
        tryCatch(rvt_task$result(), error = function(e) NULL)
      }
      if (is.null(out)) return()
      rvt_ready(list(mnt = mnt, out = out))
      if (isTRUE(compare_active())) .paint_rvt_fond(out)
    })

    # Peinture du comparateur, en CARTE UNIQUE (plus de volet swipe) : fond RVT,
    # puis la desserte BD TOPO coloree par CLASSE, puis par-dessus le reseau
    # corrige colore par SOURCE. Desactive -> nettoyage.
    #
    # `qualifier_desserte()` n'ajoute rien par elle-meme : elle RENSEIGNE (etat,
    # largeur, geometrie recalee). Ce qui ajoute, c'est le complement OSM - et
    # demain la detection LiDAR. D'ou trois sources et non deux, et d'ou le fait
    # que la couche corrigee contienne TOUJOURS au moins toute la BD TOPO.
    shiny::observe({
      on <- identical(input$layer, "desserte_comparee") &&
        isTRUE(corrected_available())
      # Pas de lecture de `input$map_groups` ici : elle ne servait a rien (aucun
      # `hideGroup` pour les groupes du comparateur) et rendait cet observe
      # auto-declenchant - le fond relief etait recalcule et repeint plusieurs
      # fois avant de se stabiliser.
      project_path <- tryCatch(app_state$current_project$path, error = function(e) NULL)
      proxy <- leaflet::leafletProxy("map") |>
        leaflet::clearGroup(ACC_RELIEF_GROUP) |>
        leaflet::clearGroup(ACC_DESSERTE_ORIG_GROUP) |>
        leaflet::clearGroup(ACC_DESSERTE_CORR_GROUP) |>
        leaflet::removeControl("cmp_legend_l") |>
        leaflet::removeControl("cmp_legend_r")

      if (!on) {
        shiny::removeNotification(session$ns("rvt_notif"))
        if (isTRUE(compare_active())) compare_active(FALSE)
        # Le relief est un groupe PARTAGE depuis que la case le pilote : on vient
        # d'effacer celui du comparateur (opaque), il faut donc rendre le fond
        # semi-transparent de la carte de base, sinon quitter le comparateur
        # laisserait la carte sans relief jusqu'au prochain re-rendu complet.
        cvat_bg <- .acc_cvat_overlay_raster(project_path)
        if (!is.null(cvat_bg)) {
          grey <- leaflet::colorNumeric(grDevices::grey.colors(64, 0, 1),
            domain = c(0, 1), na.color = "transparent")
          proxy <- leaflet::addRasterImage(proxy, cvat_bg, colors = grey,
            opacity = 0.6, group = ACC_RELIEF_GROUP, maxBytes = 16 * 1024^2,
            options = leaflet::gridOptions(pane = "nemetonCvatBase"))
          shown <- shiny::isolate(input$map_groups)
          if (!is.null(shown) && !(ACC_RELIEF_GROUP %in% shown)) {
            leaflet::hideGroup(proxy, ACC_RELIEF_GROUP)
          }
        }
        return()
      }

      # Fond relief RVT. Peu couteux (cache / CVAT pre-calcule) -> synchrone ;
      # sinon (calcul vat_combined ~1 min) -> worker async, peint par l'observe
      # `rvt_task$result()` plus bas quand il arrive.
      mnt_path <- .acc_rvt_mnt_path(project_path)
      ready <- shiny::isolate(rvt_ready())
      if (!is.null(mnt_path)) {
        if (!is.null(ready) && identical(ready$mnt, mnt_path)) {
          # Fond deja calcule pour ce MNT : on le repeint sans relancer le worker.
          .paint_rvt_fond(ready$out)
        } else if (.rvt_is_cheap(mnt_path)) {
          out <- tryCatch(generate_rvt(mnt_path), error = function(e) NULL)
          # Memo sur SUCCES seulement : un echec transitoire doit pouvoir etre
          # retente a la prochaine entree dans le comparateur.
          if (!is.null(out)) rvt_ready(list(mnt = mnt_path, out = out))
          .paint_rvt_fond(out)
        } else if (!identical(shiny::isolate(rvt_pending()), mnt_path)) {
          # Un seul worker en vol par MNT : sans cette garde, revenir sur le
          # comparateur relancerait un calcul de ~1 min deja en cours.
          shiny::showNotification(i18n$t("acc_compare_building_relief"),
            duration = NULL, type = "message", id = session$ns("rvt_notif"))
          rvt_pending(mnt_path)
          rvt_task$invoke(mnt_path, .dev_pkg_path, get_app_options())
        }
      }

      corrected_path <- .corrected_desserte_path(.accessibility_cache_dir(project_path))

      # FOND : desserte BD TOPO complete, coloree par classe. `hors_desserte` en
      # pointille gris - present depuis foretaccess 2.0.0 (conserve pour la
      # topologie), il n'entre pas dans le debardage et ne doit pas se lire comme
      # une desserte utilisable.
      dorig <- .acc_read_desserte_layer(corrected_path, "desserte_origine")
      classes_vues <- character()
      if (!is.null(dorig)) {
        cl <- tolower(as.character(dorig[["classe"]] %||% rep("", nrow(dorig))))
        classes_vues <- intersect(names(DESS_CLASSE_COLS), unique(cl))
        # Deux passes : `dashArray` n'est pas vectorisable par troncon.
        hors <- cl == "hors_desserte"
        if (any(!hors)) {
          d_util <- dorig[!hors, , drop = FALSE]
          # Repli sur un gris neutre pour une classe inconnue : foretaccess a
          # deja ajoute `hors_desserte` en 2.0.0, une couleur NA casserait le
          # rendu de TOUTE la couche plutot que du seul troncon fautif.
          col_util <- unname(DESS_CLASSE_COLS[cl[!hors]])
          col_util[is.na(col_util)] <- "#9E9E9E"
          proxy <- leaflet::addPolylines(proxy, data = d_util,
            group = ACC_DESSERTE_ORIG_GROUP, weight = 2, opacity = 0.95,
            color = col_util,
            options = leaflet::pathOptions(pane = "nemetonDessBase"),
            label = ~ as.character(classe))
        }
        if (any(hors)) {
          proxy <- leaflet::addPolylines(proxy, data = dorig[hors, , drop = FALSE],
            group = ACC_DESSERTE_ORIG_GROUP, color = unname(DESS_CLASSE_COLS[["hors_desserte"]]),
            weight = 1.5, opacity = 0.6, dashArray = "4,6",
            options = leaflet::pathOptions(pane = "nemetonDessBase"),
            label = i18n$t("acc_desserte_hors"))
        }
      }

      # SURCHARGE : statut de correction LiDAR, par-dessus la BD TOPO. Le bilan
      # n'est PAS persiste dans le gpkg -> le critere mesure/non-mesure reste
      # `is.na(etat_classe)`. L'infobulle porte les mesures utiles au terrain.
      dcorr <- .acc_read_desserte_layer(corrected_path, "desserte_corrigee")
      sources_vues <- character()
      if (!is.null(dcorr)) {
        # Plus AUCUN filtrage ici : la couche porte desormais l'integralite du
        # reseau. Un troncon non mesure se lit dans son infobulle, il ne
        # disparait pas de la carte.
        src <- tolower(as.character(dcorr[["source"]] %||% rep("bdtopo", nrow(dcorr))))
        src[!(src %in% names(DESS_SOURCE_COLS))] <- "bdtopo"
        sources_vues <- intersect(names(DESS_SOURCE_COLS), unique(src))
        etat <- as.character(dcorr[["etat_dessertr"]] %||% rep(NA_character_, nrow(dcorr)))
        larg <- suppressWarnings(as.numeric(dcorr[["largeur_carrossable_m"]]))
        # Etats traduits : ils sont desormais VISIBLES sur la carte, puisque
        # plus aucun troncon n'est retire.
        tr_etat <- c(en_service = i18n$t("acc_desserte_en_service"),
                     trouee_sans_route = i18n$t("acc_desserte_trouee"),
                     abandonnee = i18n$t("acc_desserte_abandonnee"),
                     hors_route = i18n$t("acc_desserte_sans_signal"))
        et <- unname(tr_etat[tolower(etat)])
        et[is.na(et)] <- etat[is.na(et)]
        lbl <- ifelse(
          is.na(etat), i18n$t("acc_compare_non_mesure"),
          ifelse(is.finite(larg), sprintf("%s \u2014 %.1f m", et, larg), et))
        proxy <- leaflet::addPolylines(proxy, data = dcorr,
          group = ACC_DESSERTE_CORR_GROUP, weight = 4, opacity = 0.95,
          color = unname(DESS_SOURCE_COLS[src]),
          options = leaflet::pathOptions(pane = "nemetonDessCorr"),
          label = lbl)
      }

      # Legende UNIQUE : classement des troncons BD TOPO, puis statut de
      # correction. Seules les modalites reellement presentes sont listees.
      lbl_classe <- c(route = i18n$t("acc_desserte_route"),
                      piste = i18n$t("acc_desserte_piste"),
                      reseau_public = i18n$t("acc_desserte_reseau_public"),
                      hors_desserte = i18n$t("acc_desserte_hors"))
      lbl_source <- c(bdtopo = i18n$t("acc_source_bdtopo"),
                      osm = i18n$t("acc_source_osm"),
                      detectee = i18n$t("acc_source_detectee"))
      if (length(classes_vues)) {
        proxy <- leaflet::addLegend(proxy, "bottomleft",
          colors = unname(DESS_CLASSE_COLS[classes_vues]),
          labels = unname(lbl_classe[classes_vues]),
          title = i18n$t("acc_compare_legend_origine"), layerId = "cmp_legend_l",
          opacity = 0.9)
      }
      if (length(sources_vues)) {
        proxy <- leaflet::addLegend(proxy, "bottomleft",
          colors = unname(DESS_SOURCE_COLS[sources_vues]),
          labels = unname(lbl_source[sources_vues]),
          title = i18n$t("acc_compare_legend_source"), layerId = "cmp_legend_r",
          opacity = 0.9)
      }

      # Les cases du LayersControl doivent survivre au re-dessin : on vient de
      # re-ajouter les deux groupes, ce qui les re-afficherait. `isolate()`
      # obligatoire - cet observe AJOUTE des groupes, une lecture reactive le
      # rendrait auto-declenchant (cf. v0.122.3/0.122.4).
      shown <- shiny::isolate(input$map_groups)
      if (!is.null(shown)) {
        for (g in c(ACC_DESSERTE_ORIG_GROUP, ACC_DESSERTE_CORR_GROUP)) {
          if (!(g %in% shown)) proxy <- leaflet::hideGroup(proxy, g)
        }
      }

      if (!isTRUE(compare_active())) compare_active(TRUE)
    })

    # --- Profil en travers au clic (spec 030) ----------------------------------
    # Un clic sur la carte, quand le comparateur est affiche, rend la coupe du
    # troncon le plus proche. Le calcul (lecture du nuage LiDAR, ajustement,
    # bords) appartient au coeur : `foretaccess::profil_travers()`, appele via
    # `acc_profil_travers()`. Ici, rien d'autre que l'orchestration.
    #
    # ASYNCHRONE malgre un cout mesure a ~0,4 s par clic cote coeur : ce chiffre
    # vaut sur une dalle d'exemple. Sur un projet reel, la premiere lecture d'un
    # catalogue LAZ est autrement plus lourde, et la boucle Shiny est
    # mono-thread - un clic ne doit jamais figer la carte.
    profil_task <- shiny::ExtendedTask$new(
      function(project_path, lng, lat, dev_path, app_opts) {
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
          utils::getFromNamespace("acc_profil_travers", "nemetonshiny")(
            project_path, lng, lat)
        }, seed = TRUE)
      })

    shiny::observeEvent(input$map_click, {
      # Le profil n'a de sens que sous le comparateur : c'est la seule couche qui
      # montre les troncons dont on coupe la section. Ailleurs, un clic sur la
      # carte ne veut pas dire cela.
      if (!identical(input$layer, "desserte_comparee")) return()
      clic <- input$map_click
      if (is.null(clic$lat) || is.null(clic$lng)) return()
      project_path <- tryCatch(app_state$current_project$path,
                               error = function(e) NULL)
      # Retour IMMEDIAT : le worker peut mettre plusieurs secondes, le clic doit
      # etre acquitte tout de suite (regle stricte 9).
      shiny::showNotification(i18n$t("profil_calcul"), duration = NULL,
                              type = "message", id = session$ns("profil_notif"))
      profil_task$invoke(project_path, clic$lng, clic$lat, .dev_pkg_path,
                         get_app_options())
    })

    shiny::observeEvent(profil_task$status(), {
      st <- profil_task$status()
      if (!identical(st, "success") && !identical(st, "error")) return()
      shiny::removeNotification(session$ns("profil_notif"))
      if (identical(st, "error")) {
        shiny::showNotification(i18n$t("profil_failed"), type = "error",
                                duration = 8)
        return()
      }
      res <- profil_task$result()
      # `status` porte la raison : l'absence de troncon sous le clic n'est pas
      # une panne, elle se dit autrement qu'une erreur de calcul.
      if (!is.list(res) || !identical(res$status, "success")) {
        cle <- sub("^acc_", "", res$reason %||% "acc_profil_failed")
        shiny::showNotification(
          i18n$t(cle), duration = 8,
          type = if (identical(res$status, "empty")) "warning" else "error")
        return()
      }
      rv$profil <- res
      shiny::showModal(shiny::modalDialog(
        title = i18n$t("profil_titre"),
        size = "xl", easyClose = TRUE,
        footer = shiny::modalButton(i18n$t("close")),
        htmltools::tags$p(
          class = "text-muted small",
          sprintf(i18n$t("profil_station_fmt"),
                  res$station$chainage_m %||% NA_real_,
                  as.integer(res$meta$n_points %||% 0L))),
        plotly::plotlyOutput(ns("profil_plot"), height = "60vh")))
    })

    output$profil_plot <- plotly::renderPlotly({
      p <- rv$profil
      shiny::req(p)
      plot_desserte_profil(p, i18n)
    })

    # Le rendu du tableau d'accord ACCESSFOR est SUPPRIME avec son panneau : il
    # n'etait pas utilise. Le raster ACCESSFOR reste calcule par le worker et
    # reste consultable en volet swipe via la couche " Classes de debardage /
    # ACCESSFOR (IGN) " du selecteur ; seul le tableau chiffre disparait.

    # --- Export GeoPackage -----------------------------------------------------
    output$export_gpkg <- shiny::downloadHandler(
      filename = function() {
        paste0(.project_export_slug(app_state$current_project, "nemeton"),
               "_accessibilite.gpkg")
      },
      content = function(file) {
        on.exit(session$sendCustomMessage("nemetonHideDownloadToast", list()),
                add = TRUE)
        res <- rv$result
        if (is.null(res) || !isTRUE(export_accessibility_geopackage(res, file))) {
          shiny::showNotification(i18n$t("acc_export_empty"), type = "warning")
          if (!file.exists(file)) writeLines("No data available", file)
        }
      }
    )
  })
}

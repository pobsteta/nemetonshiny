#' Internationalization (i18n) for nemetonApp
#'
#' @description
#' Translation system for the nemetonApp Shiny application.
#' Supports French (fr) and English (en).
#'
#' @name utils_i18n
#' @keywords internal
NULL


#' Translation dictionary
#'
#' @description
#' Complete translation dictionary for all UI strings.
#'
#' @noRd
TRANSLATIONS <- list(
  # ============================================================
  # Application
  # ============================================================
  app_title = list(
    fr = "N\u00e9m\u00e9ton - Diagnostic Forestier",
    en = "N\u00e9m\u00e9ton - Forest Diagnostic"
  ),

  # ============================================================
  # Navigation
  # ============================================================
  tab_selection = list(fr = "S\u00e9lection", en = "Selection"),
  tab_synthesis = list(fr = "Synth\u00e8se", en = "Synthesis"),
  tab_families = list(fr = "Familles d'indicateurs", en = "Indicator Families"),
  tab_carte_cadastrale = list(fr = "Carte cadastrale", en = "Cadastral map"),
  tab_carte_tenements = list(fr = "Carte UGF", en = "UGF Map"),
  tab_tableau_ug = list(fr = "Tableau UGF", en = "UGF Table"),
  tab_sampling = list(fr = "Terrain", en = "Field"),
  tab_terrain_export = list(fr = "Export terrain",   en = "Field export"),
  tab_terrain_import = list(fr = "Import terrain",   en = "Field import"),
  tab_action_plan = list(fr = "Plan d'actions", en = "Action Plan"),

  # ============================================================
  # Action Plan tab (S3 skeleton)
  # ============================================================
  action_plan_filters_title = list(fr = "Filtres", en = "Filters"),
  action_plan_horizon = list(fr = "Horizon (années)", en = "Horizon (years)"),
  action_plan_type = list(fr = "Type d'action", en = "Action type"),
  action_plan_statut = list(fr = "Statut", en = "Status"),
  action_plan_famille = list(fr = "Famille d'objectifs", en = "Objective family"),
  action_plan_ug = list(fr = "UGF", en = "UGF"),
  action_plan_reset_filters = list(fr = "Réinitialiser", en = "Reset"),
  action_plan_view_map_table = list(fr = "Carte + Tableau", en = "Map + Table"),
  action_plan_view_kanban = list(fr = "Kanban", en = "Kanban"),
  action_plan_kanban_empty_col = list(
    fr = "Aucune action.",
    en = "No action."
  ),
  action_plan_kanban_moved_fmt = list(
    fr = "Action déplacée vers : %s.",
    en = "Action moved to: %s."
  ),
  action_plan_kanban_card_hint = list(
    fr = "Double-clic pour éditer la fiche.",
    en = "Double-click to edit the card."
  ),
  action_plan_kanban_edit_title = list(
    fr = "Éditer la fiche action",
    en = "Edit action card"
  ),
  action_plan_kanban_edit_save = list(
    fr = "Enregistrer",
    en = "Save"
  ),
  action_plan_kanban_edit_saved = list(
    fr = "Fiche enregistrée.",
    en = "Card saved."
  ),
  action_plan_map_title = list(fr = "Carte des actions", en = "Action map"),
  action_plan_table_title = list(fr = "Tableau des actions", en = "Action table"),
  action_plan_color_year = list(fr = "Année", en = "Year"),
  action_plan_color_type = list(fr = "Type", en = "Type"),
  action_plan_color_priority = list(fr = "Priorité", en = "Priority"),
  action_plan_generate_all = list(fr = "Générer les actions (IA)", en = "Generate actions (AI)"),
  action_plan_add = list(fr = "Ajouter une action", en = "Add action"),
  action_plan_map_pending = list(
    fr = "La carte interactive sera disponible avec la story S4.",
    en = "The interactive map will be available with story S4."
  ),
  action_plan_table_pending = list(
    fr = "Le tableau interactif sera disponible avec la story S5.",
    en = "The interactive table will be available with story S5."
  ),
  action_plan_table_count_fmt = list(
    fr = "%d action(s) après filtres.",
    en = "%d action(s) after filtering."
  ),
  action_plan_generate_pending = list(
    fr = "Génération IA disponible avec la story S7.",
    en = "AI generation will be available with story S7."
  ),
  action_plan_add_pending = list(
    fr = "Ajout manuel disponible avec la story S5.",
    en = "Manual add will be available with story S5."
  ),
  action_plan_table_empty = list(
    fr = "Aucune action dans le plan pour le moment.",
    en = "No action in the plan yet."
  ),
  action_plan_clear_selection = list(
    fr = "Effacer la sélection",
    en = "Clear selection"
  ),

  # ----- Action sidebar sections (right-hand toolbar) ---------------
  action_plan_actions_title = list(fr = "Tableau des actions",
                                   en = "Action board"),
  action_plan_section_selection = list(
    fr = "Sélection", en = "Selection"
  ),
  action_plan_section_ia = list(fr = "IA", en = "AI"),
  action_plan_section_manual = list(fr = "Saisie manuelle", en = "Manual entry"),
  action_plan_section_exports = list(fr = "Exports", en = "Exports"),
  action_plan_readonly_banner = list(
    fr = "Mode lecture seule (votre rôle ne permet pas l'édition).",
    en = "Read-only mode (your role does not allow editing)."
  ),
  action_plan_readonly_locked = list(
    fr = "Action interdite : votre rôle est en lecture seule.",
    en = "Action denied: your role is read-only."
  ),

  # ----- Localised table column headers -----------------------------
  action_plan_col_id          = list(fr = "id",            en = "id"),
  action_plan_col_ug_id       = list(fr = "ug_id",         en = "ug_id"),
  action_plan_col_ug_label    = list(fr = "UGF",           en = "UGF"),
  action_plan_col_annee_offset = list(fr = "Décalage", en = "Offset"),
  action_plan_invalid_year = list(
    fr = "Année invalide.",
    en = "Invalid year."
  ),
  action_plan_col_annee       = list(fr = "Année",         en = "Year"),
  action_plan_col_type        = list(fr = "Type",          en = "Type"),
  action_plan_col_type_libre  = list(fr = "Type libre",    en = "Free type"),
  action_plan_col_intensite   = list(fr = "Intensité",     en = "Intensity"),
  action_plan_col_priorite    = list(fr = "Priorité",      en = "Priority"),
  action_plan_col_statut      = list(fr = "Statut",        en = "Status"),
  action_plan_col_objectifs   = list(fr = "Objectifs",     en = "Objectives"),
  action_plan_col_volume      = list(fr = "Volume (m³)",   en = "Volume (m³)"),
  action_plan_col_surface     = list(fr = "Surface (ha)",  en = "Area (ha)"),
  action_plan_col_tiges       = list(fr = "Tiges",         en = "Stems"),
  action_plan_col_rdi         = list(fr = "RDI",           en = "RDI"),
  action_plan_col_cout        = list(fr = "Coût (€)",      en = "Cost (€)"),
  action_plan_col_revenu      = list(fr = "Revenu (€)",    en = "Revenue (€)"),
  action_plan_col_bilan       = list(fr = "Bilan (€)",     en = "Balance (€)"),
  action_plan_balance_empty = list(
    fr = "Aucune action chiffrée. Le bilan apparaîtra dès que vous saisirez coût ou revenu.",
    en = "No costed action yet. Balance appears once cost or revenue is entered."
  ),
  action_plan_total_surface = list(fr = "Surface totale", en = "Total area"),
  action_plan_total_cout = list(fr = "Coût total", en = "Total cost"),
  action_plan_total_revenu = list(fr = "Revenu total", en = "Total revenue"),
  action_plan_total_bilan = list(fr = "Bilan", en = "Balance"),
  action_plan_export_terrain = list(
    fr = "Envoyer vers Terrain",
    en = "Send to Field"
  ),
  action_plan_export_terrain_empty = list(
    fr = "Le plan est vide.",
    en = "Plan is empty."
  ),
  action_plan_export_terrain_no_obs = list(
    fr = "Aucune action de type 'observation' à exporter.",
    en = "No 'observation' action to export."
  ),
  action_plan_export_terrain_ok_fmt = list(
    fr = "%d point(s) d'observation envoyés vers l'onglet Terrain.",
    en = "%d observation point(s) sent to the Field tab."
  ),
  action_plan_export_terrain_failed = list(
    fr = "Échec de l'écriture des points d'observation.",
    en = "Failed to write observation points."
  ),
  action_plan_field_realised_fmt = list(
    fr = "Import terrain : %d action(s) d'observation passées en 'réalisée'.",
    en = "Field import: %d observation action(s) flipped to 'done'."
  ),
  action_plan_download_gpkg = list(
    fr = "T\u00e9l\u00e9charger le GeoPackage",
    en = "Download GeoPackage"
  ),
  action_plan_download_pdf = list(
    fr = "T\u00e9l\u00e9charger le PDF",
    en = "Download PDF"
  ),
  action_plan_export_running_gpkg = list(
    fr = "Export GeoPackage en cours…",
    en = "Exporting GeoPackage…"
  ),
  action_plan_export_running_pdf = list(
    fr = "Génération du rapport PDF…",
    en = "Generating PDF report…"
  ),
  action_plan_pdf_failed = list(
    fr = "Échec de la génération du PDF",
    en = "PDF generation failed"
  ),
  action_plan_col_commentaire = list(fr = "Commentaire",   en = "Comment"),
  action_plan_col_source      = list(fr = "Source",        en = "Source"),

  # ----- Lot 3 (S7 IA + S8 Kanban) ----------------------------------
  action_plan_no_ug = list(
    fr = "Le projet ne contient aucune UGF.",
    en = "The project has no UGF."
  ),
  action_plan_generate_title = list(
    fr = "Générer un plan d'actions (IA)",
    en = "Generate action plan (AI)"
  ),
  action_plan_generate_help = list(
    fr = "L'IA produit un plan à partir des commentaires de synthèse + des familles enregistrés dans le projet. Les actions arrivent en statut 'proposée'.",
    en = "The LLM produces a plan from the synthesis + family comments saved on the project. Actions land with status 'proposed'."
  ),
  action_plan_generate_scope = list(fr = "Portée", en = "Scope"),
  action_plan_generate_scope_all = list(
    fr = "Toutes les UGF du projet",
    en = "All project UGFs"
  ),
  action_plan_generate_scope_sel_fmt = list(
    fr = "UGF sélectionnées (%d)",
    en = "Selected UGFs (%d)"
  ),
  action_plan_generate_scope_sel_none = list(
    fr = "UGF sélectionnées (aucune pour l'instant)",
    en = "Selected UGFs (none yet)"
  ),
  action_plan_generate_overwrite = list(
    fr = "Remplacer les actions existantes pour ces UGF",
    en = "Overwrite existing actions for these UGFs"
  ),
  action_plan_generate_run = list(fr = "Générer", en = "Generate"),
  action_plan_generating = list(
    fr = "Génération du plan en cours...",
    en = "Generating plan..."
  ),
  action_plan_generate_no_comments = list(
    fr = "Aucun commentaire (synthèse / famille) à exploiter. Génère d'abord la synthèse IA dans l'onglet 'Synthèse'.",
    en = "No comments (synthesis / family) to work from. Generate the AI synthesis from the 'Synthesis' tab first."
  ),
  action_plan_generate_no_actions = list(
    fr = "Le LLM n'a renvoyé aucune action exploitable.",
    en = "The LLM returned no usable action."
  ),
  action_plan_generate_ok_fmt = list(
    fr = "%d action(s) ajoutée(s) ; %d écartée(s).",
    en = "%d action(s) added; %d discarded."
  ),
  action_plan_show_history = list(fr = "Historique", en = "History"),
  action_plan_history_pick_one = list(
    fr = "Sélectionnez exactement une action dans le tableau.",
    en = "Select exactly one row in the table."
  ),
  action_plan_history_title_fmt = list(
    fr = "Historique de l'action %s",
    en = "History of action %s"
  ),
  action_plan_history_empty = list(
    fr = "Aucun changement enregistré pour cette action.",
    en = "No change recorded for this action yet."
  ),
  action_plan_no_selection = list(
    fr = "Aucune ligne sélectionnée dans le tableau.",
    en = "No row selected in the table."
  ),
  action_plan_status_proposee  = list(fr = "Proposée",  en = "Proposed"),
  action_plan_status_validee   = list(fr = "Validée",   en = "Validated"),
  action_plan_status_planifiee = list(fr = "Planifiée", en = "Planned"),
  action_plan_status_realisee  = list(fr = "Réalisée",  en = "Done"),
  action_plan_status_abandonnee = list(fr = "Abandonnée", en = "Abandoned"),
  action_plan_chat_title = list(
    fr = "Affiner le plan avec l'IA",
    en = "Refine the plan with AI"
  ),
  action_plan_chat_placeholder = list(
    fr = "Ex. : Ajoute une éclaircie en année 5 sur les UGF du nord.",
    en = "E.g. Add a thinning in year 5 on the northern UGFs."
  ),
  action_plan_chat_send = list(fr = "Envoyer", en = "Send"),
  action_plan_chat_clear = list(fr = "Effacer", en = "Clear"),
  action_plan_chat_thinking = list(
    fr = "L'IA réfléchit…",
    en = "AI is thinking…"
  ),
  action_plan_chat_role_user = list(
    fr = "Vous",
    en = "You"
  ),
  action_plan_chat_role_assistant = list(
    fr = "Assistant",
    en = "Assistant"
  ),
  action_plan_chat_scope_sel = list(
    fr = "Sélection courante",
    en = "Current selection"
  ),
  action_plan_chat_apply_overwrite_warn_fmt = list(
    fr = "⚠ Les actions existantes des %d UGF ciblées seront remplacées.",
    en = "⚠ Existing actions for the %d targeted UGFs will be replaced."
  ),
  action_plan_chat_empty = list(
    fr = "Posez une première question pour démarrer.",
    en = "Ask a first question to start."
  ),
  action_plan_chat_apply_title = list(
    fr = "Appliquer les modifications proposées ?",
    en = "Apply proposed changes?"
  ),
  action_plan_chat_apply_fmt = list(
    fr = "L'IA propose %d action(s). Les ajouter au plan ?",
    en = "The AI proposes %d action(s). Add them to the plan?"
  ),
  action_plan_chat_apply_btn = list(fr = "Appliquer", en = "Apply"),
  action_plan_chat_applied_fmt = list(
    fr = "%d action(s) appliquée(s).",
    en = "%d action(s) applied."
  ),
  action_plan_add_title = list(fr = "Nouvelle action", en = "New action"),
  action_plan_add_run = list(fr = "Créer", en = "Create"),
  action_plan_add_ok = list(fr = "Action ajoutée.", en = "Action added."),
  action_plan_year = list(fr = "Année cible", en = "Target year"),
  action_plan_comment = list(fr = "Commentaire", en = "Comment"),
  ug_map_card_title = list(fr = "Carte des unit\u00e9s de gestion foresti\u00e8re", en = "Forest Management Units Map"),
  ug_map_summary_count = list(fr = "%d t\u00e8nement(s)", en = "%d tenement(s)"),
  ug_map_summary_surface = list(fr = "Surface cadastrale : %s ha | Surface SIG : %s ha", en = "Cadastral area: %s ha | SIG area: %s ha"),

  # ============================================================
  # Search & Selection
  # ============================================================
  search_title = list(fr = "Recherche", en = "Search"),
  department = list(fr = "D\u00e9partement", en = "Department"),
  select_department = list(fr = "-- S\u00e9lectionnez --", en = "-- Select --"),
  commune = list(fr = "Commune", en = "Municipality"),
  search_commune = list(fr = "Rechercher une commune...", en = "Search for a municipality..."),
  postal_code = list(fr = "Code postal", en = "Postal code"),

  # ============================================================
  # Map
  # ============================================================
  map_title = list(fr = "Carte des parcelles cadastrales", en = "Cadastral Parcels Map"),
  basemap_osm = list(fr = "OSM", en = "OSM"),
  basemap_satellite = list(fr = "Satellite", en = "Satellite"),
  loading_parcels = list(fr = "Chargement des parcelles...", en = "Loading parcels..."),
  rendering_parcels = list(fr = "Affichage des parcelles sur la carte...", en = "Rendering parcels on map..."),
  loading_commune = list(fr = "Chargement de la commune...", en = "Loading commune..."),
  connecting_api = list(fr = "Connexion \u00e0 l'API...", en = "Connecting to API..."),
  downloading_data = list(fr = "T\u00e9l\u00e9chargement des donn\u00e9es...", en = "Downloading data..."),
  processing_data = list(fr = "Traitement des donn\u00e9es...", en = "Processing data..."),
  click_to_select = list(fr = "Cliquez pour s\u00e9lectionner", en = "Click to select"),

  # ============================================================
  # Selection
  # ============================================================
  selected_parcels = list(fr = "Parcelles s\u00e9lectionn\u00e9es", en = "Selected Parcels"),
  parcels_selected = list(fr = "parcelles", en = "parcels"),
  no_selection = list(fr = "Aucune parcelle s\u00e9lectionn\u00e9e", en = "No parcel selected"),
  max_parcels_warning = list(
    fr = "Limite de {max} parcelles atteinte",
    en = "Maximum {max} parcels reached"
  ),
  clear_selection = list(fr = "Tout d\u00e9s\u00e9lectionner", en = "Clear selection"),
  selection_blocked_during_computation = list(
    fr = "La s\u00e9lection des parcelles est bloqu\u00e9e pendant les calculs",
    en = "Parcel selection is blocked during computation"
  ),
  selection_cleared = list(
    fr = "S\u00e9lection effac\u00e9e",
    en = "Selection cleared"
  ),

  # ============================================================
  # Project
  # ============================================================
  project_info = list(fr = "Informations projet", en = "Project Information"),
  project_name = list(fr = "Nom du projet", en = "Project name"),
  project_name_placeholder = list(fr = "Mon projet forestier", en = "My forest project"),
  project_description = list(fr = "Description", en = "Description"),
  project_description_placeholder = list(
    fr = "Description optionnelle...",
    en = "Optional description..."
  ),
  project_owner = list(fr = "Propri\u00e9taire / Gestionnaire", en = "Owner / Manager"),
  project_owner_placeholder = list(fr = "Nom du propri\u00e9taire", en = "Owner name"),
  project_groupes_profile = list(
    fr = "Profil de classement UGF",
    en = "UGF classification profile"
  ),
  project_groupes_profile_help = list(
    fr = "D\u00e9termine les libell\u00e9s et codes de la liste d\u00e9roulante 'groupe/zone' des UGF (ONF, CRPF, OFB, autre).",
    en = "Defines the labels and codes of the UGF 'group/zone' dropdown (ONF, CRPF, OFB, other)."
  ),
  # For\u00eat ancienne -> N2 continuit\u00e9 (spec 031)
  foret_ancienne_section = list(
    fr = "For\u00eat ancienne (continuit\u00e9 N2)",
    en = "Ancient forest (N2 continuity)"
  ),
  foret_ancienne_source = list(
    fr = "Source historique (raster class\u00e9 ou vecteur)",
    en = "Historical source (classified raster or vector)"
  ),
  foret_ancienne_forest_class = list(
    fr = "Classe(s) = for\u00eat (raster)",
    en = "Forest class(es) (raster)"
  ),
  foret_ancienne_threshold = list(
    fr = "Seuil (valeur \u2265)",
    en = "Threshold (value \u2265)"
  ),
  foret_ancienne_min_area = list(
    fr = "Surface minimale (m\u00b2)",
    en = "Minimum area (m\u00b2)"
  ),
  foret_ancienne_mode_class = list(fr = "Par classe(s)", en = "By class(es)"),
  foret_ancienne_mode_threshold = list(fr = "Par seuil", en = "By threshold"),
  foret_ancienne_hint = list(
    fr = "Cartes de Cassini, d'\u00e9tat-major, ou couche IGN for\u00eat ancienne. Corona-4B non couvert en France.",
    en = "Cassini maps, \u00e9tat-major maps, or the IGN ancient-forest layer. Corona-4B has no France coverage."
  ),
  foret_ancienne_save = list(fr = "Enregistrer la source", en = "Save source"),
  foret_ancienne_clear = list(fr = "Retirer la source", en = "Remove source"),
  foret_ancienne_saved = list(
    fr = "Source for\u00eat ancienne enregistr\u00e9e",
    en = "Ancient-forest source saved"
  ),
  foret_ancienne_cleared = list(
    fr = "Source for\u00eat ancienne retir\u00e9e",
    en = "Ancient-forest source removed"
  ),
  foret_ancienne_none = list(
    fr = "Aucune source \u2014 N2 sur la couverture actuelle",
    en = "No source \u2014 N2 on current cover"
  ),
  foret_ancienne_current = list(fr = "Source actuelle", en = "Current source"),
  foret_ancienne_need_project = list(
    fr = "Cr\u00e9ez d'abord le projet pour ajouter une source for\u00eat ancienne.",
    en = "Create the project first to add an ancient-forest source."
  ),
  sufosat_section = list(fr = "Coupes rases (SUFOSAT)", en = "Clear-cuts (SUFOSAT)"),
  sufosat_enable = list(
    fr = "Activer la d\u00e9tection des coupes rases (T3)",
    en = "Enable clear-cut detection (T3)"
  ),
  sufosat_window = list(
    fr = "Fen\u00eatre de r\u00e9cence (ann\u00e9es)",
    en = "Recency window (years)"
  ),
  sufosat_min_proba = list(fr = "Seuil de probabilit\u00e9", en = "Probability threshold"),
  sufosat_hint = list(
    fr = "D\u00e9tection nationale des coupes rases par radar Sentinel-1 (CNES/CESBIO). Indicateur invers\u00e9 : plus de coupe = score plus bas.",
    en = "National clear-cut detection from Sentinel-1 radar (CNES/CESBIO). Inverted indicator: more cutting = lower score."
  ),
  sufosat_save = list(fr = "Enregistrer", en = "Save"),
  sufosat_saved = list(
    fr = "Coupes rases : pr\u00e9f\u00e9rence enregistr\u00e9e",
    en = "Clear-cuts: preference saved"
  ),
  sufosat_need_theia = list(
    fr = "Configurez vos identifiants Theia (cl\u00e9s API) pour activer cette source.",
    en = "Configure your Theia credentials (API keys) to enable this source."
  ),
  sufosat_active = list(
    fr = "Activ\u00e9 \u2014 T3 sur le radar de la famille T",
    en = "Enabled \u2014 T3 on the T family radar"
  ),
  sufosat_none = list(
    fr = "Non activ\u00e9 \u2014 famille T sur T1/T2",
    en = "Not enabled \u2014 T family on T1/T2"
  ),
  # Rafra\u00eechissement urbain \u2192 A5 (LST Theia/Thermocity, spec 032)
  lst_section = list(fr = "Rafra\u00eechissement urbain (LST)", en = "Urban cooling (LST)"),
  lst_enable = list(
    fr = "Activer le rafra\u00eechissement urbain (A5)",
    en = "Enable urban cooling (A5)"
  ),
  lst_buffer = list(
    fr = "Rayon de l'anneau de r\u00e9f\u00e9rence (m)",
    en = "Reference-ring radius (m)"
  ),
  lst_hint = list(
    fr = "Fra\u00eecheur de surface (LST, Theia/Thermocity) de l'arbre en ville vs son environnement. Disponible en zone urbaine / m\u00e9tropole uniquement (hors couverture, l'indicateur reste vide). Sens direct : plus frais = meilleur.",
    en = "Surface coolness (LST, Theia/Thermocity) of the urban tree vs its surroundings. Available in urban / metropolitan areas only (outside coverage, the indicator stays empty). Direct sense: cooler = better."
  ),
  lst_save = list(fr = "Enregistrer", en = "Save"),
  lst_saved = list(
    fr = "Rafra\u00eechissement urbain : pr\u00e9f\u00e9rence enregistr\u00e9e",
    en = "Urban cooling: preference saved"
  ),
  lst_need_theia = list(
    fr = "Configurez vos identifiants Theia (cl\u00e9s API) pour activer cette source.",
    en = "Configure your Theia credentials (API keys) to enable this source."
  ),
  lst_active = list(
    fr = "Activ\u00e9 \u2014 A5 sur le radar de la famille A",
    en = "Enabled \u2014 A5 on the A family radar"
  ),
  lst_none = list(
    fr = "Non activ\u00e9 \u2014 famille A sur A1-A4",
    en = "Not enabled \u2014 A family on A1-A4"
  ),
  project_date = list(fr = "Date de cr\u00e9ation", en = "Creation date"),
  project_path_label = list(fr = "R\u00e9pertoire de sauvegarde", en = "Storage directory"),
  created_at = list(fr = "Cr\u00e9\u00e9 le", en = "Created on"),
  auto_generated = list(fr = "G\u00e9n\u00e9r\u00e9 automatiquement", en = "Auto-generated"),
  create_project = list(fr = "Cr\u00e9er le projet", en = "Create project"),
  update_project = list(fr = "Mettre \u00e0 jour", en = "Update"),
  project_updated = list(fr = "Projet mis \u00e0 jour", en = "Project updated"),
  field_required = list(fr = "Ce champ est obligatoire", en = "This field is required"),
  max_chars = list(fr = "Maximum", en = "Maximum"),
  characters = list(fr = "caract\u00e8res", en = "characters"),
  name_required = list(fr = "Le nom du projet est obligatoire", en = "Project name is required"),
  name_too_long = list(fr = "Le nom ne doit pas d\u00e9passer 100 caract\u00e8res", en = "Name must not exceed 100 characters"),
  description_too_long = list(fr = "La description ne doit pas d\u00e9passer 500 caract\u00e8res", en = "Description must not exceed 500 characters"),
  owner_too_long = list(fr = "Le propri\u00e9taire ne doit pas d\u00e9passer 100 caract\u00e8res", en = "Owner must not exceed 100 characters"),
  no_parcels_selected = list(fr = "Veuillez s\u00e9lectionner au moins une parcelle", en = "Please select at least one parcel"),
  project_created = list(fr = "Projet cr\u00e9\u00e9", en = "Project created"),
  project_loading = list(fr = "Chargement du projet", en = "Loading project"),
  project_loaded = list(fr = "Projet charg\u00e9", en = "Project loaded"),
  project_not_found = list(fr = "Projet non trouv\u00e9", en = "Project not found"),
  project_deleted = list(fr = "Projet supprim\u00e9", en = "Project deleted"),

  # Recent projects
  recent_projects = list(fr = "Projets r\u00e9cents", en = "Recent projects"),
  no_recent_projects = list(fr = "Aucun projet r\u00e9cent", en = "No recent projects"),

  # Project status
  status_draft = list(fr = "Brouillon", en = "Draft"),
  status_downloading = list(fr = "T\u00e9l\u00e9chargement", en = "Downloading"),
  status_computing = list(fr = "Calcul en cours", en = "Computing"),
  status_completed = list(fr = "Termin\u00e9", en = "Completed"),
  status_error = list(fr = "Erreur", en = "Error"),
  status_unknown = list(fr = "Inconnu", en = "Unknown"),
  active = list(fr = "Actif", en = "Active"),
  corrupted = list(fr = "Corrompu", en = "Corrupted"),

  # Delete corrupted projects
  delete_corrupted_project = list(fr = "Supprimer le projet corrompu", en = "Delete corrupted project"),
  delete_corrupted_confirm = list(
    fr = "Ce projet est corrompu et ne peut pas \u00eatre ouvert. Voulez-vous le supprimer d\u00e9finitivement ?",
    en = "This project is corrupted and cannot be opened. Do you want to delete it permanently?"
  ),

  # Parcels
  parcel = list(fr = "parcelle", en = "parcel"),
  parcels = list(fr = "parcelles", en = "parcels"),
  created = list(fr = "Cr\u00e9\u00e9", en = "Created"),
  parcels_loaded = list(fr = "parcelles charg\u00e9es", en = "parcels loaded"),
  error_loading_parcels = list(fr = "Erreur chargement parcelles", en = "Error loading parcels"),

  # General
  error = list(fr = "Erreur", en = "Error"),

  # ============================================================
  # Computation
  # ============================================================
  compute_button = list(fr = "Lancer les calculs", en = "Start Calculations"),
  chm_status_auto_on = list(
    fr = "CHM Open-Canopy : activ\u00e9 automatiquement",
    en = "CHM Open-Canopy: auto-enabled"
  ),
  chm_status_auto_off = list(
    fr = "CHM Open-Canopy : indisponible (package manquant ou d\u00e9sactiv\u00e9)",
    en = "CHM Open-Canopy: unavailable (package missing or disabled)"
  ),
  augmented_height_lidar_badge = list(
    fr = "Hauteur LiDAR HD",
    en = "LiDAR HD height"
  ),
  augmented_height_lidar_tooltip = list(
    fr = "Les indicateurs de hauteur (P1, P2, C1, B2, R2) ont été calculés à partir du Modèle Numérique de Hauteur LiDAR HD (IGN, campagne aérienne nationale). Mesure directe (précision nominale ~0.5 m), pas de prédiction ML. NDP 2.",
    en = "Height indicators (P1, P2, C1, B2, R2) were computed from the IGN LiDAR HD Canopy Height Model (national airborne campaign). Direct measurement (~0.5 m nominal accuracy), not an ML prediction. NDP 2."
  ),
  augmented_height_ml_badge = list(
    fr = "Hauteur ML",
    en = "ML height"
  ),
  augmented_height_ml_tooltip = list(
    fr = "ML = Machine Learning (apprentissage automatique). Les indicateurs de hauteur (P1, P2, C1, B2, R2) ont \u00e9t\u00e9 calcul\u00e9s \u00e0 partir d'un mod\u00e8le num\u00e9rique de hauteur de canop\u00e9e (CHM) pr\u00e9dit par apprentissage automatique (Open-Canopy sur ortho IGN). NDP augment\u00e9 par rapport aux donn\u00e9es publiques de base.",
    en = "ML = Machine Learning. Height indicators (P1, P2, C1, B2, R2) were computed from a Canopy Height Model predicted by machine learning (Open-Canopy on IGN orthoimagery). Augmented NDP compared to baseline public data."
  ),
  augmented_inventory_ml_badge = list(
    fr = "Inventaire ML",
    en = "ML inventory"
  ),
  augmented_inventory_ml_tooltip = list(
    fr = "ML = Machine Learning (apprentissage automatique). Les indicateurs d'inventaire dendrom\u00e9trique (P1 volume, P3 qualit\u00e9, E1 bois-\u00e9nergie) ont \u00e9t\u00e9 calcul\u00e9s \u00e0 partir d'un diam\u00e8tre moyen (D_g) et d'une densit\u00e9 (tiges/ha) estim\u00e9s depuis le CHM par self-thinning de Charru 2012 (NDP 1 synth\u00e9tique). Moins fiable qu'un inventaire terrain (\u00b120-25 %) ; \u00e0 valider sur placette pour un usage d\u00e9cisionnel.",
    en = "ML = Machine Learning. Dendrometric inventory indicators (P1 volume, P3 quality, E1 fuelwood) were estimated from a quadratic mean diameter (D_g) and stand density (stems/ha) derived from the CHM via the Charru 2012 self-thinning law (synthetic NDP 1). Less reliable than a terrain inventory (\u00b120-25 %); validate on plots before operational use."
  ),
  confirm_computation_title = list(
    fr = "Confirmer le lancement des calculs",
    en = "Confirm Calculation Start"
  ),
  confirm_computation_message = list(
    fr = "Vous \u00eates sur le point de lancer le calcul des indicateurs pour ce projet :",
    en = "You are about to start indicator calculations for this project:"
  ),
  confirm_computation_warning = list(
    fr = "Une fois les calculs lanc\u00e9s, vous ne pourrez plus modifier la s\u00e9lection des parcelles.",
    en = "Once calculations start, you will not be able to modify the parcel selection."
  ),
  start_computation = list(fr = "D\u00e9marrer les calculs", en = "Start Calculations"),
  parcels_count = list(fr = "Nombre de parcelles", en = "Number of parcels"),
  computing = list(fr = "Calcul en cours...", en = "Computing..."),
  downloading_data = list(fr = "T\u00e9l\u00e9chargement des donn\u00e9es...", en = "Downloading data..."),
  computing_indicator = list(
    fr = "Calcul de l'indicateur {indicator}...",
    en = "Computing indicator {indicator}..."
  ),
  computation_complete = list(fr = "Calculs termin\u00e9s", en = "Calculations complete"),
  computation_cancelled = list(fr = "Calculs annul\u00e9s", en = "Calculations cancelled"),
  recomputing = list(fr = "Relance du calcul en cours\u2026", en = "Restarting computation\u2026"),
  computation_error = list(fr = "Erreur lors du calcul", en = "Computation error"),

  # Progress module
  progress_overall = list(fr = "Progression globale", en = "Overall progress"),
  completed = list(fr = "termin\u00e9s", en = "completed"),
  failed = list(fr = "\u00e9chou\u00e9s", en = "failed"),
  pending = list(fr = "en attente", en = "pending"),
  phase_init = list(fr = "Initialisation...", en = "Initializing..."),
  phase_downloading = list(fr = "T\u00e9l\u00e9chargement des donn\u00e9es...", en = "Downloading data..."),
  phase_computing = list(fr = "Calcul des indicateurs...", en = "Computing indicators..."),
  phase_complete = list(fr = "Termin\u00e9", en = "Complete"),
  task_initializing = list(fr = "Initialisation des calculs\u2026",
                            en = "Initializing computation\u2026"),
  task_download_start = list(fr = "D\u00e9marrage du t\u00e9l\u00e9chargement", en = "Starting download"),
  task_compute_start = list(fr = "D\u00e9marrage des calculs", en = "Starting calculations"),
  task_complete = list(fr = "Traitement termin\u00e9", en = "Processing complete"),
  task_error = list(fr = "Erreur de traitement", en = "Processing error"),
  task_resuming = list(fr = "Reprise des calculs...", en = "Resuming calculations..."),
  task_spectral_diversity = list(fr = "Diversit\u00e9 spectrale (B4/L3) : analyse biodivMapR sur Sentinel-2...", en = "Spectral diversity (B4/L3): biodivMapR analysis on Sentinel-2..."),
  elapsed_time = list(fr = "Temps \u00e9coul\u00e9", en = "Elapsed time"),
  errors_title = list(fr = "Erreurs rencontr\u00e9es :", en = "Errors encountered:"),
  computation_summary = list(
    fr = "%d indicateur(s) calcul\u00e9(s) sur %d",
    en = "%d indicator(s) computed out of %d"
  ),
  computation_duration = list(
    fr = "Dur\u00e9e du calcul : %s",
    en = "Computation time: %s"
  ),
  computation_failed = list(
    fr = "%d en \u00e9chec",
    en = "%d failed"
  ),
  unknown_error = list(fr = "Erreur inconnue", en = "Unknown error"),
  and_n_more_errors = list(fr = "Et %d autre(s) erreur(s)...", en = "And %d more error(s)..."),
  retry = list(fr = "R\u00e9essayer", en = "Retry"),
  retry_toast = list(
    fr = "Projet r\u00e9initialis\u00e9 \u2014 pr\u00eat \u00e0 relancer le calcul.",
    en = "Project reset \u2014 ready to recompute."
  ),
  retry_in_progress = list(
    fr = "R\u00e9initialisation des calculs en cours\u2026",
    en = "Resetting computation\u2026"
  ),
  view_results = list(fr = "Voir les r\u00e9sultats", en = "View results"),
  resuming_computation = list(
    fr = "Reprise du calcul - %d indicateur(s) d\u00e9j\u00e0 calcul\u00e9(s)",
    en = "Resuming computation - %d indicator(s) already computed"
  ),
  skipped_indicators = list(
    fr = "%d indicateur(s) saut\u00e9(s) (d\u00e9j\u00e0 calcul\u00e9s)",
    en = "%d indicator(s) skipped (already computed)"
  ),
  indicator_column = list(fr = "Indicateur", en = "Indicator"),
  status_column = list(fr = "Statut", en = "Status"),
  status_ok = list(fr = "Calcul\u00e9", en = "Computed"),
  status_error = list(fr = "Erreur", en = "Error"),
  status_not_computed = list(fr = "Non calcul\u00e9", en = "Not computed"),

  # ============================================================
  # Synthesis
  # ============================================================
  synthesis_title = list(fr = "Synth\u00e8se du projet", en = "Project Synthesis"),
  radar_title = list(fr = "Radar des 12 familles", en = "12 Families Radar"),
  radar_tip_title = list(
    fr = "LIRE LE RADAR N\u00c9M\u00c9TON",
    en = "READING THE N\u00c9M\u00c9TON RADAR"
  ),
  radar_tip_subtitle = list(
    fr = "Une boussole foresti\u00e8re, pas un classement",
    en = "A forest compass, not a ranking"
  ),
  radar_tip_intro = list(
    fr = paste0(
      "Le graphique radar N\u00e9m\u00e9ton repr\u00e9sente les ",
      "<strong>12 familles d\u2019indicateurs forestiers</strong>, ",
      "chacune exprim\u00e9e sur une \u00e9chelle de 0 \u00e0 100. Mais cette ",
      "repr\u00e9sentation ne doit pas \u00eatre lue comme un bulletin de notes."
    ),
    en = paste0(
      "The N\u00e9m\u00e9ton radar chart represents the ",
      "<strong>12 families of forest indicators</strong>, ",
      "each expressed on a scale from 0 to 100. But this representation ",
      "should not be read as a report card."
    )
  ),
  radar_tip_not_title = list(
    fr = "Ce que le radar ne dit pas",
    en = "What the radar does NOT say"
  ),
  radar_tip_not_text = list(
    fr = paste0(
      "Qu\u2019une for\u00eat \u00e0 75 est \u00ab meilleure \u00bb qu\u2019une for\u00eat \u00e0 45 sur un ",
      "indicateur donn\u00e9. Deux for\u00eats aux contextes diff\u00e9rents \u2014 essence, ",
      "sol, climat, altitude, histoire sylvicole \u2014 ne peuvent pas \u00eatre ",
      "compar\u00e9es sur la seule base de ces valeurs num\u00e9riques. Un score de ",
      "40 en diversit\u00e9 structurale dans une ch\u00eanaie de plaine et un score ",
      "de 80 dans une h\u00eatraie de montagne ne racontent pas la m\u00eame histoire."
    ),
    en = paste0(
      "That a forest scoring 75 is \u2018better\u2019 than one scoring 45 on a given ",
      "indicator. Two forests with different contexts \u2014 species, soil, climate, ",
      "altitude, silvicultural history \u2014 cannot be compared solely on these ",
      "numerical values."
    )
  ),
  radar_tip_yes_title = list(
    fr = "Ce que le radar dit",
    en = "What the radar DOES say"
  ),
  radar_tip_yes_text = list(
    fr = paste0(
      "Il r\u00e9v\u00e8le le <strong>profil</strong> d\u2019une for\u00eat, sa physionomie ",
      "syst\u00e9mique. Comme une <strong>rose des vents</strong>, il indique des ",
      "directions \u2014 les axes vers lesquels la for\u00eat \u00ab tire \u00bb, ceux o\u00f9 elle ",
      "semble en retrait. La <strong>forme du polygone</strong> qui se dessine ",
      "est plus parlante que chaque valeur prise isol\u00e9ment."
    ),
    en = paste0(
      "It reveals a forest\u2019s <strong>profile</strong>, its systemic physiognomy. ",
      "Like a <strong>compass rose</strong>, it shows directions \u2014 the axes toward ",
      "which the forest \u2018pulls\u2019, those where it seems to retreat. The ",
      "<strong>shape of the polygon</strong> is more meaningful than any single value."
    )
  ),
  radar_tip_how_title = list(
    fr = "Comment l\u2019utiliser",
    en = "How to use it"
  ),
  radar_tip_how_text = list(
    fr = paste0(
      "L\u2019int\u00e9r\u00eat du radar est <strong>diachronique</strong> plut\u00f4t que ",
      "comparatif : c\u2019est en observant l\u2019\u00e9volution du profil d\u2019une m\u00eame ",
      "for\u00eat dans le temps qu\u2019il prend tout son sens. D\u2019un passage \u00e0 ",
      "l\u2019autre, le polygone se d\u00e9forme, se r\u00e9\u00e9quilibre, se d\u00e9ploie \u2014 ",
      "et c\u2019est cette <strong>trajectoire</strong> qui guide le forestier ",
      "dans ses choix de gestion patrimoniale."
    ),
    en = paste0(
      "The radar\u2019s value is <strong>diachronic</strong> rather than comparative: ",
      "it is by observing the evolution of the same forest\u2019s profile over time ",
      "that it becomes most meaningful. From one survey to the next, the polygon ",
      "shifts, rebalances, expands \u2014 and this <strong>trajectory</strong> guides ",
      "the forester in heritage management choices."
    )
  ),
  radar_tip_conclusion = list(
    fr = paste0(
      "Le radar N\u00e9m\u00e9ton ne mesure pas la \u00ab qualit\u00e9 \u00bb d\u2019une for\u00eat sur ",
      "une \u00e9chelle universelle. Il en dessine le temp\u00e9rament, et offre au ",
      "gestionnaire une boussole pour accompagner la for\u00eat dans sa direction."
    ),
    en = paste0(
      "The N\u00e9m\u00e9ton radar does not measure a forest\u2019s \u2018quality\u2019 on a ",
      "universal scale. It sketches its temperament, offering the manager ",
      "a compass to accompany the forest in its direction."
    )
  ),
  score_tip_title = list(
    fr = "LIRE L\u2019INDICE G\u00c9N\u00c9RAL N\u00c9M\u00c9TON",
    en = "READING THE N\u00c9M\u00c9TON GENERAL INDEX"
  ),
  score_tip_subtitle = list(
    fr = "Un rep\u00e8re, pas une note",
    en = "A benchmark, not a grade"
  ),
  score_tip_intro = list(
    fr = paste0(
      "L\u2019indice g\u00e9n\u00e9ral N\u00e9m\u00e9ton synth\u00e9tise les 12 familles d\u2019indicateurs ",
      "en une valeur unique sur 100. Il repr\u00e9sente la <strong>moyenne ",
      "pond\u00e9r\u00e9e</strong> de l\u2019ensemble des axes du radar. Mais cette valeur ",
      "appelle une lecture prudente."
    ),
    en = paste0(
      "The N\u00e9m\u00e9ton general index synthesises the 12 indicator families into ",
      "a single value out of 100. It represents the <strong>weighted mean</strong> ",
      "of all radar axes. But this value calls for a cautious reading."
    )
  ),
  score_tip_not_title = list(
    fr = "Ce que l\u2019indice ne dit pas",
    en = "What the index does NOT say"
  ),
  score_tip_not_text = list(
    fr = paste0(
      "Qu\u2019une for\u00eat \u00e0 65 est en meilleure sant\u00e9 qu\u2019une for\u00eat \u00e0 41. ",
      "Deux for\u00eats de contextes diff\u00e9rents \u2014 essences, sols, climat, altitude, ",
      "historique de gestion \u2014 peuvent afficher des indices tr\u00e8s diff\u00e9rents ",
      "tout en \u00e9tant chacune dans un \u00e9tat coh\u00e9rent avec leur milieu. ",
      "<strong>L\u2019indice n\u2019est pas un outil de classement entre for\u00eats.</strong>"
    ),
    en = paste0(
      "That a forest at 65 is healthier than one at 41. Two forests with ",
      "different contexts \u2014 species, soils, climate, altitude, management ",
      "history \u2014 may display very different indices while each being in a ",
      "state consistent with their environment. ",
      "<strong>The index is not a tool for ranking forests.</strong>"
    )
  ),
  score_tip_yes_title = list(
    fr = "Ce que l\u2019indice dit",
    en = "What the index DOES say"
  ),
  score_tip_yes_text = list(
    fr = paste0(
      "Il donne un <strong>rep\u00e8re synth\u00e9tique</strong> de la situation d\u2019une ",
      "for\u00eat \u00e0 un instant donn\u00e9. C\u2019est un <strong>point de d\u00e9part</strong> qui ",
      "invite \u00e0 regarder le d\u00e9tail du radar pour comprendre quels axes tirent ",
      "la valeur vers le haut ou vers le bas. Seul, il est un signal ; ",
      "associ\u00e9 au radar, il devient une information."
    ),
    en = paste0(
      "It provides a <strong>synthetic benchmark</strong> of a forest\u2019s situation ",
      "at a given point in time. It is a <strong>starting point</strong> that invites ",
      "looking at the radar detail to understand which axes pull the value up or ",
      "down. Alone, it is a signal; combined with the radar, it becomes information."
    )
  ),
  score_tip_how_title = list(
    fr = "Comment l\u2019utiliser",
    en = "How to use it"
  ),
  score_tip_how_text = list(
    fr = paste0(
      "La vraie puissance de l\u2019indice g\u00e9n\u00e9ral est <strong>diachronique</strong> : ",
      "c\u2019est en suivant son \u00e9volution pour une m\u00eame for\u00eat au fil du temps qu\u2019il ",
      "prend tout son sens. Un indice qui passe de 41 \u00e0 52 en cinq ans signale ",
      "une dynamique positive. Un indice stable peut refl\u00e9ter un \u00e9quilibre ",
      "atteint \u2014 ou une stagnation qu\u2019il convient d\u2019interroger en consultant ",
      "le d\u00e9tail des 12 familles."
    ),
    en = paste0(
      "The true power of the general index is <strong>diachronic</strong>: it is by ",
      "tracking its evolution for the same forest over time that it becomes most ",
      "meaningful. An index rising from 41 to 52 in five years signals a positive ",
      "dynamic. A stable index may reflect an achieved balance \u2014 or a stagnation ",
      "worth investigating by consulting the detail of the 12 families."
    )
  ),
  score_tip_conclusion = list(
    fr = paste0(
      "L\u2019indice g\u00e9n\u00e9ral N\u00e9m\u00e9ton est un thermom\u00e8tre, pas un diagnostic. ",
      "Il indique une temp\u00e9rature ; c\u2019est le radar qui r\u00e9v\u00e8le o\u00f9 se situe ",
      "la fi\u00e8vre ou la vitalit\u00e9."
    ),
    en = paste0(
      "The N\u00e9m\u00e9ton general index is a thermometer, not a diagnosis. ",
      "It indicates a temperature; it is the radar that reveals where the ",
      "fever or vitality lies."
    )
  ),
  score_tip_index_label = list(
    fr = "Indice g\u00e9n\u00e9ral N\u00e9m\u00e9ton",
    en = "N\u00e9m\u00e9ton General Index"
  ),
  summary_table_title = list(fr = "R\u00e9capitulatif par famille", en = "Summary by Family"),
  summary_tip_title = list(
    fr = "Les 12 dimensions de N\u00e9m\u00e9ton",
    en = "The 12 dimensions of N\u00e9m\u00e9ton"
  ),
  summary_tip_subtitle = list(
    fr = "Traduire la for\u00eat en signes \u2014 Chapitre 17",
    en = "Translating the forest into signs \u2014 Chapter 17"
  ),
  summary_tip_intro = list(
    fr = paste0(
      "Les 12 dimensions organisent la complexit\u00e9 de la for\u00eat sans la mutiler. ",
      "Elles se r\u00e9partissent en trois cercles autour du triangle <b>Produire \u2013 Prot\u00e9ger \u2013 Partager</b>."
    ),
    en = paste0(
      "The 12 dimensions organize the complexity of the forest without diminishing it. ",
      "They are arranged in three circles around the triangle <b>Produce \u2013 Protect \u2013 Share</b>."
    )
  ),
  summary_tip_base_title = list(
    fr = "Carr\u00e9 de base : C, B, W, A",
    en = "Core square: C, B, W, A"
  ),
  summary_tip_base_text = list(
    fr = "<b>C</b>arbone \u00b7 <b>B</b>iodiversit\u00e9 \u00b7 <b>W</b>ater (eau) \u00b7 <b>A</b>ir (microclimat)",
    en = "<b>C</b>arbon \u00b7 <b>B</b>iodiversity \u00b7 <b>W</b>ater \u00b7 <b>A</b>ir (microclimate)"
  ),
  summary_tip_support_title = list(
    fr = "Supports du vivant : F, L, T, R",
    en = "Life supports: F, L, T, R"
  ),
  summary_tip_support_text = list(
    fr = "<b>F</b>ertilit\u00e9 des sols \u00b7 <b>L</b>andscape (paysage) \u00b7 <b>T</b>rame \u00e9cologique \u00b7 <b>R</b>\u00e9silience",
    en = "<b>F</b>ertility \u00b7 <b>L</b>andscape \u00b7 Ecological <b>T</b>rame \u00b7 <b>R</b>esilience"
  ),
  summary_tip_human_title = list(
    fr = "Dimensions humaines : S, P, E, N",
    en = "Human dimensions: S, P, E, N"
  ),
  summary_tip_human_text = list(
    fr = "<b>S</b>ant\u00e9 \u00b7 <b>P</b>atrimoine \u00b7 <b>E</b>ducation \u00b7 <b>N</b>uit",
    en = "<b>S</b>(health) \u00b7 <b>P</b>atrimony \u00b7 <b>E</b>ducation \u00b7 <b>N</b>ight"
  ),
  summary_tip_acrostic_title = list(
    fr = "L'acrostiche VIVREENFORET",
    en = "The VIVREENFORET acrostic"
  ),
  summary_tip_acrostic = list(
    fr = paste0(
      "<b>V</b> comme <i>Vaporeuse</i> : la brume qui s'\u00e9l\u00e8ve au petit matin<br>",
      "<b>I</b> comme <i>Infiltrante</i> : l'eau qui dispara\u00eet nourrissant les nappes<br>",
      "<b>V</b> comme <i>Vivante</i> : bact\u00e9ries, champignons, insectes que nul inventaire ne pourra \u00e9puiser<br>",
      "<b>R</b> comme <i>Racinaire</i> : le r\u00e9seau souterrain qui tient la pente et \u00e9change<br>",
      "<b>E</b> comme <i>\u00c9ponge</i> : absorber les chocs, les pluies, les vents, les exc\u00e8s<br>",
      "<b>E</b> comme <i>\u00c9missive</i> : oxyg\u00e8ne, odeurs de r\u00e9sine, particules qui ensemencent les nuages<br>",
      "<b>N</b> comme <i>Nocturne</i> : le ciel qui r\u00e9appara\u00eet entre les cimes<br>",
      "<b>F</b> comme <i>Foresti\u00e8re</i> : ce sentiment d'\u00eatre \u00ab en for\u00eat \u00bb, entour\u00e9, prot\u00e9g\u00e9<br>",
      "<b>O</b> comme <i>Oscillante</i> : les cycles, les chablis, les renaissances<br>",
      "<b>R</b> comme <i>R\u00e9sonnante</i> : la for\u00eat r\u00e9pond \u00e0 ce qu'on lui fait<br>",
      "<b>E</b> comme <i>\u00c9toil\u00e9e</i> : chaque feuille capte une lumi\u00e8re venue de l'infini<br>",
      "<b>T</b> comme <i>Tellurique</i> : l'ancrage dans la roche et le sol profond"
    ),
    en = paste0(
      "<b>V</b> for <i>Vaporous</i>: the mist rising at dawn as the soil returns moisture to the air<br>",
      "<b>I</b> for <i>Infiltrating</i>: water silently seeping down to feed invisible aquifers<br>",
      "<b>V</b> for <i>Vivid</i>: bacteria, fungi, insects that no inventory can exhaust<br>",
      "<b>R</b> for <i>Root-bound</i>: the underground network holding slopes, exchanging<br>",
      "<b>E</b> for <i>absorbEnt</i>: absorbing shocks, rains, winds, excesses<br>",
      "<b>E</b> for <i>Emissive</i>: oxygen, resin scents, particles seeding the clouds<br>",
      "<b>N</b> for <i>Nocturnal</i>: the sky reappearing between canopy tops<br>",
      "<b>F</b> for <i>Forested</i>: the simple feeling of being 'in the forest', surrounded<br>",
      "<b>O</b> for <i>Oscillating</i>: cycles, windthrows, rebirths<br>",
      "<b>R</b> for <i>Resonant</i>: the forest responds to what we do to it<br>",
      "<b>E</b> for <i>starlit (Etoil\u00e9e)</i>: each leaf captures light from infinitely far<br>",
      "<b>T</b> for <i>Telluric</i>: anchored in rock and deep soil"
    )
  ),
  summary_tip_conclusion = list(
    fr = "Un contrepoint po\u00e9tique \u00e0 la rigueur de la m\u00e9thode : derri\u00e8re chaque score, il y a une for\u00eat vaporeuse, infiltrante, vivante, racinaire, \u00e9ponge, \u00e9missive, nocturne, foresti\u00e8re, oscillante, r\u00e9sonnante, \u00e9toil\u00e9e, tellurique.",
    en = "A poetic counterpoint to the rigor of the method: behind each score lies a vaporous, infiltrating, vivid, root-bound, absorbent, emissive, nocturnal, forested, oscillating, resonant, starlit, telluric forest."
  ),
  download_pdf = list(fr = "T\u00e9l\u00e9charger le rapport PDF", en = "Download PDF Report"),
  download_gpkg = list(fr = "T\u00e9l\u00e9charger le GeoPackage", en = "Download GeoPackage"),
  generating_pdf = list(fr = "G\u00e9n\u00e9ration du rapport PDF...", en = "Generating PDF report..."),
  pdf_generated = list(fr = "Rapport PDF g\u00e9n\u00e9r\u00e9 avec succ\u00e8s", en = "PDF report generated successfully"),
  owner = list(fr = "Propri\u00e9taire", en = "Owner"),
  global_score = list(fr = "Score global", en = "Global Score"),
  no_project = list(fr = "Aucun projet charg\u00e9", en = "No project loaded"),
  no_data = list(fr = "Pas de donn\u00e9es", en = "No data"),

  # ============================================================
  # Indicator Families
  # ============================================================
  famille_carbone = list(fr = "Carbone & Vitalit\u00e9", en = "Carbon & Vitality"),
  famille_biodiversite = list(fr = "Biodiversit\u00e9", en = "Biodiversity"),
  famille_eau = list(fr = "Eau", en = "Water"),
  famille_air = list(fr = "Air & Microclimat", en = "Air & Microclimate"),
  famille_sol = list(fr = "Fertilit\u00e9 des Sols", en = "Soil Fertility"),
  famille_paysage = list(fr = "Paysage", en = "Landscape"),
  famille_temporel = list(fr = "Dynamique Temporelle", en = "Temporal Dynamics"),
  famille_risque = list(fr = "Risques & R\u00e9silience", en = "Risks & Resilience"),
  famille_social = list(fr = "Social & R\u00e9cr\u00e9atif", en = "Social & Recreational"),
  famille_production = list(fr = "Production", en = "Production"),
  famille_energie = list(fr = "\u00c9nergie & Climat", en = "Energy & Climate"),
  famille_naturalite = list(fr = "Naturalit\u00e9", en = "Naturalness"),

  # Family descriptions
  famille_carbone_desc = list(
    fr = "Stockage de carbone et vitalit\u00e9 de la v\u00e9g\u00e9tation (biomasse, NDVI)",
    en = "Carbon storage and vegetation vitality (biomass, NDVI)"
  ),
  famille_biodiversite_desc = list(
    fr = "Protection, diversit\u00e9 structurale et connectivit\u00e9 \u00e9cologique",
    en = "Protection, structural diversity and ecological connectivity"
  ),
  famille_eau_desc = list(
    fr = "R\u00e9gulation hydrique, zones humides et indice topographique",
    en = "Water regulation, wetlands and topographic index"
  ),
  famille_air_desc = list(
    fr = "Couverture foresti\u00e8re tampon et qualit\u00e9 de l'air",
    en = "Forest cover buffer and air quality"
  ),
  famille_sol_desc = list(
    fr = "Classes de sol et risque d'\u00e9rosion",
    en = "Soil classes and erosion risk"
  ),
  famille_paysage_desc = list(
    fr = "Sylvosph\u00e8re (effet lisi\u00e8re) et fragmentation paysag\u00e8re",
    en = "Sylvosphere (edge effect) and landscape fragmentation"
  ),
  famille_temporel_desc = list(
    fr = "Anciennet\u00e9 foresti\u00e8re et taux de changement",
    en = "Forest age and change rate"
  ),
  famille_risque_desc = list(
    fr = "Risques feu, temp\u00eate, s\u00e9cheresse et abroutissement",
    en = "Fire, storm, drought and browsing risks"
  ),
  famille_social_desc = list(
    fr = "Densit\u00e9 de sentiers, accessibilit\u00e9 et proximit\u00e9 population",
    en = "Trail density, accessibility and population proximity"
  ),
  famille_production_desc = list(
    fr = "Volume de bois, productivit\u00e9 et qualit\u00e9",
    en = "Timber volume, productivity and quality"
  ),
  famille_energie_desc = list(
    fr = "Potentiel bois-\u00e9nergie et \u00e9vitement CO2",
    en = "Wood energy potential and CO2 avoidance"
  ),
  famille_naturalite_desc = list(
    fr = "Distance infrastructures, continuit\u00e9 et score de naturalit\u00e9",
    en = "Infrastructure distance, continuity and naturalness score"
  ),

  # ============================================================
  # Indicator Codes (short labels for table/map display)
  # ============================================================
  indicator_C1 = list(fr = "Biomasse carbone (tC/ha)", en = "Carbon Biomass (tC/ha)"),
  indicator_C2 = list(fr = "NDVI - Vitalit\u00e9", en = "NDVI - Vitality"),
  indicator_B1 = list(fr = "Protection biodiversit\u00e9", en = "Biodiversity Protection"),
  indicator_B2 = list(fr = "Diversit\u00e9 structurale", en = "Structural Diversity"),
  indicator_B3 = list(fr = "Connectivit\u00e9 \u00e9cologique", en = "Ecological Connectivity"),
  indicator_B4 = list(fr = "Diversit\u00e9 spectrale (\u03b1)", en = "Spectral Diversity (\u03b1)"),
  indicator_W1 = list(fr = "R\u00e9seau hydrographique", en = "Water Network"),
  indicator_W2 = list(fr = "Zones humides", en = "Wetlands"),
  indicator_W3 = list(fr = "Indice topographique d'humidit\u00e9", en = "Topographic Wetness Index"),
  indicator_W4 = list(fr = "D\u00e9ficit de pression (VPD)", en = "Vapour Pressure Deficit"),
  indicator_A1 = list(fr = "Tampon forestier", en = "Forest Buffer"),
  indicator_A2 = list(fr = "Qualit\u00e9 de l'air", en = "Air Quality"),
  indicator_A3 = list(fr = "Microclimat (T\u00b0max)", en = "Microclimate (T\u00b0max)"),
  indicator_A4 = list(fr = "Tamponnement thermique", en = "Thermal buffering"),
  indicator_F1 = list(fr = "Fertilit\u00e9 des sols", en = "Soil Fertility"),
  indicator_F2 = list(fr = "Risque d'\u00e9rosion", en = "Erosion Risk"),
  indicator_L1 = list(fr = "Sylvosph\u00e8re (effet lisi\u00e8re)", en = "Sylvosphere (Edge Effect)"),
  indicator_L2 = list(fr = "Fragmentation paysag\u00e8re", en = "Landscape Fragmentation"),
  indicator_L3 = list(fr = "H\u00e9t\u00e9rog\u00e9n\u00e9it\u00e9 spectrale (\u03b2)", en = "Spectral Heterogeneity (\u03b2)"),
  indicator_T1 = list(fr = "Anciennet\u00e9 foresti\u00e8re", en = "Forest Age"),
  indicator_T2 = list(fr = "Taux de changement", en = "Change Rate"),
  indicator_T3 = list(fr = "Coupes rases", en = "Clear-cuts"),
  indicator_R1 = list(fr = "Risque incendie", en = "Fire Risk"),
  indicator_R2 = list(fr = "Risque temp\u00eate", en = "Storm Risk"),
  indicator_R3 = list(fr = "Risque s\u00e9cheresse", en = "Drought Risk"),
  indicator_R4 = list(fr = "Risque abroutissement", en = "Browsing Risk"),
  indicator_R5 = list(fr = "Dépérissement", en = "Dieback"),
  indicator_R6 = list(fr = "Sensibilit\u00e9 microclimatique", en = "Microclimatic sensitivity"),
  indicator_R7 = list(fr = "Risque de gel tardif", en = "Late-frost risk"),
  indicator_S1 = list(fr = "Densit\u00e9 de sentiers", en = "Trail Density"),
  indicator_S2 = list(fr = "Accessibilit\u00e9", en = "Accessibility"),
  indicator_S3 = list(fr = "Proximit\u00e9 population", en = "Population Proximity"),
  indicator_P1 = list(fr = "Volume de bois (m\u00b3/ha)", en = "Timber Volume (m\u00b3/ha)"),
  indicator_P2 = list(fr = "Productivit\u00e9", en = "Productivity"),
  indicator_P3 = list(fr = "Qualit\u00e9 du bois", en = "Timber Quality"),
  indicator_E1 = list(fr = "Bois-\u00e9nergie", en = "Wood Energy"),
  indicator_E2 = list(fr = "\u00c9vitement CO2", en = "CO2 Avoidance"),
  indicator_N1 = list(fr = "Distance infrastructures", en = "Infrastructure Distance"),
  indicator_N2 = list(fr = "Continuit\u00e9 foresti\u00e8re", en = "Forest Continuity"),
  indicator_N3 = list(fr = "Score de naturalit\u00e9", en = "Naturalness Score"),

  # Missing indicators
  missing_indicators_title = list(
    fr = "Indicateurs manquants",
    en = "Missing indicators"
  ),

  # ============================================================
  # Data Table
  # ============================================================
  data_table = list(fr = "Tableau des donn\u00e9es", en = "Data Table"),
  statistics_title = list(fr = "Statistiques", en = "Statistics"),
  comments_title = list(fr = "Commentaires", en = "Comments"),
  comments_markdown_tooltip_title = list(
    fr = "Format Markdown Quarto",
    en = "Quarto Markdown Format"
  ),
  comments_markdown_tooltip = list(
    fr = paste0(
      "Le texte saisi est au format <b>Markdown Quarto</b> et sera int\u00e9gr\u00e9 tel quel dans le rapport PDF.<br><br>",
      "<b>Syntaxe disponible :</b><br>",
      "\u2022 <code>**gras**</code> \u2192 <b>gras</b><br>",
      "\u2022 <code>*italique*</code> \u2192 <i>italique</i><br>",
      "\u2022 <code># Titre</code>, <code>## Sous-titre</code><br>",
      "\u2022 <code>- liste</code> pour les listes \u00e0 puces<br>",
      "\u2022 <code>1. liste</code> pour les listes num\u00e9rot\u00e9es<br>",
      "\u2022 <code>[texte](url)</code> pour les liens<br><br>",
      "<b>Astuce :</b> utilisez les titres pour structurer votre analyse par th\u00e8me (ex. biodiversit\u00e9, production, risques)."
    ),
    en = paste0(
      "Text is in <b>Quarto Markdown</b> format and will be included as-is in the PDF report.<br><br>",
      "<b>Available syntax:</b><br>",
      "\u2022 <code>**bold**</code> \u2192 <b>bold</b><br>",
      "\u2022 <code>*italic*</code> \u2192 <i>italic</i><br>",
      "\u2022 <code># Heading</code>, <code>## Subheading</code><br>",
      "\u2022 <code>- item</code> for bullet lists<br>",
      "\u2022 <code>1. item</code> for numbered lists<br>",
      "\u2022 <code>[text](url)</code> for links<br><br>",
      "<b>Tip:</b> use headings to structure your analysis by theme (e.g. biodiversity, production, risks)."
    )
  ),

  # Analysis
  analysis = list(fr = "Analyse", en = "Analysis"),
  analysis_stats = list(fr = "Statistiques descriptives", en = "Descriptive Statistics"),
  analysis_comments = list(fr = "Observations et critiques", en = "Observations and Critiques"),
  analysis_comments_placeholder = list(
    fr = "Saisissez vos observations sur les r\u00e9sultats...",
    en = "Enter your observations on the results..."
  ),
  synthesis_comments_placeholder = list(
    fr = "Saisissez vos observations sur la synth\u00e8se globale du projet...",
    en = "Enter your observations on the overall project synthesis..."
  ),
  fill_all_label = list(fr = "Tout", en = "All"),
  fill_all_tooltip = list(
    fr = "Cocher pour g\u00e9n\u00e9rer aussi les commentaires des 12 familles d'indicateurs",
    en = "Check to also generate comments for all 12 indicator families"
  ),
  ai_generating_families = list(
    fr = "G\u00e9n\u00e9ration commentaires famille", en = "Generating family comments"
  ),
  stat_min = list(fr = "Min", en = "Min"),
  stat_max = list(fr = "Max", en = "Max"),
  stat_mean = list(fr = "Moyenne", en = "Mean"),
  stat_median = list(fr = "M\u00e9diane", en = "Median"),
  stat_sd = list(fr = "\u00c9cart-type", en = "Std Dev"),
  stat_n = list(fr = "Parcelles", en = "Parcels"),
  stat_na = list(fr = "Valeurs manquantes", en = "Missing values"),
  alert_high_variability = list(
    fr = "Forte variabilit\u00e9 d\u00e9tect\u00e9e (CV > 50%)",
    en = "High variability detected (CV > 50%)"
  ),
  alert_many_na = list(
    fr = "Attention : {n} valeurs manquantes sur {total}",
    en = "Warning: {n} missing values out of {total}"
  ),
  ai_generate = list(fr = "G\u00e9n\u00e9rer par IA", en = "Generate by AI"),
  ai_generating = list(fr = "Analyse en cours...", en = "Analyzing..."),
  ai_error = list(fr = "Erreur lors de l'analyse IA", en = "AI analysis error"),
  ai_no_api_key = list(
    fr = "Cl\u00e9 API non configur\u00e9e. D\u00e9finissez la variable d'environnement {key_var}.",
    en = "API key not configured. Set the {key_var} environment variable."
  ),
  db_not_configured = list(
    fr = "Base de donn\u00e9es non configur\u00e9e \u2014 stockage local des projets (parquet).",
    en = "Database not configured \u2014 projects stored locally (parquet)."
  ),
  # v0.56.0 \u2014 RAG \u00ab perspectives IA sourc\u00e9es \u00bb (cf. brief 2026-06-02,
  # nemeton@v0.62.0). Le titre du bloc \u00ab Sources documentaires \u00bb est
  # rendu par `nemeton::format_citations(lang=)` c\u00f4t\u00e9 c\u0153ur \u2014 ne pas
  # le red\u00e9finir ici. `rag_sourced_badge` : phrase au-dessus du
  # markdown des citations, signale explicitement la nature sourc\u00e9e
  # de la perspective. `rag_toggle_label` r\u00e9serv\u00e9 pour un futur
  # checkbox UI (V2 \u2014 V1 n'expose pas de toggle utilisateur).
  rag_sourced_badge = list(
    fr = "Perspective appuy\u00e9e sur %d source(s) documentaire(s).",
    en = "Perspective backed by %d documentary source(s)."
  ),
  rag_sources_heading = list(
    fr = "Sources documentaires",
    en = "Documentary sources"
  ),
  rag_no_sources_note = list(
    fr = "Perspective g\u00e9n\u00e9r\u00e9e sans sources documentaires : le corpus de connaissances est indisponible ou vide (base PostgreSQL/pgvector non configur\u00e9e ou cl\u00e9 d'embedding absente \u2014 le RAG n'est pas disponible sur une base locale SQLite).",
    en = "Perspective generated without documentary sources: the knowledge corpus is unavailable or empty (no PostgreSQL/pgvector database configured or no embedding key \u2014 the RAG is not available on a local SQLite database)."
  ),
  rag_toggle_label = list(
    fr = "Inclure les sources documentaires",
    en = "Include documentary sources"
  ),

  # Expert profiles
  expert_label = list(fr = "Point de vue", en = "Perspective"),

  # ============================================================
  # Missing Indicators
  # ============================================================
  missing_indicator = list(
    fr = "Indicateur non disponible : {reason}",
    en = "Indicator not available: {reason}"
  ),
  missing_lidar = list(fr = "Donn\u00e9es LiDAR non disponibles", en = "LiDAR data not available"),
  missing_connection = list(
    fr = "Connexion au serveur impossible",
    en = "Unable to connect to server"
  ),
  missing_data = list(fr = "Donn\u00e9es source manquantes", en = "Source data missing"),

  # ============================================================
  # Help & Tour
  # ============================================================
  help = list(fr = "Aide", en = "Help"),
  help_title = list(fr = "Guide d'utilisation", en = "User Guide"),
  help_intro = list(
    fr = "N\u00e9m\u00e9ton vous permet d'analyser des parcelles foresti\u00e8res selon 12 familles d'indicateurs.",
    en = "N\u00e9m\u00e9ton allows you to analyze forest parcels across 12 indicator families."
  ),
  help_steps_title = list(fr = "\u00c9tapes d'utilisation", en = "Usage Steps"),
  help_step1 = list(
    fr = "S\u00e9lectionnez un d\u00e9partement puis une commune",
    en = "Select a department then a municipality"
  ),
  help_step2 = list(
    fr = "Cliquez sur les parcelles \u00e0 analyser (max 20)",
    en = "Click on parcels to analyze (max 20)"
  ),
  help_step3 = list(
    fr = "Renseignez les informations du projet",
    en = "Fill in project information"
  ),
  help_step4 = list(
    fr = "Lancez les calculs et attendez la fin",
    en = "Start calculations and wait for completion"
  ),
  help_step5 = list(
    fr = "Consultez les r\u00e9sultats et t\u00e9l\u00e9chargez le rapport",
    en = "View results and download the report"
  ),
  tour_restart = list(fr = "Relancer le tour guid\u00e9", en = "Restart guided tour"),
  documentation_link = list(
    fr = "Consulter la documentation compl\u00e8te",
    en = "View full documentation"
  ),
  close = list(fr = "Fermer", en = "Close"),

  # Tour steps
  tour_search_title = list(fr = "Recherche de commune", en = "Municipality Search"),
  tour_search_desc = list(
    fr = "S\u00e9lectionnez d'abord un d\u00e9partement, puis recherchez votre commune par nom ou code postal.",
    en = "First select a department, then search for your municipality by name or postal code."
  ),
  tour_map_title = list(fr = "S\u00e9lection des parcelles", en = "Parcel Selection"),
  tour_map_desc = list(
    fr = "Cliquez sur les parcelles cadastrales pour les s\u00e9lectionner. Un second clic les d\u00e9s\u00e9lectionne. Maximum {max} parcelles.",
    en = "Click on cadastral parcels to select them. A second click deselects them. Maximum {max} parcels."
  ),
  tour_project_title = list(fr = "Nom du projet", en = "Project Name"),
  tour_project_desc = list(
    fr = "Donnez un nom \u00e0 votre projet. Ce champ est obligatoire.",
    en = "Give your project a name. This field is required."
  ),
  tour_description_title = list(fr = "Description", en = "Description"),
  tour_description_desc = list(
    fr = "Ajoutez une description optionnelle pour mieux identifier votre projet.",
    en = "Add an optional description to better identify your project."
  ),
  tour_owner_title = list(fr = "Propri\u00e9taire", en = "Owner"),
  tour_owner_desc = list(
    fr = "Indiquez le nom du propri\u00e9taire ou gestionnaire (optionnel).",
    en = "Enter the name of the owner or manager (optional)."
  ),
  tour_create_title = list(fr = "Cr\u00e9er le projet", en = "Create Project"),
  tour_create_desc = list(
    fr = "Cliquez sur ce bouton pour cr\u00e9er votre projet et passer \u00e0 l'\u00e9tape suivante.",
    en = "Click this button to create your project and proceed to the next step."
  ),
  tour_compute_title = list(fr = "Lancement des calculs", en = "Start Calculations"),
  tour_compute_desc = list(
    fr = "Une fois vos parcelles s\u00e9lectionn\u00e9es et le projet nomm\u00e9, lancez les calculs. Ils s'ex\u00e9cutent en arri\u00e8re-plan.",
    en = "Once parcels are selected and project named, start calculations. They run in the background."
  ),

  # ----- Tour : 1 step clé par onglet (spec tour v2) -----
  tour_synthesis_title = list(fr = "Synth\u00e8se", en = "Synthesis"),
  tour_synthesis_desc = list(
    fr = "Le score global, le radar des 12 familles et la perspective IA par profil expert. G\u00e9n\u00e9rez une analyse adapt\u00e9e \u00e0 votre m\u00e9tier.",
    en = "The overall score, the 12-family radar and the AI perspective per expert profile. Generate an analysis tailored to your role."
  ),
  tour_action_plan_title = list(fr = "Plan d'action", en = "Action plan"),
  tour_action_plan_desc = list(
    fr = "\u00c0 partir du diagnostic, l'assistant propose des actions sylvicoles prioris\u00e9es et un \u00e9change conversationnel pour les affiner.",
    en = "From the diagnosis, the assistant proposes prioritised silvicultural actions and a conversational thread to refine them."
  ),
  tour_terrain_title = list(fr = "Terrain", en = "Field"),
  tour_terrain_desc = list(
    fr = "Pr\u00e9parez un plan d'\u00e9chantillonnage (GRTS / TSP) export\u00e9 vers QGIS / QField, puis r\u00e9importez les observations terrain.",
    en = "Prepare a sampling plan (GRTS / TSP) exported to QGIS / QField, then re-import the field observations."
  ),
  tour_monitoring_title = list(fr = "Suivi sanitaire", en = "Health monitoring"),
  tour_monitoring_desc = list(
    fr = "Trois modes de d\u00e9tection du d\u00e9p\u00e9rissement : FAST (chocs r\u00e9cents), FORDEAD (r\u00e9sineux / scolyte) et RECONFORT (feuillus). Choisissez le mode ici.",
    en = "Three dieback-detection modes: FAST (recent shocks), FORDEAD (conifers / bark beetle) and RECONFORT (broadleaves). Pick the mode here."
  ),
  tour_families_title = list(fr = "Familles d'indicateurs", en = "Indicator families"),
  tour_families_desc = list(
    fr = "Chaque famille (Carbone, Biodiversit\u00e9, Eau\u2026) d\u00e9taille ses indicateurs, ses cartes et ses statistiques. Explorez-les depuis ce menu.",
    en = "Each family (Carbon, Biodiversity, Water\u2026) details its indicators, maps and statistics. Browse them from this menu."
  ),

  # ============================================================
  # ============================================================
  # Errors
  # ============================================================
  error_api_cadastre = list(
    fr = "API Cadastre indisponible, utilisation de la source alternative...",
    en = "Cadastre API unavailable, using fallback source..."
  ),
  error_no_parcels = list(
    fr = "Aucune parcelle trouv\u00e9e pour cette commune",
    en = "No parcels found for this municipality"
  ),
  error_invalid_postal = list(
    fr = "Code postal invalide (5 chiffres requis)",
    en = "Invalid postal code (5 digits required)"
  ),
  error_computation = list(
    fr = "Erreur lors du calcul des indicateurs",
    en = "Error computing indicators"
  ),
  error_no_internet = list(
    fr = "Pas de connexion internet. V\u00e9rifiez votre connexion et r\u00e9essayez.",
    en = "No internet connection. Check your connection and try again."
  ),
  error_loading_communes = list(
    fr = "Erreur lors du chargement des communes : ",
    en = "Error loading municipalities: "
  ),

  # ============================================================
  # Corrupted Projects
  # ============================================================
  project_corrupted_title = list(fr = "Projet corrompu", en = "Corrupted Project"),
  project_corrupted_message = list(
    fr = "Ce projet est corrompu ou incomplet. Voulez-vous le supprimer ?",
    en = "This project is corrupted or incomplete. Do you want to delete it?"
  ),
  delete = list(fr = "Supprimer", en = "Delete"),
  delete_project = list(fr = "Supprimer le projet", en = "Delete project"),
  confirm_delete_project = list(
    fr = "Voulez-vous vraiment supprimer le projet",
    en = "Are you sure you want to delete the project"
  ),
  delete_project_warning = list(
    fr = "Cette action est irr\u00e9versible. Toutes les donn\u00e9es du projet seront perdues.",
    en = "This action cannot be undone. All project data will be lost."
  ),
  cancel = list(fr = "Annuler", en = "Cancel"),
  confirm = list(fr = "Confirmer", en = "Confirm"),

  # ============================================================
  # Progress Messages - Data Sources
  # ============================================================
  downloading_source = list(
    fr = "T\u00e9l\u00e9chargement : {source}",
    en = "Downloading: {source}"
  ),
  computing_indicator_name = list(
    fr = "Calcul de l'indicateur : {indicator}",
    en = "Computing indicator: {indicator}"
  ),
  download_complete = list(
    fr = "T\u00e9l\u00e9chargement termin\u00e9",
    en = "Download complete"
  ),

  # Data source names
  source_ndvi = list(fr = "NDVI (IRC IGN)", en = "NDVI (IGN IRC)"),
  source_dem = list(fr = "Mod\u00e8le Num\u00e9rique de Terrain", en = "Digital Elevation Model"),
  source_forest_cover = list(fr = "Occupation du sol (OSO Theia/CESBIO)", en = "Land Cover (OSO Theia/CESBIO)"),
  source_protected_areas = list(fr = "Aires prot\u00e9g\u00e9es (INPN)", en = "Protected Areas (INPN)"),
  source_water_network = list(fr = "R\u00e9seau hydrographique (IGN)", en = "Water Network (IGN)"),
  source_water_surfaces = list(fr = "Surfaces hydrographiques (IGN)", en = "Water Surfaces (IGN)"),
  source_wetlands = list(fr = "Zones humides (ZNIEFF)", en = "Wetlands (ZNIEFF)"),
  source_roads = list(fr = "R\u00e9seau routier (IGN)", en = "Road Network (IGN)"),
  source_buildings = list(fr = "B\u00e2timents (BD TOPO)", en = "Buildings (BD TOPO)"),
  source_bdforet = list(fr = "BD For\u00eat V2 - Formations v\u00e9g\u00e9tales (IGN)", en = "BD For\u00eat V2 - Vegetation Formations (IGN)"),
  source_lidar_mnh = list(fr = "Hauteur de canop\u00e9e LiDAR HD (IGN)", en = "Canopy Height Model LiDAR HD (IGN)"),
  source_lidar_mnt = list(fr = "Mod\u00e8le Num\u00e9rique de Terrain LiDAR HD (IGN)", en = "Digital Terrain Model LiDAR HD (IGN)"),
  source_lidar_copc = list(fr = "Nuages de points LiDAR HD (IGN)", en = "LiDAR HD Point Clouds (IGN)"),
  chm_inference_opencanopy = list(
    fr = "Inf\u00e9rence CHM Open-Canopy (peut prendre plusieurs minutes)\u2026",
    en = "Open-Canopy CHM inference (may take several minutes)\u2026"
  ),
  chm_tile_start = list(
    fr = "T\u00e9l\u00e9chargement ortho IGN {type} : {n} tuile(s)\u2026",
    en = "Downloading IGN {type} ortho: {n} tile(s)\u2026"
  ),
  chm_tile = list(
    fr = "T\u00e9l\u00e9chargement ortho IGN {type} : tuile {idx}/{n}\u2026",
    en = "Downloading IGN {type} ortho: tile {idx}/{n}\u2026"
  ),
  # Open-Canopy pipeline phases — 5 steps
  chm_phase_load_aoi = list(
    fr = "\u00c9tape 1/5 : chargement de l'AOI\u2026",
    en = "Step 1/5: loading AOI\u2026"
  ),
  chm_phase_download_ortho = list(
    fr = "\u00c9tape 2/5 : t\u00e9l\u00e9chargement ortho IGN\u2026",
    en = "Step 2/5: downloading IGN ortho\u2026"
  ),
  chm_phase_setup_python = list(
    fr = "\u00c9tape 3/5 : configuration Python\u2026",
    en = "Step 3/5: Python setup\u2026"
  ),
  chm_phase_download_model = list(
    fr = "\u00c9tape 3/5 : t\u00e9l\u00e9chargement mod\u00e8le {model}\u2026",
    en = "Step 3/5: downloading model {model}\u2026"
  ),
  chm_phase_inference = list(
    fr = "\u00c9tape 4/5 : inf\u00e9rence du mod\u00e8le {model}\u2026",
    en = "Step 4/5: running model {model}\u2026"
  ),
  chm_phase_export = list(
    fr = "\u00c9tape 5/5 : export des r\u00e9sultats\u2026",
    en = "Step 5/5: exporting results\u2026"
  ),
  chm_phase_lidar_hd_download = list(
    fr = "T\u00e9l\u00e9chargement CHM LiDAR HD (IGN)\u2026",
    en = "Downloading LiDAR HD CHM (IGN)\u2026"
  ),
  chm_phase_theia_formspot = list(
    fr = "T\u00e9l\u00e9chargement CHM Theia FORMSpoT\u2026",
    en = "Downloading Theia FORMSpoT CHM\u2026"
  ),
  chm_inference_start = list(
    fr = "Inf\u00e9rence CHM : {n} tuile(s)\u2026",
    en = "CHM inference: {n} tile(s)\u2026"
  ),
  chm_inference_tile = list(
    fr = "Inf\u00e9rence CHM : tuile {idx}/{n}\u2026",
    en = "CHM inference: tile {idx}/{n}\u2026"
  ),
  chm_phase_lasr_fallback = list(
    fr = "D\u00e9rivation CHM via lasR depuis les nuages LiDAR HD locaux\u2026",
    en = "Deriving CHM via lasR from cached LiDAR HD point clouds\u2026"
  ),
  chm_fallback_lasr_start = list(
    fr = "Bascule sur lasR pour d\u00e9river le CHM depuis {n} dalle(s) LiDAR HD locale(s)\u2026",
    en = "Falling back to lasR to derive the CHM from {n} cached LiDAR HD tile(s)\u2026"
  ),
  chm_fallback_lasr_success = list(
    fr = "CHM d\u00e9riv\u00e9 depuis le nuage de points LiDAR HD en {sec}s.",
    en = "CHM derived from LiDAR HD point cloud in {sec}s."
  ),
  chm_fallback_lasr_skip_no_tiles = list(
    fr = "Aucune dalle .laz locale, pas de fallback lasR.",
    en = "No local .laz tile, lasR fallback skipped."
  ),
  chm_fallback_lasr_skip_no_pkg = list(
    fr = "Package lasR non install\u00e9, pas de fallback lasR. Installer : install.packages('lasR', repos = 'https://r-lidar.r-universe.dev')",
    en = "Package lasR is not installed, lasR fallback skipped. Install with: install.packages('lasR', repos = 'https://r-lidar.r-universe.dev')"
  ),

  # ============================================================
  # External API keys configuration (Theia + LLM, tabbed modal)
  # ============================================================
  api_keys_config_open = list(
    fr = "Configuration des clés API externes",
    en = "External API keys configuration"
  ),
  api_keys_config_title = list(
    fr = "Paramètres : clés API & corpus RAG",
    en = "Settings: API keys & RAG corpus"
  ),
  api_keys_config_intro = list(
    fr = "Configurez ici les services externes de l'application : Theia / DATA TERRA pour le CHM FORMSpoT, un fournisseur LLM (Mistral, Anthropic ou OpenAI) pour les perspectives d'expert, et le corpus RAG de connaissances qui source ces perspectives.",
    en = "Configure here the application's external services: Theia / DATA TERRA for the FORMSpoT CHM, an LLM provider (Mistral, Anthropic or OpenAI) for the expert perspectives, and the RAG knowledge corpus that sources those perspectives."
  ),
  api_keys_tab_theia = list(
    fr = "Theia / DATA TERRA",
    en = "Theia / DATA TERRA"
  ),
  api_keys_tab_llm = list(
    fr = "Fournisseur LLM",
    en = "LLM provider"
  ),
  api_keys_tab_rag = list(
    fr = "Corpus RAG",
    en = "RAG corpus"
  ),
  api_keys_fullscreen = list(
    fr = "Plein écran",
    en = "Fullscreen"
  ),
  # LLM tab
  llm_config_intro = list(
    fr = "Sélectionnez le fournisseur LLM puis enregistrez sa clé API. Une seule clé par fournisseur ; les autres restent inchangées.",
    en = "Pick the LLM provider then store its API key. One key per provider; the other ones stay untouched."
  ),
  llm_provider_label = list(
    fr = "Fournisseur",
    en = "Provider"
  ),
  llm_status_label = list(
    fr = "Clé API LLM",
    en = "LLM API key"
  ),
  # v0.51.8 — ligne résumé au-dessus du selectInput (vue d'ensemble
  # « N/3 configurés : Mistral, OpenAI »).
  llm_summary_configured_fmt = list(
    fr = "%d / %d fournisseurs configurés : %s.",
    en = "%d of %d providers configured: %s."
  ),
  llm_summary_none_configured = list(
    fr = "Aucun fournisseur configuré.",
    en = "No provider configured yet."
  ),
  llm_status_ok = list(
    fr = "Configurée pour %s.",
    en = "Configured for %s."
  ),
  llm_status_ko = list(
    fr = "Aucune clé configurée pour %s.",
    en = "No key configured for %s."
  ),
  llm_status_source_env = list(
    fr = "Source : variable d'environnement %s.",
    en = "Source: environment variable %s."
  ),
  llm_status_source_file = list(
    fr = "Source : ~/.config/nemetonshiny/llm.json.",
    en = "Source: ~/.config/nemetonshiny/llm.json."
  ),
  llm_key_label = list(
    fr = "Clé API",
    en = "API key"
  ),
  llm_key_help = list(
    fr = "Stockée dans ~/.config/nemetonshiny/llm.json (chmod 0600) et exposée comme variable d'environnement dans la session R en cours.",
    en = "Stored in ~/.config/nemetonshiny/llm.json (chmod 0600) and exposed as an environment variable in the running R session."
  ),
  llm_key_save = list(
    fr = "Enregistrer la clé",
    en = "Save key"
  ),
  llm_key_saved = list(
    fr = "Clé LLM enregistrée.",
    en = "LLM key saved."
  ),
  llm_key_save_failed = list(
    fr = "Échec de l'enregistrement de la clé LLM.",
    en = "Failed to save the LLM key."
  ),
  llm_key_missing = list(
    fr = "Renseignez une clé API.",
    en = "Enter an API key."
  ),
  llm_key_configured_hint = list(
    fr = "Clé enregistrée. Tu peux la modifier ou la supprimer.",
    en = "Key stored. You can edit or delete it."
  ),
  llm_key_edit = list(
    fr = "Modifier la clé",
    en = "Edit key"
  ),
  llm_key_delete = list(
    fr = "Supprimer la clé",
    en = "Delete key"
  ),
  llm_key_deleted = list(
    fr = "Clé LLM supprimée.",
    en = "LLM key deleted."
  ),

  # Theia / DATA TERRA configuration (legacy keys preserved)
  # ============================================================
  theia_config_open = list(
    fr = "Configuration Theia / DATA TERRA",
    en = "Theia / DATA TERRA settings"
  ),
  theia_config_title = list(
    fr = "Sources de données Theia / DATA TERRA",
    en = "Theia / DATA TERRA data sources"
  ),
  theia_config_intro = list(
    fr = "Theia / DATA TERRA fournit des données satellitaires publiques (FORMSpoT, biophysique, sols, neige…) utilisées pour calculer les indicateurs en NDP 0.",
    en = "Theia / DATA TERRA provides public satellite data (FORMSpoT, biophysical, soil, snow…) used to compute the indicators at NDP 0."
  ),
  theia_python_status_label = list(
    fr = "Pré-requis Python / reticulate",
    en = "Python / reticulate prerequisite"
  ),
  theia_key_status_label = list(
    fr = "Clé API Theia",
    en = "Theia API key"
  ),
  theia_status_ready = list(
    fr = "Theia est prêt : les sources satellitaires sont accessibles.",
    en = "Theia is ready: satellite sources are accessible."
  ),
  theia_error_reticulate = list(
    fr = "Le paquet R 'reticulate' est absent. Installez-le pour accéder aux sources Theia.",
    en = "The R package 'reticulate' is missing. Install it to access Theia sources."
  ),
  theia_error_python_modules = list(
    fr = "Modules Python 'teledetection' / 'pystac_client' indisponibles. Ils sont déclarés automatiquement par reticulate::py_require().",
    en = "Python modules 'teledetection' / 'pystac_client' unavailable. They are declared automatically by reticulate::py_require()."
  ),
  theia_error_no_key = list(
    fr = "Aucune clé API Theia configurée. Renseignez-la ci-dessous.",
    en = "No Theia API key configured. Enter it below."
  ),
  # v0.46.4 — wording honnête du panel Theia config. theia_python_ok
  # remplace l'ancienne réutilisation de theia_status_ready qui
  # mentait sur la dispo des modules Python (cf. fix
  # theia_python_ready : on ne probe plus py_module_available, donc
  # « Python ok » signifie juste « reticulate installé »).
  theia_python_ok = list(
    fr = "reticulate est installé. Les modules Python (teledetection, pystac_client) sont provisionnés automatiquement au premier accès Theia.",
    en = "reticulate is installed. The Python modules (teledetection, pystac_client) are provisioned automatically on first Theia access."
  ),
  theia_key_ok = list(
    fr = "Clé API Theia configurée.",
    en = "Theia API key configured."
  ),
  # v0.46.4 — message diagnostic quand toute la config Theia est OK
  # (reticulate + clé) mais load_theia_source() échoue à l'exécution.
  # Distinct de theia_error_no_key / theia_error_reticulate.
  theia_chm_load_failed = list(
    fr = "La clé API Theia est configurée mais le chargement du CHM FORMSpoT a échoué (réseau, environnement Python, ou aucune donnée FORMSpoT pour cette zone). Voir les avertissements de téléchargement pour le détail.",
    en = "The Theia API key is configured but the FORMSpoT CHM download failed (network, Python environment, or no FORMSpoT data for this area). See the download warnings for details."
  ),
  theia_key_label_access = list(fr = "Clé d'accès (access key)", en = "Access key"),
  theia_key_label_secret = list(fr = "Clé secrète (secret key)", en = "Secret key"),
  theia_key_help = list(
    fr = "Créez une clé API sur https://gate.stac.teledetection.fr",
    en = "Create an API key at https://gate.stac.teledetection.fr"
  ),
  theia_key_save = list(fr = "Enregistrer la clé", en = "Save key"),
  theia_key_saved = list(
    fr = "Clé API Theia enregistrée.",
    en = "Theia API key saved."
  ),
  theia_key_save_failed = list(
    fr = "Échec de l'enregistrement de la clé API Theia.",
    en = "Failed to save the Theia API key."
  ),
  theia_key_missing = list(
    fr = "Renseignez la clé d'accès et la clé secrète.",
    en = "Enter both the access key and the secret key."
  ),
  # v0.51.6 — sous-section "clé déjà configurée" : Modifier / Supprimer.
  theia_key_configured_hint = list(
    fr = "Clé enregistrée dans ~/.config/teledetection/.apikey (chmod 0600). Tu peux la modifier ou la supprimer.",
    en = "Key stored in ~/.config/teledetection/.apikey (chmod 0600). You can edit or delete it."
  ),
  theia_key_edit = list(
    fr = "Modifier la clé",
    en = "Edit key"
  ),
  theia_key_delete = list(
    fr = "Supprimer la clé",
    en = "Delete key"
  ),
  theia_key_deleted = list(
    fr = "Clé API Theia supprimée.",
    en = "Theia API key deleted."
  ),
  theia_provenance_title = list(
    fr = "Provenance et licence des sources Theia",
    en = "Theia source provenance and licensing"
  ),
  theia_provenance_empty = list(
    fr = "Métadonnées de provenance indisponibles.",
    en = "Provenance metadata unavailable."
  ),
  theia_col_source = list(fr = "Source", en = "Source"),
  theia_col_provenance = list(fr = "Provenance", en = "Provenance"),
  theia_col_consumed_by = list(fr = "Indicateurs", en = "Consumed by"),
  theia_col_license = list(fr = "Licence", en = "License"),
  theia_chm_unavailable = list(
    fr = "CHM Theia FORMSpoT indisponible : pré-requis Python ou clé API Theia manquant.",
    en = "Theia FORMSpoT CHM unavailable: missing Python prerequisite or Theia API key."
  ),
  compute_chm_required = list(
    fr = "Modèle de hauteur de canopée (CHM) indisponible. Les indicateurs Production (P1/P2/P3) et Bois-énergie (E1) nécessitent un CHM (Theia FORMSpoT, LiDAR HD ou Open-Canopy). Configurez la clé API Theia via le menu de configuration.",
    en = "Canopy height model (CHM) unavailable. The Production indicators (P1/P2/P3) and Wood energy (E1) require a CHM (Theia FORMSpoT, LiDAR HD or Open-Canopy). Configure the Theia API key via the settings menu."
  ),

  # ============================================================
  # Progress Messages - Indicator Names
  # ============================================================
  indicateur_c1_biomasse = list(fr = "Biomasse carbone", en = "Carbon Biomass"),
  indicateur_c2_ndvi = list(fr = "NDVI - Vitalit\u00e9 v\u00e9g\u00e9tation", en = "NDVI - Vegetation Vitality"),
  indicateur_b1_protection = list(fr = "Protection biodiversit\u00e9", en = "Biodiversity Protection"),
  indicateur_b2_structure = list(fr = "Structure biodiversit\u00e9", en = "Biodiversity Structure"),
  indicateur_b3_connectivite = list(fr = "Connectivit\u00e9 \u00e9cologique", en = "Ecological Connectivity"),
  indicateur_b4_div_spectrale = list(fr = "Diversit\u00e9 spectrale (\u03b1)", en = "Spectral Diversity (\u03b1)"),
  indicateur_w1_reseau = list(fr = "R\u00e9seau hydrographique", en = "Water Network"),
  indicateur_w2_zones_humides = list(fr = "Zones humides", en = "Wetlands"),
  indicateur_w3_humidite = list(fr = "Indice topographique d'humidit\u00e9", en = "Topographic Wetness Index"),
  indicateur_a1_couverture = list(fr = "Tampon forestier", en = "Forest Buffer"),
  indicateur_a2_qualite_air = list(fr = "Qualit\u00e9 de l'air", en = "Air Quality"),
  indicateur_f2_erosion = list(fr = "Risque d'\u00e9rosion (RUSLE)", en = "Erosion Risk (RUSLE)"),
  indicateur_f1_fertilite = list(fr = "Fertilit\u00e9 des sols (TWI+pente)", en = "Soil Fertility (TWI+slope)"),
  indicateur_l1_sylvosphere = list(fr = "Sylvosph\u00e8re (effet lisi\u00e8re)", en = "Sylvosphere (Edge Effect)"),
  indicateur_l2_fragmentation = list(fr = "Fragmentation paysag\u00e8re", en = "Landscape Fragmentation"),
  indicateur_l3_het_spectrale = list(fr = "H\u00e9t\u00e9rog\u00e9n\u00e9it\u00e9 spectrale (\u03b2)", en = "Spectral Heterogeneity (\u03b2)"),
  indicateur_t1_anciennete = list(fr = "Anciennet\u00e9 foresti\u00e8re", en = "Forest Age"),
  indicateur_t2_changement = list(fr = "Taux de changement", en = "Change Rate"),
  indicateur_r1_feu = list(fr = "Risque incendie", en = "Fire Risk"),
  indicateur_r2_tempete = list(fr = "Risque temp\u00eate", en = "Storm Risk"),
  indicateur_r3_secheresse = list(fr = "Risque s\u00e9cheresse", en = "Drought Risk"),
  indicateur_r4_abroutissement = list(fr = "Risque abroutissement", en = "Browsing Risk"),
  indicateur_r5_deperissement = list(fr = "D\u00e9p\u00e9rissement", en = "Dieback"),
  indicateur_s1_routes = list(fr = "Densit\u00e9 de sentiers", en = "Trail Density"),
  indicateur_s2_bati = list(fr = "Accessibilit\u00e9", en = "Accessibility"),
  indicateur_s3_population = list(fr = "Proximit\u00e9 population", en = "Population Proximity"),
  indicateur_p1_volume = list(fr = "Volume de bois", en = "Timber Volume"),
  indicateur_p2_station = list(fr = "Productivit\u00e9", en = "Productivity"),
  indicateur_p3_qualite_bois = list(fr = "Qualit\u00e9 du bois", en = "Timber Quality"),
  indicateur_e1_bois_energie = list(fr = "Bois-\u00e9nergie", en = "Wood Energy"),
  indicateur_e2_evitement = list(fr = "\u00c9vitement CO2", en = "CO2 Avoidance"),
  indicateur_n1_distance = list(fr = "Distance infrastructures", en = "Infrastructure Distance"),
  indicateur_n2_continuite = list(fr = "Continuit\u00e9 foresti\u00e8re", en = "Forest Continuity"),
  indicateur_n3_naturalite = list(fr = "Score de naturalit\u00e9", en = "Naturalness Score"),

  # ============================================================
  # NDP (Niveau De Precision)
  # ============================================================
  ndp_label = list(fr = "Niveau de Pr\u00e9cision", en = "Precision Level"),
  ndp_confidence = list(fr = "Confiance \u03c6", en = "Confidence \u03c6"),
  ndp_decouverte = list(fr = "D\u00e9couverte", en = "Discovery"),
  ndp_observation = list(fr = "Observation", en = "Observation"),
  ndp_exploration = list(fr = "Exploration", en = "Exploration"),
  ndp_diagnostic = list(fr = "Diagnostic", en = "Diagnostic"),
  ndp_jumeau = list(fr = "Jumeau num\u00e9rique", en = "Digital Twin"),
  ndp_tip_title = list(fr = "Niveau De Pr\u00e9cision (NDP)", en = "Precision Level (NDP)"),
  ndp_tip_subtitle = list(
    fr = "Mesure la qualit\u00e9 des donn\u00e9es d'entr\u00e9e, pas la compl\u00e9tude de l'analyse",
    en = "Measures input data quality, not analysis completeness"
  ),
  ndp_tip_intro = list(
    fr = "Le NDP \u00e9value la fiabilit\u00e9 des donn\u00e9es utilis\u00e9es pour calculer les 31 indicateurs. Les 12 familles sont <strong>toujours</strong> calcul\u00e9es, mais avec une pr\u00e9cision croissante selon les sources disponibles.",
    en = "The NDP evaluates the reliability of data used to compute the 31 indicators. All 12 families are <strong>always</strong> calculated, but with increasing precision as better data sources become available."
  ),
  ndp_tip_levels_title = list(fr = "Les 5 niveaux", en = "The 5 levels"),
  ndp_tip_levels = list(
    fr = "<table style='width:100%; border-collapse:collapse; margin:6px 0; font-size:0.82rem;'><tr style='background:#e8f0e4;'><th style='padding:4px 6px; text-align:left;'>NDP</th><th style='padding:4px 6px; text-align:left;'>Nom</th><th style='padding:4px 6px; text-align:center;'>Poids</th><th style='padding:4px 6px; text-align:center;'>\u03c6</th></tr><tr><td style='padding:3px 6px;'><strong>0</strong></td><td>D\u00e9couverte</td><td style='text-align:center;'>1</td><td style='text-align:center;'>8.3%</td></tr><tr style='background:#f8f8f8;'><td style='padding:3px 6px;'><strong>1</strong></td><td>Observation</td><td style='text-align:center;'>1</td><td style='text-align:center;'>16.7%</td></tr><tr><td style='padding:3px 6px;'><strong>2</strong></td><td>Exploration</td><td style='text-align:center;'>2</td><td style='text-align:center;'>33.3%</td></tr><tr style='background:#f8f8f8;'><td style='padding:3px 6px;'><strong>3</strong></td><td>Diagnostic</td><td style='text-align:center;'>3</td><td style='text-align:center;'>58.3%</td></tr><tr><td style='padding:3px 6px;'><strong>4</strong></td><td>Jumeau</td><td style='text-align:center;'>5</td><td style='text-align:center;'>100%</td></tr></table>",
    en = "<table style='width:100%; border-collapse:collapse; margin:6px 0; font-size:0.82rem;'><tr style='background:#e8f0e4;'><th style='padding:4px 6px; text-align:left;'>NDP</th><th style='padding:4px 6px; text-align:left;'>Name</th><th style='padding:4px 6px; text-align:center;'>Weight</th><th style='padding:4px 6px; text-align:center;'>\u03c6</th></tr><tr><td style='padding:3px 6px;'><strong>0</strong></td><td>Discovery</td><td style='text-align:center;'>1</td><td style='text-align:center;'>8.3%</td></tr><tr style='background:#f8f8f8;'><td style='padding:3px 6px;'><strong>1</strong></td><td>Observation</td><td style='text-align:center;'>1</td><td style='text-align:center;'>16.7%</td></tr><tr><td style='padding:3px 6px;'><strong>2</strong></td><td>Exploration</td><td style='text-align:center;'>2</td><td style='text-align:center;'>33.3%</td></tr><tr style='background:#f8f8f8;'><td style='padding:3px 6px;'><strong>3</strong></td><td>Diagnostic</td><td style='text-align:center;'>3</td><td style='text-align:center;'>58.3%</td></tr><tr><td style='padding:3px 6px;'><strong>4</strong></td><td>Digital Twin</td><td style='text-align:center;'>5</td><td style='text-align:center;'>100%</td></tr></table>"
  ),
  ndp_tip_fibonacci_title = list(fr = "Pond\u00e9ration Fibonacci", en = "Fibonacci Weighting"),
  ndp_tip_fibonacci_text = list(
    fr = "L'indice g\u00e9n\u00e9ral est une moyenne pond\u00e9r\u00e9e par les poids de Fibonacci (1, 1, 2, 3, 5). Plus le NDP est \u00e9lev\u00e9, plus les indicateurs p\u00e8sent dans le score final. La suite de Fibonacci refl\u00e8te la croissance naturelle \u2014 chaque niveau apporte autant que les deux pr\u00e9c\u00e9dents combin\u00e9s.",
    en = "The general index is a weighted mean using Fibonacci weights (1, 1, 2, 3, 5). The higher the NDP, the more weight indicators carry in the final score. The Fibonacci sequence reflects natural growth \u2014 each level contributes as much as the two previous levels combined."
  ),
  ndp_tip_confidence_title = list(fr = "Confiance \u03c6 (nombre d'or)", en = "Confidence \u03c6 (golden ratio)"),
  ndp_tip_confidence_text = list(
    fr = "Le ratio \u03c6 exprime la confiance cumul\u00e9e : rapport du poids Fibonacci cumul\u00e9 sur le total (12). \u00c0 NDP 0 : 8.3% de confiance \u2014 \u00e0 NDP 4 : 100%. Ce ratio est affich\u00e9 sous le score global dans la barre de progression.",
    en = "The \u03c6 ratio expresses cumulative confidence: the ratio of cumulative Fibonacci weight to the total (12). At NDP 0: 8.3% confidence \u2014 at NDP 4: 100%. This ratio is displayed below the global score in the progress bar."
  ),
  ndp_tip_conclusion = list(
    fr = "L'application utilise des donn\u00e9es publiques (Sentinel-2, BD TOPO, LiDAR HD IGN). Le NDP augmente automatiquement lorsque des sources de meilleure r\u00e9solution sont disponibles (drone, inventaire terrain, scanner 3D).",
    en = "The application uses public data (Sentinel-2, BD TOPO, IGN LiDAR HD). The NDP increases automatically when higher-resolution data sources become available (drone, field inventory, 3D scanner)."
  ),

  # ============================================================
  # Authentication
  # ============================================================
  auth_anonymous = list(fr = "Anonyme", en = "Anonymous"),
  auth_login = list(fr = "Se connecter", en = "Sign in"),
  auth_logout = list(fr = "Se d\u00e9connecter", en = "Sign out"),
  auth_logged_as = list(fr = "Connect\u00e9 en tant que", en = "Signed in as"),

  # UG (Management Units) translations
  tab_ug = list(fr = "Unit\u00e9s de Gestion Foresti\u00e8res", en = "Forest Management Units"),
  ug_sidebar_title = list(fr = "Carte UGF", en = "UGF Map"),
  ug_table_sidebar_title = list(fr = "Tableau UGF", en = "UGF Table"),
  ug_title = list(fr = "Unit\u00e9s de Gestion Foresti\u00e8res (UGF)", en = "Forest Management Units (UGF)"),
  ug_table_title = list(fr = "Tableau des unit\u00e9s de gestion foresti\u00e8re", en = "Table of forest management units"),
  ug_merge = list(fr = "Regrouper", en = "Merge"),
  ug_split = list(fr = "Dissocier", en = "Split"),
  ug_rename = list(fr = "Renommer", en = "Rename"),
  ug_group = list(fr = "Groupe d'am\u00e9nagement", en = "Management group"),
  ug_apply_group = list(fr = "Appliquer le groupe", en = "Apply group"),
  ug_surface = list(fr = "Surface", en = "Area"),
  ug_tenements = list(fr = "T\u00e8nements", en = "Tenements"),
  ug_cadastral_refs = list(fr = "R\u00e9f\u00e9rences cadastrales", en = "Cadastral references"),
  ug_composition = list(fr = "Composition cadastrale", en = "Cadastral composition"),
  ug_confirm_merge = list(fr = "Fusionner les %d UG s\u00e9lectionn\u00e9es ?", en = "Merge %d selected UGs?"),
  ug_label_prompt = list(fr = "Nom de la nouvelle UG", en = "New UG name"),
  ug_label_required = list(fr = "Le nom de l'UG est requis", en = "UG name is required"),
  ug_no_data = list(fr = "Aucune UG d\u00e9finie. Chargez un projet pour commencer.", en = "No UG defined. Load a project to start."),
  ug_select_hint = list(fr = "S\u00e9lectionnez une UG dans le tableau pour voir ses d\u00e9tails.", en = "Select a UG in the table to see its details."),
  ug_select_one = list(fr = "S\u00e9lectionnez une seule UG.", en = "Select exactly one UG."),
  ug_select_one_to_split = list(fr = "S\u00e9lectionnez une seule UG \u00e0 dissocier.", en = "Select exactly one UG to split."),
  ug_select_at_least_2 = list(fr = "S\u00e9lectionnez au moins 2 UG \u00e0 regrouper.", en = "Select at least 2 UGs to merge."),
  ug_cannot_split_single = list(fr = "Impossible de dissocier une UG avec un seul tenement.", en = "Cannot split a UG with only one tenement."),
  ug_section_title = list(fr = "Unit\u00e9s de Gestion Foresti\u00e8res", en = "Forest Management Units"),
  ug_composition_title = list(fr = "Composition cadastrale", en = "Cadastral Composition"),
  ug_map_tab = list(fr = "Carte", en = "Map"),
  ug_table_tab = list(fr = "Tableau", en = "Table"),
  ug_map_click_hint = list(fr = "Cliquez sur les tenements de la carte pour les s\u00e9lectionner, puis cr\u00e9ez une UG.", en = "Click on tenements on the map to select them, then create a UG."),
  ug_create_from_map = list(fr = "Cr\u00e9er une UGF depuis la s\u00e9lection", en = "Create UGF from selection"),
  ug_create_btn = list(fr = "Cr\u00e9er l'UG", en = "Create UG"),
  ug_create_confirm = list(fr = "Cr\u00e9er une UG avec %d tenement(s) s\u00e9lectionn\u00e9(s) ?", en = "Create a UG with %d selected tenement(s)?"),
  ug_map_select_tenements_first = list(fr = "S\u00e9lectionnez des tenements sur la carte d'abord.", en = "Select tenements on the map first."),
  ug_clear_selection = list(fr = "Effacer la s\u00e9lection", en = "Clear selection"),
  ug_import_split = list(fr = "Importer un d\u00e9coupage", en = "Import subdivision"),
  ug_export_split = list(fr = "Exporter le d\u00e9coupage", en = "Export subdivision"),
  ug_undo_split = list(fr = "Annuler le d\u00e9coupage", en = "Undo subdivision"),
  ug_split_select_parcel = list(fr = "Parcelle \u00e0 d\u00e9couper", en = "Parcel to subdivide"),
  ug_split_file = list(fr = "Fichier de d\u00e9coupage", en = "Subdivision file"),
  ug_split_hint = list(fr = "Importez un fichier GeoJSON, Shapefile ou GeoPackage contenant les polygones du d\u00e9coupage. Ajoutez une colonne <code>label_ugf</code> pour nommer chaque UGF ; les polygones partageant la m\u00eame valeur sont regroup\u00e9s dans la m\u00eame UGF (les valeurs vides retombent sur le comportement par recouvrement).", en = "Import a GeoJSON, Shapefile or GeoPackage containing the layout polygons. Add a <code>label_ugf</code> column to name each UGF; polygons sharing the same value are grouped into the same UGF (blank values fall back to overlap-inherited UGF)."),
  ug_split_apply = list(fr = "Appliquer le d\u00e9coupage", en = "Apply subdivision"),
  ug_split_no_file = list(fr = "Veuillez s\u00e9lectionner un fichier.", en = "Please select a file."),
  ug_split_empty_file = list(fr = "Le fichier import\u00e9 ne contient aucun polygone.", en = "The imported file contains no polygons."),
  ug_split_success = list(fr = "Parcelle %s d\u00e9coup\u00e9e en %d tenements", en = "Parcel %s subdivided into %d tenements"),
  ug_split_error = list(fr = "Erreur lors du d\u00e9coupage :", en = "Subdivision error:"),
  ug_import_running = list(fr = "Import du d\u00e9coupage en cours", en = "Import in progress"),
  ug_no_split_to_undo = list(fr = "Aucune parcelle n'a \u00e9t\u00e9 d\u00e9coup\u00e9e.", en = "No parcel has been subdivided."),
  ug_undo_split_hint = list(fr = "Cette action restaure un tenement unique pour la parcelle s\u00e9lectionn\u00e9e.", en = "This action restores a single tenement for the selected parcel."),
  ug_undo_split_success = list(fr = "D\u00e9coupage annul\u00e9 pour la parcelle %s", en = "Subdivision undone for parcel %s"),
  ug_draw_split_title = list(fr = "D\u00e9couper avec les formes dessin\u00e9es", en = "Split using drawn shapes"),
  ug_draw_split_desc = list(fr = "Vous avez dessin\u00e9 %d forme(s) sur la carte. S\u00e9lectionnez la parcelle \u00e0 d\u00e9couper.", en = "You drew %d shape(s) on the map. Select the parcel to subdivide."),
  ug_line_split_no_hit = list(fr = "La ligne dessin\u00e9e ne traverse aucun t\u00e8nement.", en = "The drawn line does not cross any tenement."),
  ug_line_split_title = list(fr = "D\u00e9couper les t\u00e8nements travers\u00e9s par la ligne", en = "Split tenements crossed by the drawn line"),
  ug_line_split_desc = list(fr = "La ligne dessin\u00e9e traverse %d t\u00e8nement(s). Chaque t\u00e8nement sera coup\u00e9 le long de la ligne.", en = "The drawn line crosses %d tenement(s). Each will be cut along the line."),
  ug_line_split_success = list(fr = "T\u00e8nements d\u00e9coup\u00e9s avec succ\u00e8s", en = "Tenements successfully split"),
  ug_poly_split_title = list(fr = "D\u00e9couper les t\u00e8nements travers\u00e9s", en = "Split tenements crossed by polygon"),
  ug_poly_split_desc = list(fr = "Le polygone dessin\u00e9 traverse %d t\u00e8nement(s). Chaque t\u00e8nement sera d\u00e9coup\u00e9 en parties \u00ab dedans \u00bb et \u00ab dehors \u00bb du polygone.", en = "The drawn polygon crosses %d tenement(s). Each will be split into 'inside' and 'outside' parts."),
  ug_poly_split_no_hit = list(fr = "Le polygone dessin\u00e9 ne traverse aucun t\u00e8nement.", en = "The drawn polygon does not intersect any tenement."),
  ug_poly_split_success = list(fr = "T\u00e8nements d\u00e9coup\u00e9s avec succ\u00e8s", en = "Tenements successfully split"),
  ug_move_to = list(fr = "D\u00e9placer la s\u00e9lection vers une UGF", en = "Move selection to a UGF"),
  ug_move_desc = list(fr = "D\u00e9placer %d tenement(s) s\u00e9lectionn\u00e9(s) vers une UG existante.", en = "Move %d selected tenement(s) to an existing UG."),
  ug_move_target = list(fr = "UG de destination", en = "Target UG"),
  ug_move_confirm = list(fr = "D\u00e9placer", en = "Move"),
  ug_move_success = list(fr = "%d tenement(s) d\u00e9plac\u00e9(s) vers l'UG \u00ab %s \u00bb", en = "%d tenement(s) moved to UG '%s'"),

  # ============================================================
  # Sampling / QField (E5.a)
  # ============================================================
  sampling_title = list(
    fr = "Plan d'\u00e9chantillonnage terrain",
    en = "Field sampling plan"
  ),
  sampling_subtitle = list(
    fr = "G\u00e9n\u00e9rez des placettes dans la zone courante, puis t\u00e9l\u00e9chargez le projet QField pour la saisie terrain.",
    en = "Generate sampling plots within the current area, then download the QField project for field capture."
  ),
  sampling_no_project = list(
    fr = "S\u00e9lectionnez d'abord un projet (onglet S\u00e9lection).",
    en = "Select a project first (Selection tab)."
  ),
  sampling_dem_resolved_fmt = list(
    fr = "MNT : %s",
    en = "DEM: %s"
  ),
  sampling_no_dem_found_fmt = list(
    fr = "Aucun MNT trouv\u00e9 dans %s. T\u00e9l\u00e9chargez via `opencanopynemeton` (dtm.tif) ou IGN BD ALTI / RGE ALTI avant de lancer le plan d'\u00e9chantillonnage.",
    en = "No DEM found in %s. Download via `opencanopynemeton` (dtm.tif) or IGN BD ALTI / RGE ALTI before running the sampling plan."
  ),
  sampling_chm_missing = list(
    fr = "CHM non trouv\u00e9 \u2014 stratification sans hauteur.",
    en = "No CHM found \u2014 stratification without height."
  ),
  sampling_n_base = list(fr = "Nombre de placettes base", en = "Number of base plots"),
  sampling_n_over = list(fr = "Nombre de placettes de remplacement", en = "Number of replacement plots"),
  sampling_seed = list(fr = "Graine al\u00e9atoire", en = "Random seed"),
  sampling_region = list(fr = "R\u00e9gion biog\u00e9ographique (esp\u00e8ces)", en = "Biogeographic region (species)"),
  sampling_generate = list(fr = "G\u00e9n\u00e9rer les placettes", en = "Generate plots"),
  sampling_map_title = list(fr = "Carte des placettes", en = "Plot map"),
  sampling_empty = list(
    fr = "Aucune placette g\u00e9n\u00e9r\u00e9e. Configurez les param\u00e8tres et cliquez sur G\u00e9n\u00e9rer.",
    en = "No plots yet. Configure the parameters and click Generate."
  ),
  sampling_generating = list(
    fr = "G\u00e9n\u00e9ration du plan d'\u00e9chantillonnage (n = %d + %d)\u2026",
    en = "Generating sampling plan (n = %d + %d)\u2026"
  ),
  sampling_generated_count = list(
    fr = "%d placettes g\u00e9n\u00e9r\u00e9es (%d base, %d remplacement)",
    en = "%d plots generated (%d base, %d replacement)"
  ),
  sampling_persist_failed = list(
    fr = "Impossible d'enregistrer le plan d'\u00e9chantillonnage sur disque : %s. Le bouton \u00ab Enregistrer ce projet comme zone de suivi \u00bb restera inactif tant que le fichier samples.gpkg n'existe pas.",
    en = "Failed to persist the sampling plan to disk: %s. The \"Register this project as a monitoring zone\" button will stay disabled until samples.gpkg exists."
  ),
  sampling_method_note = list(
    fr = "Candidats tir\u00e9s sur une grille r\u00e9guli\u00e8re (50 m par d\u00e9faut) filtr\u00e9e par le masque for\u00eat (BD For\u00eat v2 \u2014 couverture \u2265 70 %). M\u00e9thode de s\u00e9lection, dans l'ordre : GRTS stratifi\u00e9 (CHM + MNT + BD For\u00eat requis), sinon LPM2 spatialement \u00e9quilibr\u00e9 (BalancedSampling), sinon tirage al\u00e9atoire.",
    en = "Candidates drawn on a regular grid (50 m default), filtered by the forest mask (BD For\u00eat v2, cover \u2265 70 %). Selection method, in order: stratified GRTS (needs CHM + DEM + BD For\u00eat), else spatially-balanced LPM2 (BalancedSampling), else random."
  ),

  # Sizing by target error + CV (E5.c)
  sampling_sizing_mode = list(fr = "Mode de dimensionnement",
                              en = "Sizing mode"),
  sampling_mode_fixed  = list(fr = "Taille fixe",
                              en = "Fixed size"),
  sampling_mode_error  = list(fr = "Erreur cible (Cochran)",
                              en = "Target error (Cochran)"),
  sampling_target_error_label = list(
    fr = "Erreur relative cible (%)",
    en = "Target relative error (%)"
  ),
  sampling_alpha_label = list(
    fr = "Risque alpha (%) \u2014 d\u00e9faut 5 %",
    en = "Alpha risk (%) \u2014 default 5 %"
  ),
  sampling_over_ratio_label = list(
    fr = "Ratio de r\u00e9serve (%)",
    en = "Over-sample ratio (%)"
  ),
  sampling_cv_source_label = list(fr = "Source du CV",
                                  en = "CV source"),
  sampling_cv_source_manual  = list(fr = "Manuel",
                                    en = "Manual"),
  sampling_cv_source_bdforet = list(fr = "BD For\u00eat v2 (auto)",
                                    en = "BD For\u00eat v2 (auto)"),
  sampling_cv_manual_label = list(fr = "CV (%)", en = "CV (%)"),
  sampling_cv_position = list(
    fr = "Position dans la fourchette de CV",
    en = "Position in the CV range"
  ),
  sampling_cv_position_low  = list(fr = "Basse (optimiste)",
                                   en = "Low (optimistic)"),
  sampling_cv_position_mid  = list(fr = "M\u00e9diane",
                                   en = "Mid"),
  sampling_cv_position_high = list(fr = "Haute (conservatrice)",
                                   en = "High (conservative)"),
  sampling_cv_compute = list(fr = "Calculer le CV",
                             en = "Compute CV"),
  sampling_cv_bdforet_hint = list(
    fr = "BD For\u00eat v2 est charg\u00e9e lors du premier calcul du projet (cache). Lancez le calcul du projet au moins une fois pour activer le mode auto.",
    en = "BD For\u00eat v2 is fetched during the project's first compute run (cache). Run the project compute at least once to enable auto mode."
  ),
  sampling_cv_bdforet_missing = list(
    fr = "BD For\u00eat v2 indisponible dans le cache du projet. Lancez le calcul du projet ou utilisez le mode manuel.",
    en = "BD For\u00eat v2 unavailable in the project cache. Run compute or fall back to manual mode."
  ),
  sampling_cv_computed = list(
    fr = "CV calcul\u00e9 : %s %% (couverture %s %%)",
    en = "Computed CV: %s %% (coverage %s %%)"
  ),
  sampling_cv_ambiguous = list(
    fr = "%d code(s) TFV ambigu(s) \u2014 revoir bdforet_v2_mapping.csv si besoin.",
    en = "%d ambiguous TFV code(s) \u2014 review bdforet_v2_mapping.csv if needed."
  ),
  sampling_cv_unmapped = list(
    fr = "%d code(s) TFV inconnu(s) du mapping (ignor\u00e9(s)).",
    en = "%d TFV code(s) not in the mapping (ignored)."
  ),
  sampling_n_computed = list(
    fr = "Taille calcul\u00e9e : n = %d placettes (t = %.2f, df = %d).",
    en = "Computed size: n = %d plots (t = %.2f, df = %d)."
  ),
  sampling_n_computed_missing = list(
    fr = "Fournissez une erreur cible et un CV pour calculer n.",
    en = "Provide a target error and a CV to compute n."
  ),

  # Tooltips on the sampling form (Export terrain)
  sampling_tt_target_error = list(
    fr = "Erreur relative acceptée sur la moyenne de la variable cible (surface terrière G/ha). Ex. 10 % signifie ±10 %. Plus c'est serré, plus n est grand (n ~ 1 / E²).",
    en = "Acceptable relative error on the mean of the target variable (basal area G/ha). E.g. 10 % means ±10 %. The tighter, the larger n (n ~ 1 / E²)."
  ),
  sampling_tt_alpha = list(
    fr = "Risque de 1ère espèce (α) : probabilité d'accepter à tort une erreur plus grande que la cible. Conventionnel à 5 % (intervalle de confiance 95 %).",
    en = "Type-I error risk (α): probability of accepting a larger error than the target by chance. Standard 5 % (95 % confidence interval)."
  ),
  sampling_tt_over_ratio = list(
    fr = "Placettes de remplacement en plus des Base, en % de n_base. Utilisées si une placette Base est inaccessible sur le terrain. 20 % est un compromis courant.",
    en = "Replacement plots added on top of n_base, as a % of n_base. Used when a Base plot turns out to be inaccessible. 20 % is a common compromise."
  ),
  sampling_tt_cv_position = list(
    fr = "Borne de la fourchette de CV (issue de la typologie) à utiliser. Basse = optimiste (n plus faible), Haute = conservatrice (n plus grand), Médiane = défaut.",
    en = "Which bound of the CV range (from the typology) to use. Low = optimistic (smaller n), High = conservative (larger n), Mid = default."
  ),
  sampling_tt_seed = list(
    fr = "Graine du générateur aléatoire. Garantit la reproductibilité : avec la même graine et les mêmes paramètres, le tirage est identique.",
    en = "Random number generator seed. Guarantees reproducibility: same seed + same parameters = identical draw."
  ),
  sampling_tt_cv_source = list(
    fr = "Source du coefficient de variation utilisé par la formule de Cochran. \"Manuel\" : vous fournissez le CV en %. \"BD Forêt v2 (auto)\" : le CV est dérivé automatiquement du cache BD Forêt v2 du projet (moyenne pondérée par surface des CV typiques par contexte sylvicole). Ce choix n'affecte PAS la méthode de tirage (GRTS / LPM2 / aléatoire), seulement la valeur de CV.",
    en = "Source of the coefficient of variation fed to the Cochran formula. \"Manual\": you set the CV in %. \"BD Forêt v2 (auto)\": the CV is derived from the project's cached BD Forêt v2 (area-weighted mean of the typical CV per silvicultural context). This choice does NOT affect the draw method (GRTS / LPM2 / random), only the CV value."
  ),
  sampling_tt_region = list(
    fr = "Région biogéographique utilisée pour le domaine des espèces (liste déroulante dans QGIS). Ex. BFC = Bourgogne-Franche-Comté, EU = domaine européen générique.",
    en = "Biogeographic region used for the species domain (dropdown in QGIS). E.g. BFC = Bourgogne-Franche-Comté, EU = generic European domain."
  ),

  # TSP map legend
  sampling_legend_tsp_title   = list(fr = "Parcours",        en = "Route"),
  sampling_legend_tsp_start   = list(fr = "Départ",          en = "Start"),
  sampling_legend_tsp_end     = list(fr = "Arrivée",         en = "Finish"),
  sampling_legend_tsp_line    = list(fr = "Ordre de visite", en = "Visit order"),
  sampling_legend_plots_title = list(fr = "Placettes",       en = "Plots"),
  qfield_download = list(
    fr = "T\u00e9l\u00e9charger le projet QGIS",
    en = "Download QGIS project"
  ),
  qfield_project_name = list(fr = "Nom du projet", en = "Project name"),
  qfield_ready = list(
    fr = "Projet QField pr\u00eat au t\u00e9l\u00e9chargement.",
    en = "QField project ready for download."
  ),

  # ============================================================
  # Field Ingest (E5.b \u2014 QField return path)
  # ============================================================
  field_ingest_title = list(
    fr = "Ingestion des donn\u00e9es de terrain",
    en = "Field data ingest"
  ),
  field_ingest_accordion_title = list(
    fr = "Import des donn\u00e9es terrain",
    en = "Field data import"
  ),
  field_ingest_subtitle = list(
    fr = "D\u00e9poser le GeoPackage renvoy\u00e9 par QField pour valider et rattacher les placettes au projet.",
    en = "Drop the GeoPackage returned by QField to validate and attach placettes to the project."
  ),
  field_ingest_upload = list(
    fr = "D\u00e9poser un GeoPackage (.gpkg)",
    en = "Drop a GeoPackage (.gpkg)"
  ),
  field_ingest_browse  = list(fr = "Parcourir...",     en = "Browse..."),
  field_ingest_placeholder = list(fr = "Aucun fichier s\u00e9lectionn\u00e9",
                                  en = "No file selected"),
  field_ingest_validate = list(fr = "Valider",         en = "Validate"),
  field_ingest_attach   = list(fr = "Rattacher au projet", en = "Attach to project"),
  field_ingest_note = list(
    fr = "Le rattachement persiste le GPKG dans le projet et met \u00e0 jour le NDP. Les indicateurs (P1, P2, B2, C1, R2) seront recalcul\u00e9s au prochain lancement de calcul.",
    en = "Attaching persists the GPKG to the project and updates the NDP level. Indicators (P1, P2, B2, C1, R2) will be recomputed on the next compute run."
  ),
  field_ingest_waiting = list(
    fr = "En attente d'un GeoPackage \u00e0 valider.",
    en = "Waiting for a GeoPackage to validate."
  ),
  field_ingest_loaded = list(
    fr = "Import\u00e9 : %d placettes, %d arbres.",
    en = "Imported: %d plots, %d trees."
  ),
  field_ingest_validated = list(
    fr = "Validation OK : %d placettes, %d arbres. Pr\u00eat \u00e0 rattacher.",
    en = "Validation OK: %d plots, %d trees. Ready to attach."
  ),
  field_ingest_errors = list(
    fr = "Validation KO : %d erreur(s). Corrigez le GeoPackage avant de rattacher.",
    en = "Validation failed: %d error(s). Fix the GeoPackage before attaching."
  ),
  field_ingest_no_file = list(
    fr = "Veuillez d\u00e9poser un fichier GeoPackage.",
    en = "Please drop a GeoPackage file."
  ),
  field_ingest_no_project = list(
    fr = "Aucun projet actif. Chargez un projet avant d'ingerer des donn\u00e9es terrain.",
    en = "No active project. Load a project before ingesting field data."
  ),
  field_ingest_no_units = list(
    fr = "Le projet ne contient pas d'unit\u00e9s de gestion foresti\u00e8re exploitable.",
    en = "The active project has no usable forest management units."
  ),
  field_ingest_not_validated = list(
    fr = "Validez d'abord le GeoPackage avant de rattacher.",
    en = "Validate the GeoPackage before attaching."
  ),
  field_ingest_persist_failed = list(
    fr = "\u00c9chec de la persistance du GeoPackage terrain.",
    en = "Failed to persist the field GeoPackage."
  ),
  field_ingest_attached = list(
    fr = "Donn\u00e9es terrain rattach\u00e9es. NDP projet mis \u00e0 jour : %d.",
    en = "Field data attached. Project NDP updated to: %d."
  ),
  field_ingest_ndp_before = list(fr = "NDP actuel",  en = "Current NDP"),
  field_ingest_ndp_after  = list(fr = "NDP apr\u00e8s ingestion",
                                  en = "NDP after ingest"),
  field_ingest_report_header = list(
    fr = "%d erreur(s), %d avertissement(s).",
    en = "%d error(s), %d warning(s)."
  ),
  field_ingest_report_errors   = list(fr = "Erreurs",       en = "Errors"),
  field_ingest_report_warnings = list(fr = "Avertissements", en = "Warnings"),

  # ============================================================
  # Monitoring (E6.b - Sentinel-2 NDVI/NBR continuous monitoring)
  # E6.c.5 (spec 008) - FORDEAD diagnostic mode
  # ============================================================
  tab_monitoring = list(fr = "Suivi sanitaire", en = "Forest health monitoring"),
  monitoring_accordion_title = list(
    fr = "Suivi continu Sentinel-2",
    en = "Continuous Sentinel-2 monitoring"
  ),
  monitoring_subtitle = list(
    fr = "D\u00e9tection des baisses NDVI / NBR sur les placettes enregistr\u00e9es.",
    en = "Detect NDVI / NBR drops on the registered plots."
  ),
  monitoring_zone_label = list(
    fr = "Zone de suivi",
    en = "Monitoring zone"
  ),
  monitoring_zone_none = list(
    fr = "Aucune zone enregistr\u00e9e dans la base de suivi.",
    en = "No monitoring zone registered yet."
  ),
  monitoring_zone_register_hint = list(
    fr = "Enregistrez une zone via R : nemeton::register_monitoring_zone(con, ...).",
    en = "Register a zone from R: nemeton::register_monitoring_zone(con, ...)."
  ),
  # v0.52.5 — État « orphelin » : la base contient des zones mais
  # aucune n'est rattachée au projet chargé. Symptôme typique :
  # `helper-monitoring.R` côté cœur a wipe + recréé une stub
  # (incident villards 2026-05-31, cf. brief cœur v0.54.0).
  monitoring_zone_orphan_title = list(
    fr = "Zones présentes — mais aucune ne correspond à ce projet",
    en = "Zones present — but none matches this project"
  ),
  monitoring_zone_orphan_body = list(
    fr = "La base contient %d zone(s), mais aucune n'est rattachée au projet chargé. Symptôme typique d'un wipe par les tests cœur (incident villards 2026-05-31). Clique sur « Enregistrer ce projet comme zone de suivi » dans la barre latérale pour recréer la zone de suivi en un clic.",
    en = "The monitoring DB contains %d zone(s), but none is linked to the loaded project. Typical symptom of a wipe by core tests (villards incident 2026-05-31). Click « Register this project as a monitoring zone » in the sidebar to recreate the monitoring zone in one click."
  ),
  monitoring_date_range = list(
    fr = "P\u00e9riode d'observation",
    en = "Observation period"
  ),
  # v0.61.0 — Clé `monitoring_bands` retirée. NDVI + NBR sont
  # systématiquement téléchargés lors du Diagnostic FAST (câblage
  # en dur dans `fast_task$invoke()`), donc le choix utilisateur
  # n'a plus lieu d'être. Les radios NDVI/NBR des sidebars droits
  # des onglets pilotent l'AFFICHAGE, pas le téléchargement.
  # v0.52.13 — Label du radio mono-index (spec 017 nemeton@v0.55.0).
  # L'utilisateur choisit l'indice spectral utilisé pour le raster
  # d'alerte FAST (NDMI / NDVI / NBR / NDRE en count/rolling). Le seuil
  # correspondant est piloté par les sliders
  # monitoring_threshold_{ndvi,nbr,ndmi,ndre}.
  monitoring_fast_index_label = list(
    fr = "Indice FAST",
    en = "FAST index"
  ),
  monitoring_threshold_ndvi = list(
    fr = "Seuil minimum NDVI",
    en = "Minimum NDVI threshold"
  ),
  monitoring_threshold_nbr = list(
    fr = "Seuil minimum NBR",
    en = "Minimum NBR threshold"
  ),
  monitoring_threshold_ndmi = list(
    fr = "Seuil minimum NDMI",
    en = "Minimum NDMI threshold"
  ),
  monitoring_window_days = list(
    fr = "Fen\u00eatre roulante (jours)",
    en = "Rolling window (days)"
  ),
  monitoring_run_btn = list(
    fr = "Lancer le diagnostic FAST",
    en = "Run FAST diagnosis"
  ),
  # v0.52.0 — Vrai cancel coopératif (nemeton@v0.53.0+). Le clic écrit
  # `<projet>/data/{fast,fordead}_cancel.flag`. Le worker poll ce
  # fichier entre tuiles (FAST) / entre phases reticulate (FORDEAD)
  # et sort proprement au prochain checkpoint. Le libellé reflète ce
  # vrai cancel ; le toast explique le mécanisme (tuile/phase courante
  # finit, puis stop ; INSERT idempotents conservés).
  monitoring_run_cancel_btn = list(
    fr = "Annuler le diagnostic",
    en = "Cancel the diagnostic"
  ),
  monitoring_run_cancel_done = list(
    fr = "Annulation demandée. Le worker termine la tuile (FAST) / la phase (FORDEAD) en cours puis s'arrête proprement. Les INSERT déjà commités en base sont conservés (ON CONFLICT DO NOTHING — relance sans risque).",
    en = "Cancellation requested. The worker finishes the current tile (FAST) / phase (FORDEAD) then exits cleanly. Already-committed DB rows are kept (ON CONFLICT DO NOTHING — safe to relaunch)."
  ),
  # v0.73.0 (spec 020) — Wording bouton + messages réorientés
  # « zones de suivi » (pluriel : 4 strates `_tot/_feu/_res/_mix`).
  monitoring_register_btn = list(
    fr = "Générer les zones de suivi",
    en = "Generate monitoring zones"
  ),
  monitoring_register_no_project = list(
    fr = "Chargez un projet avant de générer les zones de suivi.",
    en = "Load a project before generating monitoring zones."
  ),
  monitoring_register_no_samples = list(
    fr = "Aucune UGF dans ce projet. Définissez d'abord les UGFs (onglet UGF).",
    en = "No forestry units in this project. Define UGFs first (UGF tab)."
  ),
  monitoring_register_no_db = list(
    fr = "Base de suivi non configurée — impossible de générer les zones.",
    en = "Monitoring database not configured — cannot generate zones."
  ),
  monitoring_register_running = list(
    fr = "Génération des zones de suivi en cours (intersections sf)…",
    en = "Generating monitoring zones (sf intersections)…"
  ),
  # v0.73.0 — Anciennes clés `monitoring_register_success` et
  # `_already` (singulier, n_plots) remplacées par
  # `zones_build_success_fmt` (cf. plus bas).
  monitoring_register_error = list(
    fr = "Échec de la génération des zones",
    en = "Zone generation failed"
  ),
  # v0.73.0 (spec 020) — Clés spécifiques aux 4 strates.
  zones_build_success_fmt = list(
    fr = "%d zone(s) de suivi générée(s) : %s.",
    en = "%d monitoring zone(s) generated: %s."
  ),
  zones_bdforet_missing = list(
    fr = "BD Forêt manquante (`cache/layers/bdforet.gpkg`). Lancez d'abord le calcul du projet (onglet Synthèse) pour la télécharger.",
    en = "BD Forêt missing (`cache/layers/bdforet.gpkg`). Run the project computation first (Synthesis tab) to download it."
  ),
  zone_tot = list(
    fr = "Toutes essences",
    en = "All species"
  ),
  zone_feu = list(
    fr = "Feuillus",
    en = "Deciduous"
  ),
  zone_res = list(
    fr = "Résineux",
    en = "Coniferous"
  ),
  zone_mix = list(
    fr = "Mixte",
    en = "Mixed"
  ),
  # v0.77.0 — Bandeau « Surfaces des zones de suivi » (mode FAST) :
  # rappelle la surface (ha) et la part (%) des 4 strates projet
  # `_tot/_feu/_res/_mix` au-dessus des sous-onglets FAST.
  monitoring_fast_surfaces_title = list(
    fr = "Surfaces des zones de suivi",
    en = "Monitoring zone surfaces"
  ),
  # Format d'un segment : « Feuillus : 12.3 ha (45 %) ». %s = label
  # strate, %.1f = hectares, %.0f = pourcentage relatif à la strate
  # `_tot` (toutes essences).
  monitoring_fast_surf_item = list(
    fr = "%s : %.1f ha (%.0f %%)",
    en = "%s: %.1f ha (%.0f%%)"
  ),
  # Variante sans pourcentage (strate `_tot`, référence 100 %).
  monitoring_fast_surf_item_tot = list(
    fr = "%s : %.1f ha",
    en = "%s: %.1f ha"
  ),
  monitoring_validate_zone = list(
    fr = "S\u00e9lectionnez une zone de suivi avant de lancer le t\u00e9l\u00e9chargement.",
    en = "Select a monitoring zone before starting the download."
  ),
  # v0.61.0 \u2014 Cl\u00e9 `monitoring_validate_bands` retir\u00e9e (validation
  # devenue impossible : NDVI + NBR c\u00e2bl\u00e9s en dur).
  monitoring_validate_dates = list(
    fr = "P\u00e9riode d'observation invalide.",
    en = "Invalid observation period."
  ),
  monitoring_ingest_starting = list(
    fr = "T\u00e9l\u00e9chargement Sentinel-2 en cours\u2026 cela peut prendre quelques minutes.",
    en = "Sentinel-2 download running\u2026 this can take a few minutes."
  ),
  # v0.85.2.9000 \u2014 Reprise d'ingestion : bandeaux affich\u00e9s au
  # (re)chargement quand une ingestion lanc\u00e9e dans une session
  # ant\u00e9rieure est encore en cours (worker vivant) ou a \u00e9t\u00e9
  # interrompue (worker mort). `%s` = suffixe \u00ab (X/Y) \u00bb optionnel.
  monitoring_ingest_running_banner = list(
    fr = "Ingestion Sentinel-2 en cours en arri\u00e8re-plan%s\u2026",
    en = "Sentinel-2 ingestion running in the background%s\u2026"
  ),
  monitoring_ingest_interrupted_banner = list(
    fr = "Une ingestion Sentinel-2 a \u00e9t\u00e9 interrompue%s. Reprendre l\u00e0 o\u00f9 elle s'est arr\u00eat\u00e9e ?",
    en = "A Sentinel-2 ingestion was interrupted%s. Resume where it left off?"
  ),
  monitoring_ingest_resume_btn = list(
    fr = "Reprendre l'ingestion",
    en = "Resume ingestion"
  ),
  monitoring_resume_no_state = list(
    fr = "Aucune ingestion \u00e0 reprendre (sentinelle introuvable ou incompl\u00e8te).",
    en = "No ingestion to resume (sentinel missing or incomplete)."
  ),
  # Verrou crois\u00e9 FAST <-> FORDEAD : les deux diagnostics partagent le
  # cache de bandes Sentinel-2 du projet, les lancer en parall\u00e8le ferait
  # courir deux workers sur le m\u00eame fichier <bande>.tif.tmp.
  monitoring_busy_fordead = list(
    fr = "Un diagnostic FORDEAD est en cours. Attendez sa fin avant de lancer une analyse FAST (cache Sentinel-2 partag\u00e9).",
    en = "A FORDEAD diagnosis is running. Wait for it to finish before starting a FAST analysis (shared Sentinel-2 cache)."
  ),
  monitoring_busy_fast = list(
    fr = "Une analyse FAST est en cours. Attendez sa fin avant de lancer un diagnostic FORDEAD (cache Sentinel-2 partag\u00e9).",
    en = "A FAST analysis is running. Wait for it to finish before starting a FORDEAD diagnosis (shared Sentinel-2 cache)."
  ),
  # v0.53.1 \u2014 message simplifi\u00e9 : depuis nemeton@v0.58.0 (drop
  # obs_pixel insertion) le compteur `n_obs_inserted` est TOUJOURS 0,
  # donc on l'a retir\u00e9 du toast. Le nombre de sc\u00e8nes (= bandes COG
  # mises en cache) reste la m\u00e9trique pertinente.
  monitoring_ingest_success = list(
    fr = "Diagnostic FAST termin\u00e9 : %d sc\u00e8ne(s) en cache.",
    en = "FAST diagnostic done: %d scene(s) cached."
  ),
  monitoring_ingest_error = list(
    fr = "\u00c9chec du t\u00e9l\u00e9chargement",
    en = "Download failed"
  ),
  monitoring_stac_search = list(
    fr = "Recherche des sc\u00e8nes Sentinel-2 disponibles\u2026",
    en = "Searching for available Sentinel-2 scenes\u2026"
  ),
  monitoring_stac_search_with_count_fmt = list(
    fr = "Pr\u00e9paration du t\u00e9l\u00e9chargement : %d sc\u00e8ne(s) trouv\u00e9e(s)\u2026",
    en = "Preparing download: %d scene(s) found\u2026"
  ),
  monitoring_ingest_zero_fmt = list(
    fr = "Aucune sc\u00e8ne Sentinel-2 trouv\u00e9e pour la zone et la p\u00e9riode demand\u00e9es. %s",
    en = "No Sentinel-2 scene found for the requested zone and period. %s"
  ),
  monitoring_ingest_zero_default = list(
    fr = "\u00c9largis la fen\u00eatre temporelle ou tol\u00e8re plus de nuages, puis r\u00e9essaie.",
    en = "Widen the time window or allow more clouds, then try again."
  ),
  monitoring_ingest_warns_fmt = list(
    fr = "Avertissement(s) du backend : %s",
    en = "Backend warning(s): %s"
  ),
  monitoring_ingest_progress_fmt = list(
    fr = "Tuile Sentinel-2 t\u00e9l\u00e9charg\u00e9e (%d/%d)",
    en = "Sentinel-2 tile downloaded (%d/%d)"
  ),
  monitoring_ingest_progress_named_fmt = list(
    fr = "Tuile Sentinel-2 %s (%d/%d)",
    en = "Sentinel-2 tile %s (%d/%d)"
  ),
  monitoring_ingest_cache_lookup_fmt = list(
    fr = "Cache DB : %d sc\u00e8nes d\u00e9j\u00e0 ing\u00e9r\u00e9es (skip), %d \u00e0 compl\u00e9ter",
    en = "DB cache: %d scenes already ingested (skip), %d to complete"
  ),
  # v0.70.3 \u2014 Toast initial \u00e9mis \u00e0 `s2:search_done` (avant le 1er
  # `s2:scene`). Garantit que `(1/N)` appara\u00eet au moins une fois
  # m\u00eame si le polling 500 ms rate les 1\u02b3\u1d49\u02e2 sc\u00e8nes. Les 2 `%d`
  # sont (1, total_scenes).
  monitoring_ingest_search_done_fmt = list(
    fr = "Tuile (%d/%d) \u2014 d\u00e9marrage du t\u00e9l\u00e9chargement\u2026",
    en = "Tile (%d/%d) \u2014 starting download\u2026"
  ),
  monitoring_ingest_band_failed_fmt = list(
    fr = "\u00c9chec bande %s : %s",
    en = "Band %s failed: %s"
  ),
  monitoring_ingest_token_refreshed = list(
    fr = "Token SAS Planetary Computer rafra\u00eechi",
    en = "Planetary Computer SAS token refreshed"
  ),
  monitoring_cache_active_fmt = list(
    fr = "Cache COG actif : %s",
    en = "COG cache active: %s"
  ),
  monitoring_ingest_worker_event_fmt = list(
    fr = "Worker : \u00e9tape %s",
    en = "Worker: step %s"
  ),
  monitoring_ingest_fatal_title = list(
    fr = "Erreur fatale dans le worker d'ingestion",
    en = "Fatal error in the ingestion worker"
  ),
  monitoring_reprime_cache_label = list(
    fr = "R\u00e9amorcer le cache COG (fen\u00eatre de dates)",
    en = "Reset COG cache (date window)"
  ),
  monitoring_reprime_cache_help = list(
    fr = "<strong>D\u00e9coch\u00e9</strong> (d\u00e9faut) : nemeton v\u00e9rifie le cache disque et ne t\u00e9l\u00e9charge que les bandes manquantes. INSERTs DB idempotents (<code>ON CONFLICT DO NOTHING</code>).<br><br><strong>Coch\u00e9</strong> : vide puis re-t\u00e9l\u00e9charge <strong>uniquement</strong> les sc\u00e8nes de la fen\u00eatre de dates FAST. Les sc\u00e8nes hors fen\u00eatre (p\u00e9riode d'apprentissage FORDEAD \u2014 cache S2 partag\u00e9) sont <strong>pr\u00e9serv\u00e9es</strong>.",
    en = "<strong>Unchecked</strong> (default): nemeton checks the disk cache and only downloads missing bands. DB INSERTs are idempotent (<code>ON CONFLICT DO NOTHING</code>).<br><br><strong>Checked</strong>: wipes then re-downloads <strong>only</strong> the scenes within the FAST date window. Out-of-window scenes (FORDEAD training period \u2014 shared S2 cache) are <strong>preserved</strong>."
  ),
  monitoring_db_unavailable = list(
    fr = "Base de suivi non configur\u00e9e.",
    en = "Monitoring database not configured."
  ),
  monitoring_db_check_env = list(
    fr = "Renseignez NEMETON_DB_URL ou les variables NEMETON_DB_HOST/_PORT/_NAME/_USER/_PASSWORD, ou ouvrez un projet pour utiliser le mode local (SQLite).",
    en = "Set NEMETON_DB_URL or the NEMETON_DB_HOST/_PORT/_NAME/_USER/_PASSWORD variables, or open a project to use the local mode (SQLite)."
  ),
  monitoring_db_connected = list(
    fr = "Base de suivi connect\u00e9e \u2014 %d zone(s) disponible(s).",
    en = "Monitoring database connected \u2014 %d zone(s) available."
  ),
  monitoring_db_local = list(
    fr = "Mode local (SQLite) \u2014 base de suivi monoposte stock\u00e9e dans le projet.",
    en = "Local mode (SQLite) \u2014 single-user monitoring database stored in the project."
  ),
  monitoring_db_local_hint = list(
    fr = "Pour partager le suivi entre plusieurs utilisateurs, configurez Postgres+TimescaleDB via NEMETON_DB_URL.",
    en = "To share monitoring between multiple users, configure Postgres+TimescaleDB via NEMETON_DB_URL."
  ),
  monitoring_db_local_pkg_missing = list(
    fr = "Le paquet R {.pkg RSQLite} n'est pas install\u00e9, le mode local n'est pas disponible. Installez avec install.packages(\"RSQLite\") ou configurez Postgres.",
    en = "The R {.pkg RSQLite} package is not installed; local mode is unavailable. Run install.packages(\"RSQLite\") or configure Postgres."
  ),
  monitoring_db_no_project = list(
    fr = "Aucun projet charg\u00e9. S\u00e9lectionnez ou cr\u00e9ez un projet dans l'onglet S\u00e9lection pour activer le mode local (SQLite).",
    en = "No project loaded. Pick or create a project in the Selection tab to enable local mode (SQLite)."
  ),
  monitoring_db_connecting = list(
    fr = "Tentative de connexion \u00e0 la base de suivi\u2026",
    en = "Connecting to the monitoring database\u2026"
  ),
  monitoring_db_local_creating = list(
    fr = "Cr\u00e9ation de la base locale (SQLite) dans le projet\u2026",
    en = "Creating the local SQLite database in the project\u2026"
  ),
  monitoring_db_local_ready = list(
    fr = "Base locale (SQLite) pr\u00eate.",
    en = "Local database (SQLite) ready."
  ),
  monitoring_db_remote_ready = list(
    fr = "Base de suivi distante connect\u00e9e.",
    en = "Remote monitoring database connected."
  ),
  monitoring_db_loading_hint = list(
    fr = "Migration du sch\u00e9ma en cours, cela peut prendre quelques secondes au premier d\u00e9marrage\u2026",
    en = "Schema migration in progress, this can take a few seconds on first start\u2026"
  ),
  date_range_separator = list(
    fr = "au",
    en = "to"
  ),
  monitoring_timeseries_title = list(
    fr = "S\u00e9rie temporelle NDVI / NBR",
    en = "NDVI / NBR time series"
  ),
  monitoring_timeseries_no_data = list(
    fr = "Aucune observation pour les filtres courants. Lancez une ingestion ou \u00e9largissez la fen\u00eatre.",
    en = "No observations for the current filters. Run an ingestion or widen the window."
  ),
  monitoring_timeseries_xaxis = list(
    fr = "Date d'observation",
    en = "Observation date"
  ),
  monitoring_timeseries_yaxis = list(
    fr = "Valeur de l'indice",
    en = "Index value"
  ),
  monitoring_alerts_title = list(
    fr = "Alertes d\u00e9tect\u00e9es",
    en = "Detected alerts"
  ),

  # ----- Sub-tab labels (v0.28.0) -----
  monitoring_subtab_alerts = list(
    fr = "Alertes",
    en = "Alerts"
  ),
  monitoring_subtab_pixel_map = list(
    fr = "Carte pixel",
    en = "Pixel map"
  ),
  # v0.34.0 — sous-onglets de carte pixel séparés par mode :
  # FAST (NDVI/NBR raster, mode quick) vs FORDEAD (mask classifié de
  # dépérissement, mode health). Visibilité pilotée côté server par
  # un observe mode-driven.
  monitoring_subtab_pixel_map_fast = list(
    fr = "Carte FAST",
    en = "FAST map"
  ),
  monitoring_subtab_pixel_map_fordead = list(
    fr = "Alertes FORDEAD",
    en = "FORDEAD alerts"
  ),
  # v0.35.0 — sous-onglets Alertes séparés par mode, symétriques aux
  # cartes : Alertes FAST (pixels au-dessus du seuil NDVI/NBR
  # rolling-window) vs Alertes FORDEAD (placettes flaguées par
  # run_fordead_dieback). Visibilité pilotée par input$mode.
  monitoring_subtab_alerts_fast = list(
    fr = "Alertes FAST",
    en = "FAST alerts"
  ),
  # v0.36.0 — Alertes FAST module (nemeton::list_fast_alerts_for_zone)
  # Severity buckets : critical (ratio < 0.5), warning ([0.5, 1)),
  # info ([1, 1.1) — corridor d'avertissement).
  monitoring_fast_severity_critical = list(
    fr = "Critique",
    en = "Critical"
  ),
  monitoring_fast_severity_warning = list(
    fr = "Alerte",
    en = "Warning"
  ),
  monitoring_fast_severity_info = list(
    fr = "Vigilance",
    en = "Info"
  ),
  monitoring_fast_total = list(
    fr = "Total placettes",
    en = "Total plots"
  ),
  monitoring_fast_alerts_empty_title = list(
    fr = "Aucune alerte FAST sur la fenêtre",
    en = "No FAST alerts in window"
  ),
  # v0.53.0 — bandeau d'erreur distinct de « zone saine » : affiché
  # quand `read_fast_alert_raster()` lève une exception OU renvoie
  # NULL (typiquement cache S2 incomplet pour l'indice choisi —
  # courant pour NBR si B12 manque sur beaucoup de scènes).
  monitoring_fast_alerts_error_title = list(
    fr = "Raster d'alerte non calculable",
    en = "Alert raster could not be computed"
  ),
  monitoring_fast_alerts_empty_body = list(
    fr = "Aucun pixel n'a déclenché d'alerte sur la fenêtre choisie. Élargir la fenêtre, relever le seuil ou changer la zone pour explorer d'autres scénarios.",
    en = "No pixel triggered an alert in the chosen window. Widen the window, raise the threshold, or change zone to explore further."
  ),
  # v0.68.0 — Message NULL spécifique à « aucune scène cachée ne
  # porte les bandes de cet indice dans la fenêtre » (brief FAST 6
  # cartes nemeton@v0.65.0). Cas typique : NDMI demande B08+B11,
  # une zone sans B11 produit NULL pour NDMI sur cette période.
  # Le `%s` reçoit le nom de l'indice (NDVI/NBR/NDMI).
  monitoring_fast_alerts_no_scene = list(
    fr = "%s : aucune scène cachée ne porte les bandes de cet indice dans la fenêtre (cache S2 vide ou incomplet pour les bandes requises).",
    en = "%s: no cached scene carries the required bands for this index in the window (S2 cache empty or missing required bands)."
  ),
  # v0.41.1 — popup_plot renamed to popup_coords : FAST alerts are
  # moving from per-placette markers to a pixel raster (cf. spec
  # validation-sampling). The popup row anchors the user on a *position*
  # rather than a logical plot, hence "Coordonnées" / "Coordinates".
  monitoring_fast_alert_popup_coords = list(
    fr = "Coordonnées",
    en = "Coordinates"
  ),
  # v0.41.1 — "Sévérité" est conservé le temps que
  # nemeton::read_fast_alert_raster() tranche entre un score continu et
  # des classes discrètes. Si score continu : reformuler en
  # monitoring_fast_alerts_legend_title ("Score d'alerte"). Si classes :
  # garder ce libellé.
  monitoring_fast_alert_popup_severity = list(
    fr = "Sévérité",
    en = "Severity"
  ),
  monitoring_fast_alert_popup_ndvi = list(
    fr = "NDVI",
    en = "NDVI"
  ),
  monitoring_fast_alert_popup_nbr = list(
    fr = "NBR",
    en = "NBR"
  ),
  monitoring_fast_alert_popup_last_obs = list(
    fr = "Dernière observation",
    en = "Last observation"
  ),
  # v0.42.0 — wiring raster d'alerte FAST (spec 013, nemeton 0.46.0).
  # mod_monitoring_fast_alerts bascule de la liste markers per-placette
  # à addRasterImage(read_fast_alert_raster()). Le module expose un
  # toggle compte / rolling : `count` retourne un entier par pixel
  # (nombre de dates en alerte), `rolling` un continu (magnitude).
  # `legend_title` est utilisé pour le mode rolling, `legend_count_title`
  # pour le mode count.
  # v0.45.0 — libellés revus après tests user villards. "Compte" et
  # "Magnitude" étaient abstraits ; "Fréquence" / "Intensité" parlent
  # davantage du sens métier (jours en alerte vs intensité du déficit
  # NDVI/NBR sur la fenêtre roulante).
  monitoring_fast_alerts_mode_label = list(
    fr = "Mode du raster",
    en = "Raster mode"
  ),
  monitoring_fast_alerts_mode_count = list(
    fr = "Fréquence",
    en = "Frequency"
  ),
  monitoring_fast_alerts_mode_rolling = list(
    fr = "Intensité",
    en = "Intensity"
  ),
  monitoring_fast_alerts_mode_trend = list(
    fr = "Tendance",
    en = "Trend"
  ),
  monitoring_fast_alerts_badge_count = list(
    fr = "Alertes FAST — fréquence des dépassements de seuil %s, à la résolution Sentinel-2 (10 m)",
    en = "FAST alerts — threshold-breach frequency for %s, at Sentinel-2 resolution (10 m)"
  ),
  monitoring_fast_alerts_badge_rolling = list(
    fr = "Alertes FAST — intensité du déficit %s sur fenêtre roulante, à la résolution Sentinel-2 (10 m)",
    en = "FAST alerts — %s deficit intensity over a rolling window, at Sentinel-2 resolution (10 m)"
  ),
  monitoring_fast_alerts_badge_trend = list(
    fr = "Alertes FAST \u2014 tendance pluriannuelle du d\u00e9clin %s (Theil-Sen + Mann-Kendall), \u00e0 la r\u00e9solution Sentinel-2 (10 m)",
    en = "FAST alerts \u2014 multi-year decline trend for %s (Theil-Sen + Mann-Kendall), at Sentinel-2 resolution (10 m)"
  ),
  monitoring_trend_months = list(
    fr = "Mois du composite saisonnier",
    en = "Seasonal composite months"
  ),
  monitoring_trend_min_years = list(
    fr = "Ann\u00e9es minimum",
    en = "Minimum years"
  ),
  monitoring_trend_alpha = list(
    fr = "Seuil de significativit\u00e9 (alpha)",
    en = "Significance threshold (alpha)"
  ),
  index_ndmi = list(
    fr = "NDMI (humidit\u00e9)",
    en = "NDMI (moisture)"
  ),
  # v0.70.5 \u2014 Cl\u00e9s `monitoring_fast_ndmi_hint` et
  # `monitoring_fast_ndmi_b11_note` retir\u00e9es avec le helper
  # `.fast_ndmi_note()` (mod_monitoring_fast_alerts.R +
  # mod_monitoring_pixel_map.R). L'avertissement B11 \u00e9tait
  # obsol\u00e8te depuis le plancher c\u0153ur `nemeton (>= 0.65.1)`
  # (v0.69.1 app) : B11 cach\u00e9e syst\u00e9matiquement en best-effort
  # par `ingest_sentinel2_timeseries()` (spec 019 D3), donc le
  # message \u00ab r\u00e9-ingestion sans cache pour activer NDMI \u00bb ne
  # s'applique plus aux installations r\u00e9centes. Le hint
  # descriptif (\u00ab baisse sous stress hydrique \u00bb) est retir\u00e9
  # avec \u2014 \u00e9pure l'UI des sidebars FAST.
  # v0.55.0 — Clés utilisées par les toasts `fast_prewarm:*` émis par
  # le cœur (spec 018 nemeton@v0.61.0) consommés par l'observer
  # ingest_progress de mod_monitoring.R. Les noms `fast_mode_*` sont
  # courts car réutilisables hors du contexte « alerts » (Carte FAST
  # pourrait aussi vouloir afficher le libellé du mode si besoin).
  fast_mode_frequence = list(
    fr = "Fréquence",
    en = "Frequency"
  ),
  fast_mode_intensite = list(
    fr = "Intensité",
    en = "Intensity"
  ),
  fast_mode_trend = list(
    fr = "Tendance",
    en = "Trend"
  ),
  fast_prewarm_running = list(
    fr = "Pré-calcul carte %s %s en cours…",
    en = "Pre-computing %s %s map…"
  ),
  fast_prewarm_done = list(
    fr = "Carte %s %s prête.",
    en = "%s %s map ready."
  ),
  fast_prewarm_failed = list(
    fr = "Carte %s %s non calculable.",
    en = "%s %s map unavailable."
  ),
  fast_prewarm_cancelled = list(
    fr = "Pré-calcul des cartes FAST annulé.",
    en = "FAST map pre-computation cancelled."
  ),
  # v0.70.1 — Toast court (4 s) émis à `fast_prewarm:complete` pour
  # signaler la fin du Diagnostic FAST côté UX. Le toast running
  # `fast_prewarm_progress` (persistent) est retiré juste avant.
  # Sans ce signal, l'utilisateur ne savait pas si l'application
  # était de nouveau disponible (le bouton « Lancer le diagnostic
  # FAST » se réactive automatiquement via l'observer status() mais
  # un signal explicite côté UI rassure).
  monitoring_fast_diagnostic_complete = list(
    fr = "Diagnostic FAST terminé — application disponible.",
    en = "FAST diagnostic complete — application available."
  ),
  monitoring_fast_alerts_legend_title = list(
    fr = "Intensité du déficit",
    en = "Deficit intensity"
  ),
  monitoring_fast_alerts_legend_count_title = list(
    fr = "Jours en alerte",
    en = "Alert days"
  ),
  # v0.57.0 — Affichage du mask FAST en quartiles 0-4 (spec 017 D2
  # nemeton@v0.55.0+). Le mask catégoriel sort de
  # `nemeton::compute_fast_alert_mask()`. La classe 0 (sain) est
  # rendue transparente — pas de libellé i18n nécessaire pour elle.
  fast_alert_legend_title = list(
    fr = "Sévérité de l'alerte (%s)",
    en = "Alert severity (%s)"
  ),
  fast_alert_class_1 = list(
    fr = "1 — Faible",
    en = "1 — Low"
  ),
  fast_alert_class_2 = list(
    fr = "2 — Modéré",
    en = "2 — Moderate"
  ),
  fast_alert_class_3 = list(
    fr = "3 — Fort",
    en = "3 — High"
  ),
  fast_alert_class_4 = list(
    fr = "4 — Sévère",
    en = "4 — Severe"
  ),
  monitoring_fast_alerts_opacity_label = list(
    fr = "Opacité du raster",
    en = "Raster opacity"
  ),
  # v0.61.0 — Clé `monitoring_fast_alerts_raster_visible` retirée.
  # Visibilité du raster d'alerte pilotée par le LayersControl
  # Leaflet (entrée « Alertes » sous « UGF »).
  monitoring_fast_alerts_threshold_label = list(
    fr = "Masquer en dessous du seuil",
    en = "Hide below threshold"
  ),
  # v0.42.0 — annotation des lignes horizontales de seuil sur le plot
  # modal pixel de Carte FAST (Livrable 3 spec 013). Format : %s reçoit
  # le nom de la bande ("NDVI" / "NBR"), %.2f la valeur du seuil.
  monitoring_pixel_plot_threshold_fmt = list(
    fr = "seuil %s %.2f",
    en = "%s threshold %.2f"
  ),

  # v0.36.0 — Carte FORDEAD module (nemeton::read_fordead_dieback_mask)
  # Empty state shown tant que le writer cœur n'a pas shippé.
  monitoring_fordead_map_empty_title = list(
    fr = "Aucun masque FORDEAD disponible",
    en = "No FORDEAD mask available"
  ),
  monitoring_fordead_map_empty_body = list(
    fr = "Lancer un diagnostic FORDEAD pour cette zone (mode Santé). Le masque classifié 0..4 du dépérissement s'affichera ici dès que le cœur Nemeton aura persisté le résultat sur disque (postprocess hook prévu dans une release ultérieure).",
    en = "Run a FORDEAD diagnostic for this zone (Health mode). The 0..4 classified dieback mask will appear here once the Nemeton core persists the run output on disk (postprocess hook scheduled for a later release)."
  ),
  monitoring_fordead_class_title = list(
    fr = "Classe de dépérissement",
    en = "Dieback class"
  ),
  # Partie B — sélecteur de couche pixel de la Carte FORDEAD.
  monitoring_fordead_layer_label = list(
    fr = "Couche",
    en = "Layer"
  ),
  monitoring_fordead_date_label = list(
    fr = "Détecté jusqu'au",
    en = "Detected up to"
  ),
  monitoring_fordead_layer_severity = list(
    fr = "Sévérité (0-4)",
    en = "Severity (0-4)"
  ),
  monitoring_fordead_layer_first_detection = list(
    fr = "Date de 1re détection",
    en = "First detection date"
  ),
  monitoring_fordead_layer_anomaly_index = list(
    fr = "Indice d'anomalie",
    en = "Anomaly index"
  ),
  monitoring_fordead_layer_confidence = list(
    fr = "Zone modélisée",
    en = "Modelled area"
  ),
  monitoring_fordead_layer_modelled_yes = list(
    fr = "Modélisé",
    en = "Modelled"
  ),
  monitoring_fordead_layer_modelled_no = list(
    fr = "Non modélisé",
    en = "Not modelled"
  ),
  monitoring_fordead_layer_legend_date = list(
    fr = "Date de 1re détection",
    en = "First detection date"
  ),
  monitoring_fordead_layer_legend_index = list(
    fr = "Indice d'anomalie",
    en = "Anomaly index"
  ),
  monitoring_fordead_layer_unavailable = list(
    fr = "Couche indisponible pour ce diagnostic (relancer un run FORDEAD récent pour la générer).",
    en = "Layer unavailable for this run (re-run a recent FORDEAD diagnosis to generate it)."
  ),
  # Explications (bouton « i ») de chaque couche pixel FORDEAD.
  monitoring_fordead_layer_info_severity = list(
    fr = "Classe de dépérissement par pixel (0 = sain, 1 = faible, 2 = moyenne, 3 = forte, 4 = sol nu). Les pixels sains (0) sont transparents : seuls les pixels affectés sont peints.",
    en = "Per-pixel dieback class (0 = healthy, 1 = low, 2 = moderate, 3 = high, 4 = bare soil). Healthy pixels (0) are transparent: only affected pixels are painted."
  ),
  monitoring_fordead_layer_info_first_detection = list(
    fr = "Date de la 1re anomalie de dépérissement détectée pour chaque pixel (du plus ancien au plus récent). Utile pour repérer la progression dans le temps.",
    en = "Date of the first detected dieback anomaly for each pixel (oldest to most recent). Useful to track progression over time."
  ),
  monitoring_fordead_layer_info_anomaly_index = list(
    fr = "Intensité continue de l'anomalie au dernier état (dernier indice d'anomalie CRSWIR). Plus la valeur est élevée, plus l'écart au modèle sain est marqué.",
    en = "Continuous anomaly intensity at the latest state (last CRSWIR anomaly index). Higher values mean a stronger departure from the healthy model."
  ),
  monitoring_fordead_layer_info_confidence = list(
    fr = "Pixels pour lesquels FORDEAD a pu calibrer un modèle exploitable (zone modélisée). Hors de cette zone, le diagnostic n'est pas fiable.",
    en = "Pixels for which FORDEAD could calibrate a usable model (modelled area). Outside this area the diagnosis is not reliable."
  ),
  monitoring_fordead_class_0 = list(
    fr = "sain",
    en = "healthy"
  ),
  monitoring_fordead_class_1 = list(
    fr = "faible",
    en = "low"
  ),
  monitoring_fordead_class_2 = list(
    fr = "moyenne",
    en = "medium"
  ),
  monitoring_fordead_class_3 = list(
    fr = "forte",
    en = "high"
  ),
  monitoring_fordead_class_4 = list(
    fr = "sol nu",
    en = "bare soil"
  ),

  # v0.59.0 (TODO #3) — Modal diagnostic pixel CRSWIR au clic sur la
  # carte FORDEAD. Affiche la série CRSWIR observée vs la prédiction
  # harmonique du modèle FORDEAD, avec marqueur vertical sur la
  # date de 1re anomalie détectée.
  monitoring_fordead_pixel_modal_title_fmt = list(
    fr = "Pixel CRSWIR FORDEAD \u2014 %.5f, %.5f",
    en = "FORDEAD CRSWIR pixel \u2014 %.5f, %.5f"
  ),
  monitoring_fordead_pixel_observed = list(
    fr = "Observ\u00e9",
    en = "Observed"
  ),
  monitoring_fordead_pixel_predicted = list(
    fr = "Pr\u00e9dit (harmonique)",
    en = "Predicted (harmonic)"
  ),
  monitoring_fordead_pixel_first_anomaly = list(
    fr = "1re anomalie",
    en = "First anomaly"
  ),
  monitoring_fordead_pixel_yaxis = list(
    fr = "Indice",
    en = "Index"
  ),
  # v0.72.0 \u2014 Wording plus explicite : la cause principale du NULL
  # est un clic hors zone mod\u00e9lis\u00e9e (extent du bundle FORDEAD plus
  # petit que l'AOI rendue sur la carte). On donne une instruction
  # actionnable. Duration toast bump\u00e9e \u00e0 8 s c\u00f4t\u00e9 handler.
  monitoring_fordead_pixel_no_data = list(
    fr = "Aucune s\u00e9rie CRSWIR disponible pour ce pixel. Cliquez DANS la zone d'alerte color\u00e9e du raster (extent mod\u00e9lis\u00e9 plus petit que l'AOI).",
    en = "No CRSWIR series available for this pixel. Click WITHIN the coloured alert zone of the raster (modelled extent smaller than the AOI)."
  ),
  # v0.72.0 \u2014 Nouvelles courbes/annotations du modal CRSWIR
  # enrichi (consume `seuil_haut`, `anomalie`, `dans_zone_validite`
  # du dataframe `nemeton::read_fordead_pixel_series()`).
  monitoring_fordead_pixel_threshold = list(
    fr = "Seuil de d\u00e9tection (pr\u00e9dit + \u0394)",
    en = "Detection threshold (predicted + \u0394)"
  ),
  monitoring_fordead_pixel_anomaly = list(
    fr = "Anomalie d\u00e9tect\u00e9e",
    en = "Detected anomaly"
  ),
  # Cat\u00e9gories de points du graphique pixel FORDEAD (rendu canonique
  # FORDEAD : training / healthy / anomaly / confirmed anomaly).
  monitoring_fordead_pixel_training = list(
    fr = "Entra\u00eenement",
    en = "Training"
  ),
  monitoring_fordead_pixel_healthy = list(
    fr = "Sain",
    en = "Healthy"
  ),
  monitoring_fordead_pixel_confirmed = list(
    fr = "Anomalie confirm\u00e9e",
    en = "Confirmed anomaly"
  ),
  monitoring_fordead_pixel_outside_validity = list(
    fr = "Pixel hors zone de validit\u00e9 (essence non calibr\u00e9e)",
    en = "Pixel outside calibration validity (uncalibrated species)"
  ),

  # ----- Pixel map sub-tab (spec 010) -----
  monitoring_pixel_map_title = list(
    fr = "Carte pixel \u2014 NDVI / NBR / NDMI \u00e0 la r\u00e9solution Sentinel-2 (10 m)",
    en = "Pixel map \u2014 NDVI / NBR / NDMI at Sentinel-2 native resolution (10 m)"
  ),
  monitoring_pixel_map_index = list(
    fr = "Indice spectral",
    en = "Spectral index"
  ),
  monitoring_pixel_map_date = list(
    fr = "Date d'observation",
    en = "Observation date"
  ),
  # v0.61.0 — Clé `monitoring_pixel_map_raster_visible` retirée.
  # Visibilité du raster pilotée par le LayersControl Leaflet
  # (entrée `"NDVI/NBR"`). Seul le slider d'opacité reste.
  monitoring_pixel_map_raster_opacity = list(
    fr = "Opacité du raster",
    en = "Raster opacity"
  ),
  monitoring_pixel_map_click_hint = list(
    fr = "Clique sur un pixel pour voir sa s\u00e9rie compl\u00e8te (NDMI + NDVI + NBR).",
    en = "Click a pixel for its full series (NDMI + NDVI + NBR)."
  ),
  monitoring_pixel_map_modal_title_fmt = list(
    fr = "Pixel \u00e0 (lat: %.5f, lon: %.5f)",
    en = "Pixel at (lat: %.5f, lon: %.5f)"
  ),
  # v0.52.16 \u2014 cl\u00e9s i18n `monitoring_pixel_map_placette_modal_title_fmt`
  # et `monitoring_pixel_map_no_placette_data` retir\u00e9es : la modale
  # \u00ab clic marqueur placette \u00bb n'existe plus (FAST = pure raster
  # per-pixel, sans contact avec les placettes Terrain).
  monitoring_pixel_map_no_cache = list(
    fr = "Pas de cache disque disponible. Lance le diagnostic FAST pour peupler le cache COG.",
    en = "No disk cache available. Run a FAST diagnosis to populate the COG cache."
  ),
  # v0.44.0 — diagnostic empty-states pour Carte FAST. Distingue le cas
  # "cache absent" (existant) de "cache présent mais 0 obs en DB"
  # (typique 403 SAS-token-expiry sur ingest long) et "build_index_stack
  # KO" (extents incohérents entre scènes, multi-tuile MGRS).
  monitoring_pixel_map_cache_no_obs_fmt = list(
    fr = "Cache COG présent (%d scènes sur disque) mais aucune observation extraite en base. C’est probablement dû à des erreurs HTTP 403 pendant la phase de crop (tokens SAS Azure expirés sur ingest long). Relance le diagnostic FAST.",
    en = "COG cache present (%d scenes on disk) but no observation extracted to the database. Typically caused by HTTP 403 errors during the crop phase (Azure SAS tokens expiring on long ingestions). Re-run the FAST diagnosis."
  ),
  monitoring_pixel_map_stack_failed_fmt = list(
    fr = "Empilement raster échoué : %s. Probablement des extents incohérents entre scènes (multi-tuile MGRS ou re-fetch partiel du cache).",
    en = "Raster stacking failed: %s. Likely extent mismatch between scenes (multi-tile MGRS or partial cache re-fetch)."
  ),
  monitoring_pixel_map_loading = list(
    fr = "Construction du stack d'indices\u2026",
    en = "Building index stack\u2026"
  ),
  monitoring_pixel_map_no_pixel = list(
    fr = "Aucune donn\u00e9e pour ce pixel (hors emprise ou bandes manquantes).",
    en = "No data for this pixel (out of bounds or missing bands)."
  ),
  monitoring_pixel_map_computing = list(
    fr = "Calcul du graphique pixel en cours\u2026",
    en = "Computing the pixel chart\u2026"
  ),
  monitoring_pixel_map_fullscreen = list(
    fr = "Plein \u00e9cran",
    en = "Fullscreen"
  ),
  monitoring_fast_raster_computing = list(
    fr = "Calcul du raster d'alerte en cours\u2026",
    en = "Computing the alert raster\u2026"
  ),
  # Indicateur unique bas-droite affiche des l'arrivee sur l'onglet Suivi
  # sanitaire, maintenu tant qu'un calcul lourd tourne (raster d'alerte FAST
  # ou build_index_stack du pixel map). Evite la fenetre sans feedback ou
  # l'utilisateur pouvait cliquer dans le vide pendant le scan a froid.
  monitoring_computing = list(
    fr = "Calcul en cours\u2026",
    en = "Computing\u2026"
  ),
  # --- Lissage d'affichage de la s\u00e9rie pixel (spec 026) -----------------
  monitoring_pixel_smooth_win_label = list(
    fr = "Lissage (jours)",
    en = "Smoothing (days)"
  ),
  monitoring_pixel_smooth_method_label = list(
    fr = "M\u00e9thode de lissage",
    en = "Smoothing method"
  ),
  monitoring_pixel_smooth_rolling = list(
    fr = "M\u00e9diane glissante",
    en = "Rolling median"
  ),
  monitoring_pixel_smooth_loess = list(
    fr = "LOESS",
    en = "LOESS"
  ),
  monitoring_pixel_smooth_harmonic = list(
    fr = "Harmonique",
    en = "Harmonic"
  ),
  monitoring_pixel_smooth_nharm_label = list(
    fr = "Harmoniques (cycles annuels)",
    en = "Harmonics (annual cycles)"
  ),
  monitoring_pixel_smooth_hover = list(
    fr = "liss\u00e9",
    en = "smoothed"
  ),
  # --- Graphe de tendance par pixel (Alertes FAST, mode Tendance) -------
  yes = list(fr = "oui", en = "yes"),
  no  = list(fr = "non", en = "no"),
  fast_trend_pixel_computing = list(
    fr = "Calcul du graphique de tendance en cours\u2026",
    en = "Computing the trend chart\u2026"
  ),
  fast_trend_pixel_no_data = list(
    fr = "Aucune donn\u00e9e de tendance pour ce pixel (hors emprise, bandes manquantes ou ann\u00e9es insuffisantes).",
    en = "No trend data for this pixel (out of bounds, missing bands or not enough years)."
  ),
  fast_trend_pixel_title_fmt = list(
    fr = "Tendance %s \u2014 pixel (%s, %s)",
    en = "%s trend \u2014 pixel (%s, %s)"
  ),
  fast_trend_pixel_xaxis = list(fr = "Ann\u00e9e", en = "Year"),
  fast_trend_pixel_yaxis = list(
    fr = "%s composite estival",
    en = "%s summer composite"
  ),
  fast_trend_pixel_theilsen = list(
    fr = "Tendance Theil-Sen",
    en = "Theil-Sen trend"
  ),
  fast_trend_pixel_slope_fmt = list(
    fr = "Pente : %s /an",
    en = "Slope: %s /yr"
  ),
  fast_trend_pixel_pvalue_fmt = list(
    fr = "p (Mann-Kendall) : %s",
    en = "p (Mann-Kendall): %s"
  ),
  fast_trend_pixel_signif_fmt = list(
    fr = "D\u00e9clin significatif : %s",
    en = "Significant decline: %s"
  ),
  fast_trend_pixel_nyears_fmt = list(
    fr = "Ann\u00e9es valides : %s",
    en = "Valid years: %s"
  ),
  fast_trend_pixel_severity_fmt = list(
    fr = "Classe de s\u00e9v\u00e9rit\u00e9 : %s",
    en = "Severity class: %s"
  ),
  fast_trend_pixel_fullscreen = list(
    fr = "Plein \u00e9cran",
    en = "Fullscreen"
  ),
  monitoring_pixel_map_scene_count_fmt = list(
    fr = "%d sc\u00e8nes disponibles dans le cache.",
    en = "%d scenes available in the cache."
  ),
  # v0.36.5 \u2014 card \u00ab Zone saine \u00bb (Carte FORDEAD, overlay) quand un run
  # se termine sans pixel affect\u00e9. \u00c9vite que l'utilisateur confonde
  # \u00ab pas encore lanc\u00e9 \u00bb / \u00ab run en cours \u00bb / \u00ab 0 anomalie \u00bb.
  # v0.92.x \u2014 `monitoring_alerts_placeholder` + les variantes meta de la
  # card sont retir\u00e9es avec le sous-onglet \u00ab Alertes FORDEAD \u00bb (doublon
  # de la Carte FORDEAD). L'overlay du sous-module n'affiche que titre+body.
  monitoring_fordead_no_alerts_title = list(
    fr = "Zone saine \u2014 aucune anomalie d\u00e9tect\u00e9e",
    en = "Healthy zone \u2014 no anomaly detected"
  ),
  monitoring_fordead_no_alerts_body = list(
    fr = "Aucun pixel ne d\u00e9passe le seuil d'anomalie de d\u00e9p\u00e9rissement sur la p\u00e9riode. Le diagnostic FORDEAD s'est termin\u00e9 sans d\u00e9tecter de zone affect\u00e9e.",
    en = "No pixel exceeds the dieback anomaly threshold over the period. The FORDEAD diagnosis completed without detecting any affected area."
  ),

  # ----- Mode toggle (rapide / sanitaire) -----
  monitoring_mode_label = list(
    fr = "Mode de suivi",
    en = "Monitoring mode"
  ),
  monitoring_mode_quick = list(
    fr = "Diagnostic FAST (spot/trend)",
    en = "FAST diagnosis (spot/trend)"
  ),
  monitoring_mode_health = list(
    fr = "Diagnostic FORDEAD (r\u00e9sineux)",
    en = "FORDEAD diagnosis (conifers)"
  ),
  monitoring_mode_quick_help = list(
    fr = "D\u00e9tection de chocs r\u00e9cents (coupe, chablis, incendie) via NDMI/NDVI/NBR rolling-window + trend.",
    en = "Detect recent shocks (cut, windthrow, fire) using NDMI/NDVI/NBR rolling-window + trend."
  ),
  monitoring_mode_health_help = list(
    fr = "D\u00e9tection de d\u00e9p\u00e9rissement progressif (scolyte, s\u00e9cheresse) via FORDEAD.",
    en = "Detect progressive dieback (bark beetle, drought) using FORDEAD."
  ),

  # ----- FORDEAD parameters -----
  monitoring_dates_training_label = list(
    fr = "P\u00e9riode d'entra\u00eenement (saine)",
    en = "Training period (healthy reference)"
  ),
  monitoring_dates_monitoring_label = list(
    fr = "P\u00e9riode de suivi",
    en = "Monitoring period"
  ),
  monitoring_dates_observation_label = list(
    fr = "P\u00e9riode d'observation",
    en = "Observation period"
  ),
  monitoring_threshold_anomaly = list(
    fr = "Seuil d'anomalie (CRSWIR)",
    en = "Anomaly threshold (CRSWIR)"
  ),
  monitoring_vegetation_index = list(
    fr = "Indice de v\u00e9g\u00e9tation",
    en = "Vegetation index"
  ),
  monitoring_run_health_btn = list(
    fr = "Lancer le diagnostic FORDEAD",
    en = "Run FORDEAD diagnosis"
  ),
  monitoring_health_starting = list(
    fr = "Diagnostic FORDEAD lanc\u00e9. Premier calcul : plusieurs heures \u00e0 un jour si toutes les dates ne sont pas d\u00e9j\u00e0 t\u00e9l\u00e9charg\u00e9es par le Diagnostic FAST ; nettement plus rapide ensuite.",
    en = "FORDEAD diagnosis started. First run: several hours to a day if not all dates have already been downloaded by FAST; much faster afterwards."
  ),
  monitoring_health_success = list(
    fr = "Diagnostic termin\u00e9 : %d alertes ins\u00e9r\u00e9es en %.0f s.",
    en = "Diagnosis completed: %d alerts inserted in %.0f s."
  ),
  # Phase A (spec 008 \u00a715, D2) \u2014 le c\u0153ur ne renvoie plus de d\u00e9compte
  # d'alertes (n_alerts_inserted = NA ; \u00ab zone saine \u00bb se d\u00e9cide sur le
  # raster, pas sur un compte DB). La notif de fin n'annonce donc plus
  # \u00ab N alertes \u00bb \u2014 uniquement la dur\u00e9e. Le verdict sain/affect\u00e9 est lu
  # sur la carte FORDEAD (raster masqu\u00e9 par strate).
  monitoring_health_success_done = list(
    fr = "Diagnostic FORDEAD termin\u00e9 en %.0f s.",
    en = "FORDEAD diagnosis completed in %.0f s."
  ),
  monitoring_health_error = list(
    fr = "Erreur lors du diagnostic FORDEAD",
    en = "FORDEAD diagnosis error"
  ),
  monitoring_health_phase_fmt = list(
    fr = "FORDEAD — phase %s (%d/%d)",
    en = "FORDEAD — phase %s (%d/%d)"
  ),
  monitoring_health_phase_simple_fmt = list(
    fr = "FORDEAD — phase %s",
    en = "FORDEAD — phase %s"
  ),

  # ----- FORDEAD phase toasts (v0.32.0) ---------------------------------
  # Templates use glue-style {placeholder} args because get_i18n()$t()
  # runs glue::glue() on the result. The fordead:* dispatcher in
  # R/mod_monitoring.R passes named args matching these placeholders.
  # Per-phase labels are resolved by .fordead_phase_label() — when a
  # future nemeton release emits a phase_name with no matching key
  # here, a Title-Cased version of the raw name is used as fallback.
  #
  # Accents are encoded as \uXXXX per the convention enforced in this
  # file (utils_i18n.R, ~360 keys — see CLAUDE.md i18n rule 4).
  monitoring_fordead_phase_progress = list(
    fr = "Phase {n}/{total} \u2014 {label}",
    en = "Phase {n}/{total} \u2014 {label}"
  ),
  monitoring_fordead_phase_done = list(
    fr = "\u2713 {label}",
    en = "\u2713 {label}"
  ),
  monitoring_fordead_complete = list(
    fr = "FORDEAD termin\u00e9 \u00b7 {n} alertes \u00b7 {sec}s",
    en = "FORDEAD complete \u00b7 {n} alerts \u00b7 {sec}s"
  ),
  monitoring_fordead_error = list(
    fr = "FORDEAD \u00e9chec en {phase} : {msg}",
    en = "FORDEAD failed at {phase}: {msg}"
  ),

  # ----- RECONFORT (L6, spec 021) ---------------------------------------
  # 3e mode de Suivi sanitaire : deperissement des feuillus via RECONFORT
  # (CRSWIR + CRre, pas de modele harmonique). Accents en \uXXXX (rule 4).
  monitoring_mode_reconfort = list(
    fr = "Diagnostic RECONFORT (feuillus)",
    en = "RECONFORT diagnosis (broadleaves)"
  ),
  monitoring_mode_reconfort_help = list(
    fr = "D\u00e9tection du d\u00e9p\u00e9rissement des feuillus (ch\u00eane, ch\u00e2taignier) via RECONFORT (CRSWIR + CRre). Quelques minutes, opt-in (conda IOTA\u00b2/GEODES).",
    en = "Detect broadleaf dieback (oak, chestnut) using RECONFORT (CRSWIR + CRre). A few minutes, opt-in (conda IOTA\u00b2/GEODES)."
  ),
  # Libellé relabellisé « Alertes RECONFORT » (l'onglet affiche les
  # clusters d'alertes vectorielles + clic-pixel ; nom de clé conservé,
  # legacy). Symétrie avec « Alertes FAST » / « Alertes FORDEAD ».
  monitoring_subtab_pixel_map_reconfort = list(
    fr = "Alertes RECONFORT",
    en = "RECONFORT alerts"
  ),
  monitoring_reconfort_outside_validity = list(
    fr = "Zone hors domaine de calibration RECONFORT \u2014 r\u00e9sultats indicatifs (avertissement, non bloquant).",
    en = "Zone outside the RECONFORT calibration domain \u2014 results are indicative (advisory, non-blocking)."
  ),
  monitoring_reconfort_outside_validity_title = list(
    fr = "Zone hors domaine de calibration RECONFORT",
    en = "Area outside the RECONFORT calibration domain"
  ),
  monitoring_reconfort_year_incomplete = list(
    fr = "Année Sentinel-2 incomplète : la saison n'est pas terminée. Choisissez au plus la dernière année complète.",
    en = "Incomplete Sentinel-2 year: the season is not over yet. Pick at most the latest complete year."
  ),
  # --- Planche pixel dépérissement 4 panneaux (Partie B) ---
  pixel_planche_title = list(fr = "Pixel \u2014 s\u00e9ries pluriannuelles des deux indices (fond orange = \u00e9t\u00e9)", en = "Pixel \u2014 multi-year series of both indices (orange band = summer)"),
  pixel_title_of_species = list(fr = "Pixel de %s", en = "%s pixel"),
  pixel_title_bare = list(fr = "Pixel", en = "Pixel"),
  pixel_title_suffix = list(fr = " \u2014 s\u00e9ries pluriannuelles des deux indices (fond orange = \u00e9t\u00e9)", en = " \u2014 multi-year series of both indices (orange band = summer)"),
  pixel_title_coords_fmt = list(fr = "lat : %.5f, lon : %.5f", en = "lat: %.5f, lon: %.5f"),
  pixel_smooth_label = list(fr = "Lissage", en = "Smoothing"),
  pixel_points_label = list(fr = "Observations brutes",
                            en = "Raw observations"),
  pixel_trough_swir = list(fr = "Trajectoire du creux estival CRswir", en = "Summer CRswir trough trajectory"),
  pixel_peak_re = list(fr = "Trajectoire du pic estival CRre", en = "Summer CRre peak trajectory"),
  pixel_centroids = list(fr = "Centroïdes annuels", en = "Annual centroids"),
  pixel_doy_axis = list(fr = "Jour de l'année", en = "Day of year"),
  pixel_cycles_swir = list(fr = "CRswir : cycles annuels repli\u00e9s", en = "CRswir: folded annual cycles"),
  pixel_cycles_re = list(fr = "CRre : cycles annuels repli\u00e9s", en = "CRre: folded annual cycles"),
  pixel_state_space = list(fr = "Espace CRswir \u00d7 CRre (\u00e9t\u00e9)", en = "CRswir \u00d7 CRre space (summer)"),
  pixel_year = list(fr = "Année", en = "Year"),
  pixel_legend_crswir = list(fr = "CRswir (lissage l\u00e9ger)", en = "CRswir (light smoothing)"),
  pixel_legend_crre = list(fr = "CRre (lissage l\u00e9ger)", en = "CRre (light smoothing)"),
  pixel_axis_crswir = list(fr = "CRswir (\u2191 = plus d'eau)", en = "CRswir (\u2191 = more water)"),
  pixel_axis_crre = list(fr = "CRre (\u2191 = plus de chlorophylle)", en = "CRre (\u2191 = more chlorophyll)"),
  pixel_axis_crswir_water = list(fr = "CRswir (eau)", en = "CRswir (water)"),
  pixel_axis_crre_chloro = list(fr = "CRre (chlorophylle)", en = "CRre (chlorophyll)"),
  pixel_annot_trough = list(fr = "creux estival qui s'enfonce", en = "deepening summer trough"),
  pixel_annot_peak = list(fr = "pic plus faible = s\u00e9nescence pr\u00e9coce", en = "lower peak = early senescence"),
  pixel_annot_drift = list(fr = "d\u00e9rive vers eau ET chloro. basses", en = "drift toward low water AND chlorophyll"),
  pixel_smooth_none = list(fr = "Aucun", en = "None"),
  pixel_smooth_light = list(fr = "Léger", en = "Light"),
  pixel_table_label = list(fr = "Table des données (série pixel)",
                           en = "Data table (pixel series)"),
  pixel_plot_alt = list(
    fr = "Planche 4 panneaux du suivi pluriannuel du pixel : séries CRswir/CRre à double axe, cycles annuels repliés et espace d'état.",
    en = "4-panel plate of multi-year pixel monitoring: dual-axis CRswir/CRre series, folded annual cycles and state space."
  ),
  pixel_export_png = list(fr = "Exporter PNG", en = "Export PNG"),
  pixel_export_failed = list(
    fr = "Export PNG indisponible (moteur kaleido/webshot2 absent).",
    en = "PNG export unavailable (kaleido/webshot2 engine missing)."
  ),
  monitoring_reconfort_outside_validity_body = list(
    fr = "R\u00e9sultats indicatifs (avertissement, non bloquant).",
    en = "Results are indicative (advisory, non-blocking)."
  ),
  monitoring_reconfort_map_empty_title = list(
    fr = "Aucune alerte RECONFORT",
    en = "No RECONFORT alert"
  ),
  monitoring_reconfort_map_empty_body = list(
    fr = "Lancez un diagnostic RECONFORT ou s\u00e9lectionnez une zone disposant d'un run pour afficher les alertes.",
    en = "Run a RECONFORT diagnosis or pick a zone with an existing run to display alerts."
  ),
  monitoring_reconfort_class_2 = list(
    fr = "D\u00e9p\u00e9rissant",
    en = "Declining"
  ),
  monitoring_reconfort_class_3 = list(
    fr = "Tr\u00e8s d\u00e9p\u00e9rissant",
    en = "Severely declining"
  ),
  monitoring_reconfort_class_title = list(
    fr = "Classe de d\u00e9p\u00e9rissement",
    en = "Dieback class"
  ),
  monitoring_reconfort_popup_class = list(fr = "Classe", en = "Class"),
  monitoring_reconfort_popup_stress = list(
    fr = "Indice de stress",
    en = "Stress index"
  ),
  monitoring_reconfort_pixel_no_data = list(
    fr = "Diagnostic pixel indisponible \u00e0 cet emplacement (aucun run RECONFORT, ou pixel hors emprise mod\u00e9lis\u00e9e).",
    en = "Pixel diagnosis unavailable here (no RECONFORT run, or pixel outside the modelled extent)."
  ),
  monitoring_reconfort_crswir = list(
    fr = "CRSWIR observ\u00e9",
    en = "Observed CRSWIR"
  ),
  monitoring_reconfort_crre = list(
    fr = "CRre observ\u00e9",
    en = "Observed CRre"
  ),
  monitoring_reconfort_species = list(fr = "Essence", en = "Species"),
  monitoring_reconfort_model = list(fr = "Mod\u00e8le", en = "Model"),
  monitoring_reconfort_pixel_yaxis = list(
    fr = "Indice spectral",
    en = "Spectral index"
  ),
  monitoring_reconfort_pixel_modal_title_fmt = list(
    fr = "Diagnostic pixel RECONFORT (%.5f, %.5f)",
    en = "RECONFORT pixel diagnosis (%.5f, %.5f)"
  ),
  monitoring_reconfort_s2_year = list(
    fr = "Ann\u00e9e Sentinel-2",
    en = "Sentinel-2 year"
  ),
  # ----- RECONFORT couches carte (toggles + opacit\u00e9) -----
  # Libell\u00e9s \u00e9mis par le manifeste c\u0153ur (reconfort_layer_manifest$label_key).
  reconfort_couches = list(
    fr = "Couches",
    en = "Layers"
  ),
  reconfort_opacite = list(
    fr = "Opacit\u00e9 du raster",
    en = "Raster opacity"
  ),
  reconfort_couche_score = list(
    fr = "Score de d\u00e9p\u00e9rissement",
    en = "Dieback score"
  ),
  reconfort_couche_classes = list(
    fr = "Classes de sant\u00e9",
    en = "Health classes"
  ),
  reconfort_couche_proba = list(
    fr = "Probabilit\u00e9",
    en = "Probability"
  ),
  reconfort_couche_alertes = list(
    fr = "Alertes",
    en = "Alerts"
  ),
  # Info-bulles « i » par couche (parite FORDEAD).
  reconfort_couche_score_info = list(
    fr = "Score continu de d\u00e9p\u00e9rissement issu du mod\u00e8le RECONFORT. Plus la valeur est \u00e9lev\u00e9e, plus le d\u00e9p\u00e9rissement est s\u00e9v\u00e8re. \u00c9chelle de couleur par quantiles (pleine intensit\u00e9).",
    en = "Continuous dieback score from the RECONFORT model. Higher means more severe dieback. Quantile colour scale (full contrast)."
  ),
  reconfort_couche_classes_info = list(
    fr = "Classification en classes de sant\u00e9 (1-sain transparent, 2-d\u00e9p\u00e9rissant, 3-tr\u00e8s d\u00e9p\u00e9rissant). Seuls les pixels affect\u00e9s sont peints.",
    en = "Health-class classification (1-healthy transparent, 2-declining, 3-severely declining). Only affected pixels are painted."
  ),
  reconfort_couche_proba_info = list(
    fr = "Probabilit\u00e9 / confiance de la classification du mod\u00e8le. \u00c9chelle de couleur par quantiles.",
    en = "Model classification probability / confidence. Quantile colour scale."
  ),
  reconfort_couche_alertes_info = list(
    fr = "Placettes en alerte (classes 2-d\u00e9p\u00e9rissant / 3-tr\u00e8s d\u00e9p\u00e9rissant), filtr\u00e9es \u00e0 la zone de suivi. Clic sur la carte : diagnostic pixel CRSWIR / CRre.",
    en = "Alert plots (classes 2-declining / 3-severely declining), clipped to the monitoring zone. Click the map for the CRSWIR / CRre pixel diagnosis."
  ),
  monitoring_run_reconfort_btn = list(
    fr = "Lancer le diagnostic RECONFORT",
    en = "Run RECONFORT diagnosis"
  ),
  monitoring_reconfort_run_unavailable = list(
    fr = "Lancement RECONFORT indisponible : environnement conda (IOTA\u00b2/GEODES) requis. La carte et le diagnostic restent accessibles sur les runs d\u00e9j\u00e0 produits.",
    en = "RECONFORT run unavailable: conda environment (IOTA\u00b2/GEODES) required. The map and pixel diagnosis remain available on existing runs."
  ),
  monitoring_reconfort_starting = list(
    fr = "Diagnostic RECONFORT lanc\u00e9. Premier calcul \u00e0 froid : plusieurs heures ; nettement plus rapide ensuite.",
    en = "RECONFORT diagnosis started. First cold run: several hours; much faster afterwards."
  ),
  monitoring_reconfort_success = list(
    fr = "Diagnostic RECONFORT termin\u00e9 : %d alertes ins\u00e9r\u00e9es en %.0f s.",
    en = "RECONFORT diagnosis completed: %d alerts inserted in %.0f s."
  ),
  monitoring_reconfort_error = list(
    fr = "Erreur lors du diagnostic RECONFORT",
    en = "RECONFORT diagnosis error"
  ),
  monitoring_reconfort_phase_progress = list(
    fr = "Phase {n}/{total} \u2014 {label}",
    en = "Phase {n}/{total} \u2014 {label}"
  ),
  monitoring_reconfort_phase_done = list(
    fr = "\u2713 {label}",
    en = "\u2713 {label}"
  ),
  monitoring_reconfort_ingest_listed = list(
    fr = "Ingestion S2 \u00b7 {total} sc\u00e8ne(s) \u00e0 traiter",
    en = "S2 ingestion \u00b7 {total} scene(s) to process"
  ),
  monitoring_reconfort_ingest_item = list(
    fr = "Sc\u00e8ne {n}/{total} \u2014 {detail}",
    en = "Scene {n}/{total} \u2014 {detail}"
  ),
  monitoring_reconfort_step_download = list(
    fr = "t\u00e9l\u00e9chargement",
    en = "downloading"
  ),
  monitoring_reconfort_step_crop = list(
    fr = "extraction + recadrage AOI",
    en = "extracting + cropping to AOI"
  ),
  monitoring_reconfort_step_done = list(
    fr = "recadr\u00e9e",
    en = "cropped"
  ),
  monitoring_reconfort_step_cached = list(
    fr = "d\u00e9j\u00e0 en cache",
    en = "already cached"
  ),
  monitoring_reconfort_step_failed = list(
    fr = "\u00e9chec, ignor\u00e9e",
    en = "failed, skipped"
  ),
  monitoring_reconfort_complete = list(
    fr = "RECONFORT termin\u00e9 \u00b7 {n} alertes \u00b7 {sec}s",
    en = "RECONFORT complete \u00b7 {n} alerts \u00b7 {sec}s"
  ),
  monitoring_reconfort_phase_env = list(
    fr = "Pr\u00e9paration de l'environnement",
    en = "Environment setup"
  ),
  monitoring_reconfort_phase_model = list(
    fr = "Chargement du mod\u00e8le",
    en = "Model loading"
  ),
  monitoring_reconfort_phase_ingest = list(
    fr = "Ingestion Sentinel-2",
    en = "Sentinel-2 ingestion"
  ),
  monitoring_reconfort_phase_mapprod = list(
    fr = "Production des cartes",
    en = "Map production"
  ),
  monitoring_reconfort_phase_postprocess = list(
    fr = "Post-traitement",
    en = "Post-processing"
  ),
  monitoring_reconfort_phase_persist = list(
    fr = "Persistance des r\u00e9sultats",
    en = "Persisting results"
  ),
  monitoring_reconfort_phase_mask = list(
    fr = "Masque forestier",
    en = "Forest mask"
  ),
  monitoring_reconfort_phase_tiles = list(
    fr = "Tuiles",
    en = "Tiles"
  ),
  monitoring_reconfort_phase_stage = list(
    fr = "Pr\u00e9paration",
    en = "Staging"
  ),
  monitoring_reconfort_phase_collect = list(
    fr = "Collecte des r\u00e9sultats",
    en = "Collecting results"
  ),

  # ----- ntfy push messages (E6 \u2014 out-of-band FORDEAD progress) ------
  # Sent worker-side by .ntfy_send() so a FORDEAD run that outlives
  # its Shiny session still notifies the user. These strings are
  # consumed via sprintf() (NOT glue) \u2014 hence %s / %d placeholders,
  # which glue::glue() leaves untouched. Opt-in: no-op unless
  # NEMETON_NTFY_TOPIC is set. Cf. service_monitoring.R.
  monitoring_ntfy_fordead_start = list(
    fr = "Diagnostic FORDEAD d\u00e9marr\u00e9 (zone %s).",
    en = "FORDEAD diagnosis started (zone %s)."
  ),
  monitoring_ntfy_fordead_phase = list(
    fr = "\u00c9tape en cours : %s",
    en = "Current step: %s"
  ),
  monitoring_ntfy_fordead_complete = list(
    fr = "Diagnostic FORDEAD termin\u00e9 : %d alerte(s) en %s.",
    en = "FORDEAD diagnosis complete: %d alert(s) in %s."
  ),
  monitoring_ntfy_fordead_error = list(
    fr = "\u00c9chec du diagnostic FORDEAD : %s",
    en = "FORDEAD diagnosis failed: %s"
  ),

  # ntfy push messages for RECONFORT (spec 021) \u2014 sym\u00e9trique avec
  # FORDEAD ci-dessus. M\u00eame opt-in via NEMETON_NTFY_TOPIC ; sprintf
  # placeholders (i18n$t() rendu brut puis sprintf). \u00c9mis worker-side
  # par run_reconfort_async() : start avant l'appel n\u00e9meton, phase \u00e0
  # chaque nouvelle \u00e9tape (de-dup via .build_reconfort_progress_callback),
  # complete/error en sortie.
  monitoring_ntfy_reconfort_start = list(
    fr = "Diagnostic RECONFORT d\u00e9marr\u00e9 (zone %s).",
    en = "RECONFORT diagnosis started (zone %s)."
  ),
  monitoring_ntfy_reconfort_phase = list(
    fr = "\u00c9tape en cours : %s",
    en = "Current step: %s"
  ),
  monitoring_ntfy_reconfort_complete = list(
    fr = "Diagnostic RECONFORT termin\u00e9 : %d alerte(s) en %s.",
    en = "RECONFORT diagnosis complete: %d alert(s) in %s."
  ),
  monitoring_ntfy_reconfort_error = list(
    fr = "\u00c9chec du diagnostic RECONFORT : %s",
    en = "RECONFORT diagnosis failed: %s"
  ),

  # v0.42.1 \u2014 ntfy push messages for FAST ingestion (sym\u00e9trique avec
  # FORDEAD ci-dessus). M\u00eame opt-in via NEMETON_NTFY_TOPIC ; sprintf
  # placeholders. Sent worker-side par run_ingestion_async() : start
  # avant l'appel n\u00e9meton, scenes (one-shot) \u00e0 la premi\u00e8re event
  # s2:scene, complete/error en sortie.
  # v0.71.1 \u2014 Wording align\u00e9 sur le toast UI et le push complete
  # (\u00ab Diagnostic FAST termin\u00e9 \u00bb). Coh\u00e9rence end-to-end : d\u00e9marrage
  # = \u00ab Diagnostic FAST d\u00e9marr\u00e9 \u00bb, fin = \u00ab Diagnostic FAST termin\u00e9 \u00bb.
  monitoring_ntfy_ingest_start = list(
    fr = "Diagnostic FAST d\u00e9marr\u00e9 (zone %s).",
    en = "FAST diagnostic started (zone %s)."
  ),
  monitoring_ntfy_ingest_scenes = list(
    fr = "T\u00e9l\u00e9chargement Sentinel-2 : %d sc\u00e8nes \u00e0 traiter.",
    en = "Sentinel-2 download: %d scenes to process."
  ),
  # v0.70.4 \u2014 Wording align\u00e9 sur le toast UI
  # (`monitoring_ingest_success` = \u00ab Diagnostic FAST termin\u00e9 \u00bb).
  # Retrait du `%d observations` : toujours 0 depuis `nemeton@v0.58.0`
  # (drop obs_pixel insertion) \u2014 cf. commentaire `mod_monitoring.R:2049`
  # qui l'avait d\u00e9j\u00e0 retir\u00e9 c\u00f4t\u00e9 toast UI. ntfy l'affichait toujours
  # \u00e0 0, source de confusion. Les 2 `%d %s` du nouveau format sont
  # `(n_scenes, duration)` \u2014 l'appelant `service_monitoring.R:303`
  # doit retirer l'argument `n_obs_inserted` du `sprintf`.
  monitoring_ntfy_ingest_complete = list(
    fr = "Diagnostic FAST termin\u00e9 : %d sc\u00e8ne(s) en cache (%s).",
    en = "FAST diagnostic done: %d scene(s) cached (%s)."
  ),
  monitoring_ntfy_ingest_error = list(
    fr = "\u00c9chec de l'ingestion FAST : %s",
    en = "FAST ingestion failed: %s"
  ),

  # v0.43.0 \u2014 Plan de validation terrain (spec 014). Sous-onglet
  # \u00ab Plan de validation \u00bb de Suivi sanitaire, encapsule
  # nemeton::create_validation_sampling_plan(). Voir
  # mod_validation_sampling.R + service_validation_sampling.R.
  validation_sampling_title = list(
    fr = "Plan de validation",
    en = "Validation plan"
  ),
  # v0.43.3 — split du sous-onglet validation en 2 instances mode-driven
  # (symétriques avec Alertes FAST/FORDEAD et Carte FAST/FORDEAD).
  # Source figée par onglet, plus de radio à choisir.
  validation_sampling_title_fast = list(
    fr = "Plan de validation FAST",
    en = "FAST validation plan"
  ),
  validation_sampling_title_fordead = list(
    fr = "Plan de validation FORDEAD",
    en = "FORDEAD validation plan"
  ),
  validation_source_label = list(
    fr = "Source d'alerte",
    en = "Alert source"
  ),
  validation_n_validation_label = list(
    fr = "Placettes de validation",
    en = "Validation plots"
  ),
  validation_n_control_label = list(
    fr = "Placettes t\u00e9moins",
    en = "Control plots"
  ),
  validation_classes_label = list(
    fr = "Classes d'alerte retenues",
    en = "Retained alert classes"
  ),
  # Spec control_classes (v0.51.0) — sélection des classes servant à
  # tirer les placettes témoins + aide au choix (distribution du raster).
  validation_control_classes_label = list(
    fr = "Classes consid\u00e9r\u00e9es comme t\u00e9moins",
    en = "Classes used as controls"
  ),
  validation_class_distribution_fmt = list(
    fr = "Distribution du raster : %s",
    en = "Raster distribution: %s"
  ),
  validation_no_healthy_pixel_hint = list(
    fr = "Aucun pixel sain (classe 0). Cochez une classe plus haute pour tirer des t\u00e9moins (ex. 3).",
    en = "No healthy pixel (class 0). Tick a higher class to draw controls (e.g. 3)."
  ),
  validation_control_auto_relaxed = list(
    fr = "Aucun pixel sain \u2014 t\u00e9moins tir\u00e9s en classe %s (la plus saine disponible).",
    en = "No healthy pixel \u2014 controls drawn from class %s (the healthiest available)."
  ),
  validation_no_control_warning = list(
    fr = "Aucune placette t\u00e9moin g\u00e9n\u00e9r\u00e9e : aucune cellule dans les classes t\u00e9moins choisies.",
    en = "No control plot generated: no cell in the chosen control classes."
  ),
  # v0.45.0 — labels unifiés FAST entre Alertes FAST (légende) et
  # Plan de validation FAST (checkboxes). FAST = quartiles du raster
  # d'alerte courant, calculés dynamiquement via .fast_class_labels().
  # FORDEAD = labels biologiques statiques (le mask catégoriel
  # FORDEAD est fixe).
  validation_class_unit_days = list(
    fr = "j",
    en = "d"
  ),
  validation_class_unit_deficit = list(
    fr = "",
    en = ""
  ),
  validation_class_unit_trend = list(
    fr = "pente/an",
    en = "slope/yr"
  ),
  # Fallback FAST quand le raster d'alerte n'est pas encore calculé
  # (cache absent, ingestion pas encore lancée, etc.) — labels
  # génériques par classe.
  validation_class_fast_1 = list(
    fr = "1 — faible (moins fiable)",
    en = "1 — low (less reliable)"
  ),
  validation_class_fast_2 = list(
    fr = "2 — moyenne (moins fiable)",
    en = "2 — medium (less reliable)"
  ),
  validation_class_fast_3 = list(
    fr = "3 — forte",
    en = "3 — high"
  ),
  validation_class_fast_4 = list(
    fr = "4 — très forte",
    en = "4 — very high"
  ),
  # FORDEAD : libellés biologiques fixes (mapping cœur du masque
  # catégoriel produit par run_fordead_dieback()).
  validation_class_fordead_1 = list(
    fr = "1 — faible (moins fiable)",
    en = "1 — low (less reliable)"
  ),
  validation_class_fordead_2 = list(
    fr = "2 — moyenne (moins fiable)",
    en = "2 — medium (less reliable)"
  ),
  validation_class_fordead_3 = list(
    fr = "3 — forte",
    en = "3 — high"
  ),
  validation_class_fordead_4 = list(
    fr = "4 — sol nu",
    en = "4 — bare soil"
  ),
  # ----- RECONFORT validation (spec 021 G4) -----
  validation_sampling_title_reconfort = list(
    fr = "Plan de validation RECONFORT",
    en = "RECONFORT validation plan"
  ),
  reconfort_class_label_1 = list(fr = "1 — sain", en = "1 — healthy"),
  reconfort_class_label_2 = list(
    fr = "2 — d\u00e9p\u00e9rissant",
    en = "2 — declining"
  ),
  reconfort_class_label_3 = list(
    fr = "3 — tr\u00e8s d\u00e9p\u00e9rissant",
    en = "3 — severely declining"
  ),
  validation_buffer_label = list(
    fr = "Tampon (m) autour des alertes",
    en = "Buffer (m) around alerts"
  ),
  validation_seed_label = list(
    fr = "Graine al\u00e9atoire",
    en = "Random seed"
  ),
  # --- Pond\u00e9ration continue du tirage (spec 014, FORDEAD/RECONFORT) -------
  validation_weighting_label = list(
    fr = "Pond\u00e9ration du tirage",
    en = "Draw weighting"
  ),
  validation_weighting_continuous = list(
    fr = "Par s\u00e9v\u00e9rit\u00e9 (continu)",
    en = "By severity (continuous)"
  ),
  validation_weighting_uniform = list(
    fr = "Par classe (uniforme)",
    en = "By class (uniform)"
  ),
  validation_alert_weight_col = list(
    fr = "S\u00e9v\u00e9rit\u00e9",
    en = "Severity"
  ),
  validation_weight_mismatch_title = list(
    fr = "Couche de s\u00e9v\u00e9rit\u00e9 inexploitable",
    en = "Severity layer unusable"
  ),
  validation_weight_mismatch_msg = list(
    fr = "Couche de s\u00e9v\u00e9rit\u00e9 non g\u00e9or\u00e9f\u00e9renc\u00e9e : tirage impossible.",
    en = "Severity layer not georeferenced: cannot draw."
  ),
  validation_generate_btn = list(
    fr = "G\u00e9n\u00e9rer plan de validation",
    en = "Generate validation plan"
  ),
  validation_persist_btn = list(
    fr = "Persister dans samples.gpkg",
    en = "Persist to samples.gpkg"
  ),
  validation_export_qgis_btn = list(
    fr = "Exporter pour QGIS",
    en = "Export for QGIS"
  ),
  validation_idle_hint = list(
    fr = "Configurer les param\u00e8tres puis cliquer \u00ab G\u00e9n\u00e9rer plan de validation \u00bb.",
    en = "Configure parameters then click \"Generate validation plan\"."
  ),
  validation_generating = list(
    fr = "G\u00e9n\u00e9ration du plan en cours\u2026",
    en = "Generating the plan\u2026"
  ),
  validation_empty_mask_title = list(
    fr = "Zone saine",
    en = "Healthy zone"
  ),
  # --- Plan de validation FAST branché sur le trend (spec 025) ----------
  validation_source_fast_trend = list(
    fr = "FAST trend (NDRE)",
    en = "FAST trend (NDRE)"
  ),
  validation_trend_index_label = list(
    fr = "Indice",
    en = "Index"
  ),
  validation_trend_window_label = list(
    fr = "Fenêtre d'analyse",
    en = "Analysis window"
  ),
  validation_trend_n_plots_label = list(
    fr = "Placettes sanitaires",
    en = "Sanitary plots"
  ),
  validation_trend_n_control_label = list(
    fr = "Placettes témoins",
    en = "Control plots"
  ),
  validation_trend_advanced_label = list(
    fr = "Paramètres avancés",
    en = "Advanced parameters"
  ),
  validation_trend_params_note = list(
    fr = "Mois de saison, années min. et seuil α sont repris de l'onglet « Alertes FAST » (mode Tendance).",
    en = "Season months, min. years and α threshold are taken from the \"Alertes FAST\" tab (Trend mode)."
  ),
  validation_trend_min_obs_label = list(
    fr = "Observations min. / an",
    en = "Min. observations / year"
  ),
  validation_trend_generate_btn = list(
    fr = "Générer le plan sanitaire",
    en = "Generate sanitary plan"
  ),
  validation_trend_legend_title = list(
    fr = "Sévérité du déclin (|pente| %s/an)",
    en = "Decline severity (|slope| %s/yr)"
  ),
  validation_empty_trend_title = list(
    fr = "Aucun déclin significatif",
    en = "No significant decline"
  ),
  validation_empty_trend_body = list(
    fr = "Aucun déclin pluriannuel significatif sur la fenêtre d'analyse — rien à valider.",
    en = "No significant pluriannual decline on the analysis window — nothing to validate."
  ),
  validation_empty_mask_body = list(
    fr = "Aucune cellule d'alerte dans les classes retenues sur la fen\u00eatre courante \u2014 rien \u00e0 valider.",
    en = "No alert cell in the retained classes on the current window \u2014 nothing to validate."
  ),
  validation_no_mask_title = list(
    fr = "Mask d'alerte indisponible",
    en = "Alert mask unavailable"
  ),
  validation_no_mask_body = list(
    fr = "Aucun mask FORDEAD/FAST n'a \u00e9t\u00e9 produit pour cette zone. Lance d'abord le diagnostic.",
    en = "No FORDEAD/FAST mask has been produced for this zone. Run the diagnostic first."
  ),
  validation_persisted_toast = list(
    fr = "%d placettes enregistr\u00e9es dans samples.gpkg.",
    en = "%d plots saved to samples.gpkg."
  ),
  validation_qgis_exported_toast = list(
    fr = "Projet QGIS export\u00e9 : %s",
    en = "QGIS project exported: %s"
  ),
  validation_legend_validation = list(
    fr = "Validation",
    en = "Validation"
  ),
  validation_legend_temoin = list(
    fr = "T\u00e9moin",
    en = "Control"
  ),

  # Per-phase labels \u2014 current 1.x sequence (nemeton@v0.22.5)
  # Phase 0 ingest (nemeton@v0.24.0): downloads missing S2 bands
  # (B02/B05/B8A/B11) reusing FAST's cached B04/B12.
  monitoring_fordead_phase_ingest = list(
    fr = "T\u00e9l\u00e9chargement des bandes manquantes\u2026",
    en = "Downloading missing bands\u2026"
  ),
  monitoring_fordead_phase_vegetation_index = list(
    fr = "Indice de v\u00e9g\u00e9tation",
    en = "Vegetation index"
  ),
  monitoring_fordead_phase_train_model = list(
    fr = "Entra\u00eenement du mod\u00e8le",
    en = "Model training"
  ),
  monitoring_fordead_phase_forest_mask = list(
    fr = "Masque forestier",
    en = "Forest mask"
  ),
  monitoring_fordead_phase_dieback_detection = list(
    fr = "D\u00e9tection de d\u00e9p\u00e9rissement",
    en = "Dieback detection"
  ),
  monitoring_fordead_phase_export_results = list(
    fr = "Export des r\u00e9sultats",
    en = "Results export"
  ),
  monitoring_fordead_phase_postprocess = list(
    fr = "Post-traitement",
    en = "Post-processing"
  ),
  monitoring_fordead_phase_persist = list(
    fr = "Enregistrement DB",
    en = "Database persistence"
  ),

  # Anticipated for nemeton@v0.23.0 (4-5 phase STAC pipeline). Pre-shipped
  # so a future c\u0153ur bump consumes these labels without an app release.
  monitoring_fordead_phase_stac_assembly = list(
    fr = "Assemblage STAC",
    en = "STAC assembly"
  ),
  monitoring_fordead_phase_fit = list(
    fr = "Entra\u00eenement (fit)",
    en = "Training (fit)"
  ),
  monitoring_fordead_phase_predict = list(
    fr = "D\u00e9tection (predict)",
    en = "Detection (predict)"
  ),

  # ----- G3 banners (geographic + species validity) -----
  monitoring_warning_geo_title = list(
    fr = "Zone hors domaine de validation FORDEAD",
    en = "Area outside FORDEAD validation domain"
  ),
  monitoring_warning_geo_body = list(
    fr = "La zone d'\u00e9tude n'intersecte les d\u00e9partements valid\u00e9s (88, 39, 01, 73, 74) qu'\u00e0 %.0f %%. Calibration ONF/DSF 2024 non garantie.",
    en = "The study area only overlaps validated departments (88, 39, 01, 73, 74) by %.0f%%. ONF/DSF 2024 calibration not guaranteed."
  ),
  monitoring_warning_species_title = list(
    fr = "Composition d'essences hors domaine valid\u00e9",
    en = "Species composition outside validated domain"
  ),
  monitoring_warning_species_body = list(
    fr = "Seulement %.0f %% d'\u00e9pic\u00e9a + sapin pectin\u00e9 selon BD For\u00eat v2. FORDEAD n'est calibr\u00e9 que sur ces deux essences.",
    en = "Only %.0f%% of Norway spruce + silver fir according to BD For\u00eat v2. FORDEAD is only calibrated on these two species."
  ),
  monitoring_warning_low_classes = list(
    fr = "Inclure les classes faible/moyenne entra\u00eene jusqu'\u00e0 50 % de faux positifs (rapport ONF/DSF 2024).",
    en = "Including low/medium classes yields up to 50% false positives (ONF/DSF 2024 report)."
  ),
  monitoring_confirm_invalid_title = list(
    fr = "Lancer FORDEAD hors zone de validit\u00e9 ?",
    en = "Run FORDEAD outside validity domain?"
  ),
  monitoring_confirm_invalid_body = list(
    fr = "La zone et/ou les essences ne respectent pas les seuils ONF/DSF. Les r\u00e9sultats sont \u00e0 interpr\u00e9ter avec circonspection. Continuer ?",
    en = "The area and/or species do not meet ONF/DSF thresholds. Results should be interpreted with caution. Continue?"
  ),
  monitoring_confirm_yes = list(fr = "Lancer quand m\u00eame", en = "Run anyway"),
  monitoring_confirm_no  = list(fr = "Annuler",            en = "Cancel"),

  # ----- Alert classes (leaflet legend + popups) -----
  monitoring_class_1 = list(fr = "1 - faible",   en = "1 - low"),
  monitoring_class_2 = list(fr = "2 - moyenne",  en = "2 - medium"),
  monitoring_class_3 = list(fr = "3 - forte",    en = "3 - strong"),
  monitoring_class_4 = list(fr = "4 - sol nu",   en = "4 - bare soil"),
  monitoring_include_low = list(
    fr = "Inclure les classes faible et moyenne",
    en = "Include low and medium classes"
  ),
  monitoring_alert_popup_class       = list(fr = "Classe",        en = "Class"),
  monitoring_alert_popup_stress      = list(fr = "Stress index",  en = "Stress index"),
  monitoring_alert_popup_date        = list(fr = "Date",          en = "Date"),
  monitoring_alert_popup_status      = list(fr = "Statut",        en = "Status"),
  monitoring_alert_popup_disturbance = list(fr = "Type",          en = "Type"),
  monitoring_alert_validate          = list(fr = "Valider",       en = "Confirm"),
  monitoring_alert_false_positive    = list(fr = "Faux positif",  en = "False positive"),

  # ----- QGIS health validation workflow -----
  # v0.88.2 \u2014 cl\u00e9s du g\u00e9n\u00e9rateur QGIS legacy (monitoring_qgis_*) retir\u00e9es
  # avec le panneau Alertes FORDEAD correspondant (doublon de \u00ab Plan de
  # validation FORDEAD \u00bb).

  # ----- Health validation tab (mod_field_ingest) -----
  health_validation_tab_title = list(
    fr = "Validation sanitaire",
    en = "Health validation"
  ),
  health_validation_subtitle = list(
    fr = "Importer un GPKG de placettes saisies sur le terrain pour mettre \u00e0 jour le statut des alertes FORDEAD.",
    en = "Import a GPKG of field-collected plots to update FORDEAD alert statuses."
  ),
  health_validation_zone_label = list(
    fr = "Zone de suivi cible",
    en = "Target monitoring zone"
  ),
  health_validation_snap_label = list(
    fr = "Distance de rattachement (m)",
    en = "Snap distance (m)"
  ),
  health_validation_run_btn = list(
    fr = "Ing\u00e9rer la validation",
    en = "Ingest validation"
  ),
  health_validation_report_title = list(
    fr = "Rapport d'ingestion",
    en = "Ingestion report"
  ),
  health_validation_report_summary = list(
    fr = "%d alertes mises \u00e0 jour : %d confirm\u00e9es, %d faux positifs, %d sans correspondance.",
    en = "%d alerts updated: %d confirmed, %d false positives, %d unmatched."
  ),
  health_validation_no_file = list(
    fr = "S\u00e9lectionnez d'abord un GeoPackage.",
    en = "Select a GeoPackage first."
  ),
  health_validation_error = list(
    fr = "Erreur lors de l'ingestion : %s",
    en = "Ingestion error: %s"
  ),

  # ----- R5 (radar labels, future) -----
  r5_label   = list(fr = "D\u00e9p\u00e9rissement (R5)", en = "Dieback (R5)"),
  r5_tooltip = list(
    fr = "Score 0-100 calcul\u00e9 \u00e0 partir des classes FORDEAD pond\u00e9r\u00e9es par leur fiabilit\u00e9 ONF/DSF (rapport 2024).",
    en = "0-100 score from FORDEAD classes weighted by their ONF/DSF reliability (2024 report)."
  ),

  # ============================================================
  # RAG / Knowledge corpus admin (spec 009.2 \u2014 onglet du modal Param\u00e8tres)
  # ============================================================
  rag_tab_title = list(
    fr = "RAG / Corpus de connaissances",
    en = "RAG / Knowledge corpus"
  ),
  rag_manifest_section = list(
    fr = "Manifeste du corpus",
    en = "Corpus manifest"
  ),
  rag_validation_section = list(
    fr = "Validation du manifeste",
    en = "Manifest validation"
  ),
  rag_report_section = list(
    fr = "Bilan de l'import",
    en = "Import report"
  ),
  rag_inventory_section = list(
    fr = "Inventaire en base",
    en = "Database inventory"
  ),
  rag_btn_save = list(fr = "Enregistrer", en = "Save"),
  rag_btn_preview = list(fr = "Pr\u00e9visualiser le plan", en = "Preview plan"),
  rag_btn_import = list(fr = "Importer", en = "Import"),
  rag_btn_add_row = list(fr = "Ajouter une ligne", en = "Add row"),
  rag_btn_delete_row = list(fr = "Supprimer la ligne", en = "Delete row"),
  rag_btn_delete = list(fr = "Supprimer le document", en = "Delete document"),
  rag_btn_refresh = list(fr = "Rafra\u00eechir", en = "Refresh"),
  rag_btn_import_csv = list(fr = "Importer un CSV", en = "Import CSV"),
  rag_btn_export_csv = list(fr = "Exporter (CSV)", en = "Export (CSV)"),
  rag_reset_corpus = list(
    fr = "R\u00e9initialiser depuis le corpus du package",
    en = "Reset from package corpus"
  ),
  rag_reset_corpus_title = list(
    fr = "R\u00e9initialiser le corpus ?",
    en = "Reset the corpus?"
  ),
  rag_reset_corpus_warn = list(
    fr = "\u00c9crase votre copie \u00e9ditable par le corpus livr\u00e9 avec le package. Vos modifications non export\u00e9es seront perdues.",
    en = "Overwrites your editable copy with the corpus shipped in the package. Your unexported changes will be lost."
  ),
  rag_reset_corpus_done = list(
    fr = "Corpus r\u00e9initialis\u00e9 depuis le package.",
    en = "Corpus reset from the package."
  ),
  rag_import_csv_placeholder = list(fr = "Aucun fichier", en = "No file"),
  rag_import_csv_ok = list(
    fr = "Manifeste import\u00e9 (%d documents). V\u00e9rifiez puis Enregistrer.",
    en = "Manifest imported (%d documents). Review then Save."
  ),
  rag_import_csv_error = list(
    fr = "Import impossible : %s",
    en = "Import failed: %s"
  ),
  rag_opt_include_tbc = list(
    fr = "Inclure les documents \u00e0 confirmer (avertissement licence D5)",
    en = "Include to-confirm documents (D5 licence warning)"
  ),
  rag_opt_fresh = list(
    fr = "Reconstruire le corpus (fresh)",
    en = "Rebuild corpus (fresh)"
  ),
  rag_fresh_confirm_title = list(
    fr = "Reconstruction compl\u00e8te",
    en = "Full rebuild"
  ),
  rag_fresh_confirm = list(
    fr = "La reconstruction supprime les embeddings existants avant de r\u00e9-ing\u00e9rer le corpus. Confirmer ?",
    en = "Rebuilding deletes existing embeddings before re-ingesting the corpus. Confirm?"
  ),
  rag_fresh_confirm_yes = list(fr = "Reconstruire", en = "Rebuild"),
  rag_fresh_confirm_cancel = list(fr = "Annuler", en = "Cancel"),
  rag_progress = list(fr = "Import en cours", en = "Import running"),
  rag_progress_idle = list(
    fr = "Aucun import en cours.",
    en = "No import running."
  ),
  rag_no_api_key = list(
    fr = "Cl\u00e9 d'embedding Mistral absente (NEMETON_MISTRAL_API_KEY). L'import est d\u00e9sactiv\u00e9.",
    en = "Mistral embedding key missing (NEMETON_MISTRAL_API_KEY). Import is disabled."
  ),
  rag_api_key_console = list(
    fr = "Obtenir une cl\u00e9 sur la console Mistral",
    en = "Get a key from the Mistral console"
  ),
  rag_no_db = list(
    fr = "Aucune base de connaissances configur\u00e9e (NEMETON_KNOWLEDGE_DB_URL ou NEMETON_DB_URL).",
    en = "No knowledge database configured (NEMETON_KNOWLEDGE_DB_URL or NEMETON_DB_URL)."
  ),
  rag_no_manifest = list(
    fr = "Manifeste inscriptible indisponible.",
    en = "Writable manifest unavailable."
  ),
  rag_saved_ok = list(fr = "Manifeste enregistr\u00e9.", en = "Manifest saved."),
  rag_save_blocked = list(
    fr = "Enregistrement refus\u00e9 :",
    en = "Save refused:"
  ),
  rag_preview_done = list(
    fr = "Plan pr\u00e9visualis\u00e9 (dry-run).",
    en = "Plan previewed (dry-run)."
  ),
  rag_import_done = list(fr = "Import termin\u00e9.", en = "Import done."),
  rag_import_error = list(fr = "Erreur :", en = "Error:"),
  rag_delete_done = list(fr = "Document supprim\u00e9.", en = "Document deleted."),
  rag_delete_select_first = list(
    fr = "S\u00e9lectionnez d'abord une ligne.",
    en = "Select a row first."
  ),
  rag_enum_invalid = list(
    fr = "%s : valeur invalide. Valeurs admises : %s",
    en = "%s: invalid value. Allowed values: %s"
  ),
  rag_unauthorized = list(
    fr = "Acc\u00e8s r\u00e9serv\u00e9 aux administrateurs.",
    en = "Administrator access only."
  ),
  rag_inventory_empty = list(
    fr = "Aucun document en base.",
    en = "No document in database."
  ),
  # Column / field labels (manifest + report + issues)
  rag_col_doc_id = list(fr = "Identifiant", en = "ID"),
  rag_col_title = list(fr = "Titre", en = "Title"),
  rag_col_author = list(fr = "Auteur", en = "Author"),
  rag_col_publisher = list(fr = "\u00c9diteur", en = "Publisher"),
  rag_col_pub_date = list(fr = "Date", en = "Date"),
  rag_col_lang = list(fr = "Langue", en = "Language"),
  rag_col_doc_type = list(fr = "Type", en = "Type"),
  rag_col_source_url = list(fr = "URL source", en = "Source URL"),
  rag_col_license = list(fr = "Licence", en = "Licence"),
  rag_col_license_commercial_ok = list(
    fr = "Usage commercial",
    en = "Commercial use"
  ),
  rag_col_family_codes = list(fr = "Familles", en = "Families"),
  rag_col_profile_codes = list(fr = "Profils", en = "Profiles"),
  rag_col_ingest_strategy = list(fr = "Strat\u00e9gie", en = "Strategy"),
  rag_col_local_path = list(fr = "Chemin local", en = "Local path"),
  rag_col_status = list(fr = "Statut", en = "Status"),
  rag_col_notes = list(fr = "Notes", en = "Notes"),
  rag_col_row = list(fr = "Ligne", en = "Row"),
  rag_col_severity = list(fr = "S\u00e9v\u00e9rit\u00e9", en = "Severity"),
  rag_col_field = list(fr = "Champ", en = "Field"),
  rag_col_message = list(fr = "Message", en = "Message"),
  rag_col_action = list(fr = "Action", en = "Action"),
  rag_col_reason = list(fr = "Raison", en = "Reason"),
  rag_col_mode = list(fr = "Mode", en = "Mode"),
  rag_col_n_chunks = list(fr = "Chunks", en = "Chunks"),
  rag_col_document_id = list(fr = "ID document", en = "Document ID"),
  rag_col_duration_sec = list(fr = "Dur\u00e9e (s)", en = "Duration (s)"),

  # ============================================================
  # reG\u00e9n\u00e9ration \u2014 vuln\u00e9rabilit\u00e9 climatique (spec 027, L4)
  # ============================================================
  # --- Verrou de projet (serveur multi-utilisateurs) ---
  lock_readonly_notice = list(
    fr = "Projet ouvert en \u00e9dition par %s \u2014 vous \u00eates en lecture seule.",
    en = "Project open for editing by %s \u2014 you are read-only."
  ),
  lock_readonly_banner = list(
    fr = "Projet ouvert en \u00e9dition par %s \u2014 lecture seule.",
    en = "Project open for editing by %s \u2014 read-only."
  ),
  lock_anonymous_banner = list(
    fr = "Connectez-vous pour \u00e9diter ce projet \u2014 lecture seule.",
    en = "Sign in to edit this project \u2014 read-only."
  ),
  lock_role_banner = list(
    fr = "Votre r\u00f4le est en lecture seule pour ce projet.",
    en = "Your role is read-only for this project."
  ),
  lock_stolen_notice = list(
    fr = "Le verrou pr\u00e9c\u00e9dent avait expir\u00e9 ; vous \u00e9ditez ce projet.",
    en = "The previous lock had expired; you are now editing this project."
  ),
  lock_lost_notice = list(
    fr = "Verrou perdu \u2014 passage en lecture seule.",
    en = "Lock lost \u2014 switching to read-only."
  ),
  lock_readonly_action = list(
    fr = "Action indisponible : projet en lecture seule.",
    en = "Action unavailable: project is read-only."
  ),
  regen_tab_title = list(fr = "reG\u00e9n\u00e9ration", en = "reGeneration"),
  regen_intro = list(
    fr = "Lecture de vuln\u00e9rabilit\u00e9 climatique (exposition microclimatique \u00d7 stress hydrique du sol) pour prioriser les interventions de r\u00e9g\u00e9n\u00e9ration.",
    en = "Climate-vulnerability read (microclimatic exposure \u00d7 soil water stress) to prioritise regeneration interventions."
  ),
  regen_config_section = list(fr = "Configuration", en = "Configuration"),
  regen_years_section = list(fr = "Ann\u00e9es de r\u00e9f\u00e9rence", en = "Reference years"),
  regen_year_moyenne = list(fr = "Ann\u00e9e moyenne", en = "Average year"),
  regen_year_canicule = list(fr = "Ann\u00e9e caniculaire", en = "Heatwave year"),
  regen_year_auto = list(fr = "Auto (E-OBS)", en = "Auto (E-OBS)"),
  regen_eobs_index = list(fr = "Indice E-OBS", en = "E-OBS index"),
  regen_stand_section = list(fr = "Peuplement", en = "Stand"),
  regen_forest_type = list(fr = "Type de peuplement", en = "Stand type"),
  regen_forest_type_tip = list(
    fr = "Phénologie du peuplement dans le bilan hydrique du sol (BILJOU). Un feuillu caduc (interception/transpiration saisonnières) et un résineux sempervirent (toute l'année) donnent des jours de stress hydrique différents, donc une sensibilité et une priorité différentes. Agit en amont : nécessite de relancer l'analyse (contrairement à l'essence cible, live).",
    en = "Stand phenology in the soil water balance (BILJOU). A deciduous broadleaf (seasonal interception/transpiration) and an evergreen conifer (year-round) yield different water-stress days, hence different sensitivity and priority. Applied upstream: requires re-running the analysis (unlike the target species, which is live)."),
  regen_forest_feuillu = list(fr = "Feuillu", en = "Broadleaf"),
  regen_forest_resineux = list(fr = "R\u00e9sineux", en = "Conifer"),
  regen_budburst = list(fr = "D\u00e9bourrement (jour julien)", en = "Budburst (day of year)"),
  regen_leaf_fall = list(fr = "Chute des feuilles (jour julien)", en = "Leaf fall (day of year)"),
  regen_lai_max = list(fr = "LAI max", en = "Max LAI"),
  regen_ewm = list(fr = "Eau extractible (mm)", en = "Extractable water (mm)"),
  regen_ewm_hint = list(
    fr = "Laisser vide pour dériver la réserve utile par UGF depuis SoilGrids (250 m). Saisir une valeur force un sol uniforme sur tout le massif.",
    en = "Leave empty to derive per-unit extractable water from SoilGrids (250 m). Entering a value forces a uniform soil across the whole area."
  ),
  regen_rooting_depth = list(fr = "Profondeur d'enracinement (cm)", en = "Rooting depth (cm)"),
  regen_rooting_depth_hint = list(
    fr = "Profondeur sur laquelle SoilGrids intègre la réserve utile. Sans effet si l'eau extractible est saisie à la main.",
    en = "Depth over which SoilGrids integrates extractable water. No effect when extractable water is entered manually."
  ),
  regen_canopy_pai_lidar = list(
    fr = "LAI max du bilan hydrique dérivé du PAI LiDAR",
    en = "Water-balance max LAI derived from the LiDAR PAI"
  ),
  regen_roots = list(fr = "Fractions racinaires", en = "Root fractions"),
  regen_forcing = list(fr = "For\u00e7age m\u00e9t\u00e9o", en = "Weather forcing"),
  regen_forcing_safran = list(fr = "SAFRAN (d\u00e9faut)", en = "SAFRAN (default)"),
  regen_forcing_era5 = list(fr = "ERA5-Land", en = "ERA5-Land"),
  regen_resolution = list(fr = "R\u00e9solution microclimat", en = "Microclimate resolution"),
  regen_res_2m = list(fr = "2 m (d\u00e9faut)", en = "2 m (default)"),
  regen_res_5m = list(fr = "5 m", en = "5 m"),
  regen_species_target = list(fr = "Essence cible (option)", en = "Target species (optional)"),
  regen_species_generic = list(fr = "G\u00e9n\u00e9rique (d\u00e9faut)", en = "Generic (default)"),
  regen_buffer = list(fr = "Buffer contexte r\u00e9gional (km)", en = "Regional context buffer (km)"),
  regen_run = list(fr = "Lancer l'analyse", en = "Run analysis"),
  regen_run_hydric_only = list(fr = "Bilan hydrique seul (rapide)", en = "Water balance only (fast)"),
  regen_running = list(fr = "Analyse reG\u00e9n\u00e9ration en cours\u2026", en = "reGeneration analysis running\u2026"),
  regen_restore_loading = list(fr = "Chargement des cartes reG\u00e9n\u00e9ration\u2026", en = "Loading reGeneration maps\u2026"),
  regen_context_opacity = list(fr = "Opacit\u00e9 E-OBS", en = "E-OBS opacity"),
  # Moteur \u00ab risque de gel tardif \u00bb (R7) \u2014 meteoland Tmin -> indicateur_r7_gel.
  regen_frost_section = list(fr = "Risque de gel tardif (R7)", en = "Late-frost risk (R7)"),
  regen_frost_run = list(fr = "Risque de gel (meteoland)", en = "Frost risk (meteoland)"),
  regen_frost_tip = list(
    fr = "Interpole la temp\u00e9rature minimale journali\u00e8re (SAFRAN \u2192 MNT, via meteoland) sur la fen\u00eatre de d\u00e9bourrement, puis compte les gel\u00e9es tardives (indicateur R7). Contexte r\u00e9gional NDP 1 \u2014 gradient topographique, PAS l'effet tampon sous couvert (microclimf). Opt-in, co\u00fbteux, r\u00e9sultat cach\u00e9.",
    en = "Interpolates daily minimum temperature (SAFRAN \u2192 DEM, via meteoland) over the budburst window, then counts late frosts (indicator R7). Regional NDP-1 context \u2014 topographic gradient, NOT the below-canopy buffering (microclimf). Opt-in, costly, result cached."),
  regen_frost_running_short = list(fr = "Gel\u2026", en = "Frost\u2026"),
  regen_frost_running = list(fr = "Interpolation gel (meteoland)\u2026", en = "Frost interpolation (meteoland)\u2026"),
  regen_frost_done = list(fr = "Risque de gel calcul\u00e9 (R7).", en = "Frost risk computed (R7)."),
  regen_frost_skipped = list(
    fr = "Risque de gel non calcul\u00e9 : Tmin indisponible (meteoland/SAFRAN). R7 absent du radar.",
    en = "Frost risk not computed: Tmin unavailable (meteoland/SAFRAN). R7 absent from the radar."),
  regen_frost_unavailable = list(
    fr = "meteoland n'est pas install\u00e9 : le risque de gel (R7) est indisponible.",
    en = "meteoland is not installed: frost risk (R7) is unavailable."),
  regen_map_gel = list(fr = "Gel\u00e9es tardives (R7)", en = "Late frosts (R7)"),
  regen_map_gel_info = list(
    fr = "Nombre moyen de gel\u00e9es tardives par an apr\u00e8s d\u00e9bourrement (indicateur R7). Rouge = beaucoup de gel\u00e9es = critique.",
    en = "Mean number of late frosts per year after budburst (indicator R7). Red = many frosts = critical."),
  regen_run_done = list(fr = "Analyse reG\u00e9n\u00e9ration termin\u00e9e", en = "reGeneration analysis complete"),
  regen_run_done_warn = list(fr = "Analyse termin\u00e9e avec %d avertissement(s) \u2014 voir le bandeau.", en = "Analysis complete with %d warning(s) \u2014 see the banner."),
  regen_guard_sensibilite = list(fr = "Sensibilit\u00e9 microclimatique non calcul\u00e9e : fournissez une sortie microclimf via le cache du projet (precomputed), ou activez le moteur (entr\u00e9es LiDAR).", en = "Microclimatic sensitivity not computed: provide a microclimf output via the project cache (precomputed), or enable the engine (LiDAR inputs)."),
  regen_guard_hydrique = list(fr = "Bilan hydrique non calcul\u00e9 : fournissez une sortie BILJOU via le cache du projet (precomputed), ou activez le moteur (entr\u00e9es m\u00e9t\u00e9o/sol).", en = "Water balance not computed: provide a BILJOU output via the project cache (precomputed), or enable the engine (weather/soil inputs)."),
  regen_engine_section = list(fr = "Moteur microclimf r\u00e9el", en = "Real microclimf engine"),
  regen_engine_run = list(fr = "Lancer le moteur r\u00e9el", en = "Run the real engine"),
  regen_engine_tip = list(fr = "Lance le vrai mod\u00e8le microclimf (structure LiDAR HD + for\u00e7age ERA5-Land) pour calculer la sensibilit\u00e9 microclimatique. Co\u00fbteux (minutes \u00e0 heures) : s'ex\u00e9cute en arri\u00e8re-plan sans bloquer l'application.", en = "Runs the real microclimf model (LiDAR HD structure + ERA5-Land forcing) to compute microclimatic sensitivity. Expensive (minutes to hours): runs in the background without blocking the app."),
  regen_engine_running = list(fr = "Moteur microclimf en cours (LiDAR HD + ERA5) \u2014 cela peut prendre plusieurs minutes\u2026", en = "microclimf engine running (LiDAR HD + ERA5) \u2014 this can take several minutes\u2026"),
  regen_engine_running_short = list(fr = "Moteur en cours\u2026", en = "Engine running\u2026"),
  regen_auto_running_short = list(fr = "D\u00e9tection E-OBS\u2026", en = "E-OBS detection\u2026"),
  regen_engine_done = list(fr = "Moteur microclimf termin\u00e9 \u2014 sensibilit\u00e9 mise \u00e0 jour et mise en cache.", en = "microclimf engine finished \u2014 sensitivity updated and cached."),
  regen_engine_ready = list(fr = "Pr\u00e9requis r\u00e9unis (grille LiDAR HD + identifiants CDS).", en = "Prerequisites met (LiDAR HD grid + CDS credentials)."),
  regen_engine_prereq_core = list(fr = "Moteur indisponible : nemeton::regen_sensibilite absent (mettez \u00e0 jour le c\u0153ur).", en = "Engine unavailable: nemeton::regen_sensibilite missing (update the core)."),
  regen_engine_prereq_lidar = list(fr = "Grille LiDAR HD absente : calculez d'abord les indices avec la source CHM \u00ab LiDAR HD \u00bb pour ce projet.", en = "LiDAR HD grid missing: first compute the indices with the \u201cLiDAR HD\u201d CHM source for this project."),
  regen_engine_prereq_cds = list(fr = "Identifiants Copernicus CDS non configur\u00e9s (ecmwfr::wf_set_key) \u2014 requis pour le for\u00e7age ERA5.", en = "Copernicus CDS credentials not configured (ecmwfr::wf_set_key) \u2014 required for the ERA5 forcing."),
  regen_engine_no_vegetation_structure = list(fr = "Sensibilit\u00e9 microclimatique : aucune structure de v\u00e9g\u00e9tation disponible (ni nuage LiDAR, ni LAI Sentinel-2).", en = "Microclimatic sensitivity: no vegetation structure available (neither LiDAR point cloud nor Sentinel-2 LAI)."),
  regen_engine_era5_interrupted = list(fr = "Acquisition ERA5 interrompue (throttle CDS) \u2014 relancez \u00ab Lancer le moteur r\u00e9el \u00bb pour reprendre depuis le cache.", en = "ERA5 acquisition interrupted (CDS throttling) \u2014 click \u201cRun the real engine\u201d again to resume from cache."),
  # Notifications ntfy du moteur reG\u00e9n\u00e9ration (jalons + granularit\u00e9 fine).
  regen_ntfy_start = list(fr = "Moteur reG\u00e9n\u00e9ration lanc\u00e9 (microclimf + BILJOU)\u2026", en = "Regeneration engine started (microclimf + BILJOU)\u2026"),
  regen_ntfy_micro_start = list(fr = "microclimf : acquisition ERA5 + exposition en cours\u2026", en = "microclimf: ERA5 acquisition + exposure running\u2026"),
  regen_ntfy_micro_done = list(fr = "microclimf : sensibilit\u00e9 microclimatique calcul\u00e9e \u2713", en = "microclimf: microclimatic sensitivity computed \u2713"),
  regen_ntfy_micro_skip = list(fr = "microclimf : non calcul\u00e9 (structure de v\u00e9g\u00e9tation ou CDS manquants)", en = "microclimf: not computed (missing vegetation structure or CDS)"),
  regen_ntfy_biljou_start = list(fr = "BILJOU : bilan hydrique en cours\u2026", en = "BILJOU: water balance running\u2026"),
  regen_ntfy_biljou_done = list(fr = "BILJOU : bilan hydrique calcul\u00e9 \u2713", en = "BILJOU: water balance computed \u2713"),
  regen_ntfy_done = list(fr = "Moteur reG\u00e9n\u00e9ration termin\u00e9 : %s", en = "Regeneration engine finished: %s"),
  regen_ntfy_done_empty = list(fr = "Moteur reG\u00e9n\u00e9ration termin\u00e9 : aucune sortie produite", en = "Regeneration engine finished: no output produced"),
  regen_ntfy_era5 = list(fr = "ERA5 %d (%s) [%d/%d]\u2026", en = "ERA5 %d (%s) [%d/%d]\u2026"),
  regen_ntfy_micro_cat = list(fr = "microclimf : \u00e9t\u00e9 %s\u2026", en = "microclimf: %s summer\u2026"),
  regen_ntfy_biljou_pts = list(fr = "BILJOU : %d points\u2026", en = "BILJOU: %d points\u2026"),
  # --- Phase en cours du moteur reG\u00e9n\u00e9ration (notif bas-droite, spec 027 / brief engine-phase-status) ---
  regen_phase_grille = list(fr = "Pr\u00e9paration grille LiDAR HD\u2026", en = "Preparing LiDAR-HD grid\u2026"),
  regen_phase_pai = list(fr = "Structure de v\u00e9g\u00e9tation (PAI %s)\u2026", en = "Vegetation structure (PAI %s)\u2026"),
  regen_phase_pai_lidar = list(fr = "LiDAR", en = "LiDAR"),
  regen_phase_pai_raster = list(fr = "satellite", en = "satellite"),
  regen_phase_pai_cache = list(fr = "cache", en = "cache"),
  regen_pai_recompute = list(fr = "Recalculer le PAI", en = "Recompute PAI"),
  regen_pai_recompute_tip = list(fr = "Supprime le cache PAI (cache/regeneration/pai.tif). Utile si le nuage LiDAR a été remplacé à emprise constante : la structure de végétation sera recalculée au prochain run.", en = "Deletes the PAI cache (cache/regeneration/pai.tif). Useful when the LiDAR cloud was replaced at constant extent: vegetation structure will be recomputed on the next run."),
  regen_pai_cache_cleared = list(fr = "Cache PAI supprimé — recalcul au prochain run.", en = "PAI cache cleared — recomputed on next run."),
  regen_phase_micro_moy = list(fr = "Microclimat \u2014 \u00e9t\u00e9s moyens", en = "Microclimate \u2014 average summers"),
  regen_phase_micro_can = list(fr = "Microclimat \u2014 \u00e9t\u00e9s canicule", en = "Microclimate \u2014 heatwave summers"),
  regen_phase_exposition = list(fr = "Agr\u00e9gation de l'exposition\u2026", en = "Aggregating exposure\u2026"),
  regen_phase_ewm = list(fr = "R\u00e9serve utile (SoilGrids)", en = "Extractable water (SoilGrids)"),
  regen_engine_log = list(fr = "Journal du moteur", en = "Engine log"),
  regen_expert_section = list(
    fr = "Paramètres experts (dérivés automatiquement si vides)",
    en = "Expert parameters (auto-derived when empty)"
  ),
  regen_expert_hint = list(
    fr = "Laisser vide : la valeur est dérivée de la donnée (PAI LiDAR, SoilGrids). Renseigner un champ force la valeur sur toutes les UGF et court-circuite la dérivation.",
    en = "Leave empty: the value is derived from the data (LiDAR PAI, SoilGrids). Filling a field forces the value on every unit and bypasses the derivation."
  ),
  regen_override_badge = list(fr = "forcé", en = "forced"),
  regen_lai_derived_stats = list(
    fr = "LAI dérivé : %s [%s – %s] sur %d UGF",
    en = "Derived LAI: %s [%s – %s] across %d units"
  ),
  regen_ewm_derived_stats = list(
    fr = "Réserve utile dérivée (SoilGrids) : %s [%s – %s] mm sur %d UGF",
    en = "Derived extractable water (SoilGrids): %s [%s – %s] mm across %d units"
  ),
  regen_ewm_fallback_uniform = list(
    fr = "SoilGrids injoignable : repli sur un sol uniforme (%s mm) — le bilan hydrique n'est plus spatialisé par le sol.",
    en = "SoilGrids unreachable: falling back to a uniform soil (%s mm) — the water balance is no longer spatialised by soil."
  ),
  regen_log_ntfy_failed = list(
    fr = "Notification ntfy non envoy\u00e9e (canal indisponible) \u2014 les diagnostics restent dans ce journal.",
    en = "ntfy notification not sent (channel unavailable) \u2014 diagnostics remain in this log."
  ),
  regen_phase_biljou = list(fr = "Bilan hydrique du sol (BILJOU)\u2026", en = "Soil water balance (BILJOU)\u2026"),
  regen_phase_micro_skip = list(fr = "Exposition microclimf ignor\u00e9e : %s", en = "microclimf exposure skipped: %s"),
  regen_phase_skip_reason_cds = list(fr = "cl\u00e9 CDS/ERA5 absente", en = "no CDS/ERA5 key"),
  regen_phase_skip_reason_structure = list(fr = "structure de v\u00e9g\u00e9tation manquante", en = "missing vegetation structure"),
  regen_guard_biljou = list(fr = "Bilan hydrique r\u00e9el indisponible : for\u00e7age m\u00e9t\u00e9o (SAFRAN/ERA5) ou donn\u00e9es sol non r\u00e9cup\u00e9r\u00e9s pour cette zone.", en = "Real water balance unavailable: weather forcing (SAFRAN/ERA5) or soil data could not be retrieved for this area."),
  regen_engine_status_micro = list(fr = "microclimf : grille LiDAR HD + identifiants CDS requis.", en = "microclimf: LiDAR HD grid + CDS credentials required."),
  regen_engine_status_biljou_era5 = list(fr = "BILJOU (ERA5) : identifiants CDS requis.", en = "BILJOU (ERA5): CDS credentials required."),
  regen_engine_ready_micro = list(fr = "microclimf pr\u00eat (LiDAR HD + CDS).", en = "microclimf ready (LiDAR HD + CDS)."),
  regen_engine_ready_biljou = list(fr = "BILJOU pr\u00eat (for\u00e7age m\u00e9t\u00e9o).", en = "BILJOU ready (weather forcing)."),
  regen_canopee_lidar = list(fr = "Canop\u00e9e : LiDAR HD", en = "Canopy: LiDAR HD"),
  regen_canopee_satellite = list(fr = "Canop\u00e9e : satellite (repli)", en = "Canopy: satellite (fallback)"),
  regen_canopee_satellite_info = list(fr = "Repli NDP 0 : LAI Sentinel-2 (inversion PROSAIL) en l'absence de LiDAR HD. Proxy d\u00e9grad\u00e9 de la structure de canop\u00e9e (LAI \u2260 PAI) ; pr\u00e9cision moindre.", en = "NDP-0 fallback: Sentinel-2 LAI (PROSAIL inversion) without LiDAR HD. Degraded proxy of canopy structure (LAI \u2260 PAI); lower accuracy."),
  regen_year_auto_tip = list(fr = "D\u00e9tecte l'ann\u00e9e moyenne et l'ann\u00e9e caniculaire repr\u00e9sentatives \u00e0 partir des donn\u00e9es climatiques E-OBS sur la zone du projet, et pr\u00e9-remplit les deux champs.", en = "Detects the representative average and heatwave years from E-OBS climate data over the project area, and fills both fields."),
  regen_auto_done = list(fr = "Ann\u00e9es E-OBS d\u00e9tect\u00e9es : moyenne %s / caniculaire %s.", en = "E-OBS years detected: average %s / heatwave %s."),
  regen_auto_none = list(fr = "D\u00e9tection E-OBS indisponible \u2014 saisir les ann\u00e9es manuellement.", en = "E-OBS detection unavailable \u2014 enter the years manually."),
  regen_auto_running = list(fr = "D\u00e9tection E-OBS en cours (t\u00e9l\u00e9chargement CDS) \u2014 cela peut prendre un moment\u2026", en = "E-OBS detection running (CDS download) \u2014 this can take a while\u2026"),
  regen_lai_tip = list(fr = "Laisser vide : le LAI max est estim\u00e9 automatiquement depuis le PAI du nuage LiDAR HD. Renseigner une valeur pour forcer.", en = "Leave empty: max LAI is auto-estimated from the HD LiDAR point-cloud PAI. Set a value to override."),
  regen_species_tip = list(fr = "Essences param\u00e9trables par le mod\u00e8le (table du c\u0153ur). Les essences pr\u00e9sentes sur la zone (BD For\u00eat v2) sont list\u00e9es en t\u00eate ; les autres essences d'adaptation suivent.", en = "Species the model can parametrise (core table). Species present on the area (BD For\u00eat v2) are listed first; other adaptation species follow."),
  regen_species_group_present = list(fr = "Pr\u00e9sentes sur la zone", en = "Present on the area"),
  regen_species_group_adaptation = list(fr = "Essences d'adaptation", en = "Adaptation species"),
  regen_report_section = list(fr = "reG\u00e9n\u00e9ration \u2014 vuln\u00e9rabilit\u00e9 climatique", en = "reGeneration \u2014 climate vulnerability"),
  regen_report_intro = list(fr = "Unit\u00e9s de gestion les plus sensibles au stress climatique (exposition microclimatique \u00d7 stress hydrique), class\u00e9es par rang de sensibilit\u00e9. Indices mod\u00e9lis\u00e9s : prudents en valeur absolue, fiables en classement relatif.", en = "Management units most sensitive to climate stress (microclimatic exposure by water stress), ranked by sensitivity. Modelled indices: cautious in absolute value, reliable in relative ranking."),
  regen_resume_cache = list(fr = "Reprise depuis le cache", en = "Resumed from cache"),
  regen_need_project = list(
    fr = "Chargez un projet avec des UGF pour lancer l'analyse reG\u00e9n\u00e9ration.",
    en = "Load a project with UGF units to run the reGeneration analysis."
  ),
  regen_engine_missing = list(
    fr = "Moteur ou donn\u00e9e manquant : %s. Fournissez des sorties pr\u00e9calcul\u00e9es ou installez le moteur.",
    en = "Missing engine or data: %s. Provide precomputed outputs or install the engine."
  ),
  regen_results_section = list(fr = "R\u00e9sultats", en = "Results"),
  regen_tab_map = list(fr = "Carte", en = "Map"),
  regen_select_ug = list(fr = "S\u00e9lectionnez une UG dans le tableau.", en = "Select a unit in the table."),
  regen_export_gpkg = list(fr = "Exporter (GPKG)", en = "Export (GPKG)"),
  regen_persist_db = list(fr = "Enregistrer l'\u00e9tat (base)", en = "Save state (database)"),
  regen_persisted = list(fr = "\u00c9tat reG\u00e9n\u00e9ration enregistr\u00e9 (version %d).", en = "reGeneration state saved (version %d)."),
  regen_export_empty = list(fr = "Aucun r\u00e9sultat \u00e0 exporter.", en = "No result to export."),
  regen_db_unavailable = list(fr = "Base de donn\u00e9es non configur\u00e9e.", en = "Database not configured."),
  regen_map_layer = list(fr = "Couche affich\u00e9e", en = "Displayed layer"),
  regen_map_priorite = list(fr = "Indice de priorit\u00e9", en = "Priority index"),
  regen_map_sensibilite = list(fr = "Sensibilit\u00e9 microclimatique", en = "Microclimatic sensitivity"),
  regen_map_njstress = list(fr = "Jours de stress hydrique", en = "Water-stress days"),
  regen_map_dtmax = list(fr = "\u0394T\u00b0max sous couvert", en = "Sub-canopy \u0394T\u00b0max"),
  regen_map_bivariate = list(fr = "\u0394T\u00b0max \u00d7 \u0394VPD (parcellaire)", en = "\u0394T\u00b0max \u00d7 \u0394VPD (per unit)"),
  regen_map_context = list(fr = "Contexte r\u00e9gional (E-OBS)", en = "Regional context (E-OBS)"),
  regen_context_need_tx = list(
    fr = "Contexte r\u00e9gional indisponible : la s\u00e9rie E-OBS de temp\u00e9ratures maximales n'est pas t\u00e9l\u00e9charg\u00e9e. Lancez \u00ab Auto (E-OBS) \u00bb dans le panneau Ann\u00e9es de r\u00e9f\u00e9rence.",
    en = "Regional context unavailable: the E-OBS maximum-temperature series is not downloaded. Run \u201cAuto (E-OBS)\u201d in the Reference years panel."
  ),
  regen_context_need_rr = list(
    fr = "Contexte r\u00e9gional indisponible : cette carte croise temp\u00e9ratures et pr\u00e9cipitations, et la s\u00e9rie E-OBS de pr\u00e9cipitations n'est pas t\u00e9l\u00e9charg\u00e9e.",
    en = "Regional context unavailable: this map crosses temperature with precipitation, and the E-OBS precipitation series is not downloaded."
  ),
  # Statuts de d\u00e9gradation d'eobs_downscale (meta$reason) \u2014 carte contexte raster.
  eobs_downscale_no_dem = list(
    fr = "\u00c9l\u00e9vation de contexte indisponible (service IGN injoignable). R\u00e9essayez plus tard.",
    en = "Context elevation unavailable (IGN service unreachable). Try again later."
  ),
  eobs_downscale_dem_too_small = list(
    fr = "MNT de contexte indisponible pour cette zone.",
    en = "Context DEM unavailable for this area."
  ),
  eobs_downscale_too_few_cells = list(
    fr = "Zone trop petite pour un contexte r\u00e9gional E-OBS : \u00e9largissez le rayon.",
    en = "Area too small for an E-OBS regional context: widen the radius."
  ),
  eobs_downscale_rr_out_of_scope = list(
    fr = "Contexte \u00ab pr\u00e9cipitations \u00bb non disponible (version 1).",
    en = "Precipitation context not available (version 1)."
  ),
  regen_context_value_tx_trend = list(
    fr = "Tendance T\u00b0max estivale (\u00b0C/d\u00e9cennie)", en = "Summer max-T trend (\u00b0C/decade)"
  ),
  regen_context_computing = list(
    fr = "Calcul du contexte r\u00e9gional (E-OBS)\u2026", en = "Computing regional context (E-OBS)\u2026"
  ),
  regen_eobs_rr_fetch = list(
    fr = "T\u00e9l\u00e9charger les pr\u00e9cipitations (~800 Mo)",
    en = "Download precipitation (~800 MB)"
  ),
  regen_eobs_rr_running = list(
    fr = "T\u00e9l\u00e9chargement de la s\u00e9rie E-OBS de pr\u00e9cipitations (Copernicus CDS) \u2014 plusieurs minutes, en arri\u00e8re-plan.",
    en = "Downloading the E-OBS precipitation series (Copernicus CDS) \u2014 several minutes, in the background."
  ),
  regen_eobs_rr_running_short = list(fr = "T\u00e9l\u00e9chargement\u2026", en = "Downloading\u2026"),
  regen_eobs_rr_done = list(
    fr = "S\u00e9rie de pr\u00e9cipitations t\u00e9l\u00e9charg\u00e9e \u2014 carte de contexte r\u00e9gional disponible.",
    en = "Precipitation series downloaded \u2014 regional context map available."
  ),
  regen_eobs_rr_failed = list(
    fr = "T\u00e9l\u00e9chargement des pr\u00e9cipitations impossible (identifiants CDS ou r\u00e9seau).",
    en = "Precipitation download failed (CDS credentials or network)."
  ),
  regen_map_priorite_info = list(
    fr = "Indice composite 0\u2013100 croisant l'exposition microclimatique (microclimf) et le stress hydrique du sol (BILJOU). Valeur \u00e9lev\u00e9e = unit\u00e9 la plus vuln\u00e9rable, \u00e0 accompagner en priorit\u00e9 pour la r\u00e9g\u00e9n\u00e9ration.",
    en = "Composite 0\u2013100 index crossing microclimatic exposure (microclimf) with soil water stress (BILJOU). High value = most vulnerable unit, to prioritise for regeneration."
  ),
  regen_map_sensibilite_info = list(
    fr = "Somme des \u00e9carts normalis\u00e9s (z-scores) de \u0394T\u00b0max et \u0394VPD entre \u00e9t\u00e9s caniculaires et \u00e9t\u00e9s moyens. Score relatif sans unit\u00e9, centr\u00e9 sur la moyenne des unit\u00e9s du projet : positif = couvert qui tamponne mal la canicule. Sert au rang de sensibilit\u00e9.",
    en = "Sum of the normalised deviations (z-scores) of \u0394T\u00b0max and \u0394VPD between heatwave and average summers. Unitless relative score, centred on the project's unit mean: positive = canopy buffering the heatwave poorly. Drives the sensitivity rank."
  ),
  regen_map_njstress_info = list(
    fr = "Nombre moyen de jours d'\u00e9t\u00e9 o\u00f9 la r\u00e9serve en eau du sol (REW) passe sous le seuil de stress, mod\u00e8le BILJOU forc\u00e9 par SAFRAN (ou ERA5-Land). Valeur \u00e9lev\u00e9e = s\u00e9cheresse \u00e9daphique prolong\u00e9e.",
    en = "Mean number of summer days when soil relative extractable water (REW) drops below the stress threshold, BILJOU model forced by SAFRAN (or ERA5-Land). High value = prolonged soil drought."
  ),
  regen_map_dtmax_info = list(
    fr = "\u00c9cart de temp\u00e9rature maximale sous couvert (\u00e0 0,5 m du sol) entre les \u00e9t\u00e9s caniculaires et les \u00e9t\u00e9s moyens, en \u00b0C (mod\u00e8le microclimf, structure LiDAR HD). Valeur \u00e9lev\u00e9e = la canop\u00e9e prot\u00e8ge mal les semis pendant la canicule.",
    en = "Difference in maximum sub-canopy temperature (0.5 m above ground) between heatwave and average summers, in \u00b0C (microclimf model, LiDAR HD structure). High value = canopy poorly protecting seedlings during heatwaves."
  ),
  regen_map_legend_scale = list(
    fr = "L\u00e9gende (en bas \u00e0 droite de la carte) : rouge = valeur \u00e9lev\u00e9e, donc situation critique ; vert = situation favorable. Le d\u00e9grad\u00e9 est born\u00e9 aux valeurs minimale et maximale des unit\u00e9s affich\u00e9es \u2014 les couleurs sont relatives au projet, pas absolues.",
    en = "Legend (bottom-right of the map): red = high value, hence a critical situation; green = favourable. The ramp is bounded by the minimum and maximum of the displayed units \u2014 colours are relative to the project, not absolute."
  ),
  regen_table_section = list(fr = "Tableau des UGF", en = "UGF table"),
  regen_filter_coverage = list(fr = "Masquer les UG mal couvertes", en = "Hide poorly covered units"),
  regen_parcel_sheet = list(fr = "Fiche parcelle", en = "Unit sheet"),
  regen_rew_chronicle = list(fr = "Chronique REW (r\u00e9serve en eau)", en = "REW chronicle (water reserve)"),
  regen_col_priorite = list(fr = "Priorit\u00e9", en = "Priority"),
  regen_col_indice = list(fr = "Indice priorit\u00e9", en = "Priority index"),
  regen_col_sensibilite = list(fr = "Sensibilit\u00e9", en = "Sensitivity"),
  regen_col_rang = list(fr = "Rang", en = "Rank"),
  regen_col_njstress = list(fr = "Jours stress", en = "Stress days"),
  regen_col_istress = list(fr = "Intensit\u00e9 stress", en = "Stress intensity"),
  regen_col_deb_stress = list(fr = "D\u00e9but stress", en = "Stress onset"),
  regen_col_rew_min = list(fr = "REW min", en = "Min REW"),
  regen_col_dtmax = list(fr = "\u0394T\u00b0max", en = "\u0394T\u00b0max"),
  regen_col_dvpd = list(fr = "\u0394VPD", en = "\u0394VPD"),
  regen_col_couverture = list(fr = "Couverture (%)", en = "Coverage (%)"),
  regen_ndp_model = list(
    fr = "Indices mod\u00e9lis\u00e9s (mod\u00e8le m\u00e9caniste, non mesur\u00e9s terrain) : fiables en classement relatif, prudents en valeur absolue.",
    en = "Modelled indices (mechanistic model, not field-measured): reliable for relative ranking, cautious in absolute value."
  ),
  regen_biljou_warn = list(
    fr = "Bilan hydrique BILJOU : r\u00e9impl\u00e9mentation non cautionn\u00e9e par l'INRAE.",
    en = "BILJOU water balance: reimplementation not endorsed by INRAE."
  ),
  regen_coverage_warn = list(
    fr = "%d UG \u00e0 couverture faible exclues par d\u00e9faut.",
    en = "%d low-coverage units excluded by default."
  )
)


#' Get translation object
#'
#' @description
#' Returns a translation object for the specified language.
#'
#' @param language Character. Language code ("fr" or "en").
#'
#' @return A list with a $t() method for translation.
#'
#' @noRd
get_i18n <- function(language = "fr") {
  lang <- match.arg(language, c("fr", "en"))

  # Create translator object
  translator <- list(
    language = lang,

    # Translation function
    t = function(key, ...) {
      if (!key %in% names(TRANSLATIONS)) {
        cli::cli_warn("Translation key not found: {key}")
        return(key)
      }

      text <- TRANSLATIONS[[key]][[lang]]

      if (is.null(text)) {
        # Fallback to English
        text <- TRANSLATIONS[[key]][["en"]]
      }

      if (is.null(text)) {
        return(key)
      }

      # Handle string interpolation
      args <- list(...)
      if (length(args) > 0) {
        text <- glue::glue(text, .envir = as.environment(args))
      }

      as.character(text)
    },

    # Get all keys
    keys = function() {
      names(TRANSLATIONS)
    },

    # Check if key exists
    has = function(key) {
      key %in% names(TRANSLATIONS)
    }
  )

  class(translator) <- c("nemeton_i18n", "list")
  translator
}


#' Print method for i18n object
#'
#' @param x i18n object
#' @param ... Additional arguments (ignored)
#' @noRd
print.nemeton_i18n <- function(x, ...) {
  cat(sprintf("nemeton i18n translator [%s]\n", x$language))
  cat(sprintf("  %d translation keys available\n", length(x$keys())))
  invisible(x)
}


#' Get available languages
#'
#' @return Character vector of language codes
#' @noRd
get_available_languages <- function() {
  c("fr", "en")
}


#' Export translations to JSON files
#'
#' @description
#' Exports the translation dictionary to JSON files for external translators
#' or external i18n tooling. The R list `TRANSLATIONS` remains the single
#' source of truth consumed at runtime by `get_i18n()`.
#'
#' @param output_dir Directory to write JSON files.
#'
#' @return Invisible NULL. Creates fr.json and en.json files.
#'
#' @noRd
export_translations_json <- function(output_dir = "inst/app/i18n") {
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  for (lang in get_available_languages()) {
    translations <- lapply(TRANSLATIONS, function(t) t[[lang]])
    json_path <- file.path(output_dir, paste0(lang, ".json"))

    jsonlite::write_json(
      translations,
      json_path,
      auto_unbox = TRUE,
      pretty = TRUE
    )

    cli::cli_alert_success("Exported {lang}.json to {json_path}")
  }

  invisible(NULL)
}


#' Translate task message for progress display
#'
#' @description
#' Translates a task message from the computation progress callback.
#' Handles the special format "download:source_key" and "compute:indicator_key".
#'
#' @param task Character. The task identifier from progress callback.
#' @param i18n The i18n translator object.
#'
#' @return Character. The translated message.
#'
#' @noRd
translate_task_message <- function(task, i18n) {
  if (is.null(task) || task == "") {
    return("")
  }

  # Handle special task keywords
  if (task %in% c("initializing", "download_start", "compute_start",
                  "complete", "error", "resuming")) {
    return(i18n$t(paste0("task_", task)))
  }

  # Handle download_complete
  if (task == "download_complete") {
    return(i18n$t("download_complete"))
  }

  # Spectral diversity (B4/L3) — biodivMapR run, spec 028
  if (task == "spectral_diversity") {
    return(i18n$t("task_spectral_diversity"))
  }

  # OSO raster download progress "download_oso_progress:42"
  if (grepl("^download_oso_progress:", task)) {
    pct <- sub("^download_oso_progress:", "", task)
    return(paste0(i18n$t("downloading_source",
                         source = i18n$t("source_forest_cover")),
                  " : ", pct, " %"))
  }

  # LiDAR HD per-tile download "download_lidar:mnh:20/500"
  if (grepl("^download_lidar:", task)) {
    parts <- strsplit(sub("^download_lidar:", "", task), ":", fixed = TRUE)[[1]]
    product <- tolower(parts[1] %||% "mnh")
    progress <- parts[2] %||% ""
    lidar_key_map <- c(mnh = "source_lidar_mnh",
                       mnt = "source_lidar_mnt",
                       nuage = "source_lidar_copc")
    source_key <- if (product %in% names(lidar_key_map)) lidar_key_map[[product]]
                  else paste0("source_lidar_", product)
    return(paste0(i18n$t("downloading_source", source = i18n$t(source_key)),
                  " (", progress, ")"))
  }

  # CHM inference (Open-Canopy ML pipeline, spec 005 phase 6).
  # Not a download — handled separately so the UI does not mislabel
  # a multi-minute ML run as a "Téléchargement".
  if (task == "chm_inference_opencanopy") {
    return(i18n$t("chm_inference_opencanopy"))
  }

  # Backward-compat: pre-e74bdcc progress state files used
  # "download:source_chm_opencanopy" for the same event. Migrate
  # silently so resuming an old project doesn't spit a "key not
  # found" warning.
  if (task == "download:source_chm_opencanopy") {
    return(i18n$t("chm_inference_opencanopy"))
  }

  # Open-Canopy pipeline phases (5 steps).
  #   "chm_phase:load_aoi"                   -> "Étape 1/5 : chargement de l'AOI…"
  #   "chm_phase:download_ortho"             -> "Étape 2/5 : téléchargement ortho IGN…"
  #   "chm_phase:setup_python"               -> "Étape 3/5 : configuration Python…"
  #   "chm_phase:download_model:pvtv2"       -> "Étape 3/5 : téléchargement modèle pvtv2…"
  #   "chm_phase:inference:pvtv2"            -> "Étape 4/5 : inférence modèle pvtv2…"
  #   "chm_phase:export"                     -> "Étape 5/5 : export des résultats…"
  if (grepl("^chm_phase:", task)) {
    parts <- strsplit(sub("^chm_phase:", "", task), ":", fixed = TRUE)[[1]]
    step <- parts[1] %||% ""
    model <- parts[2] %||% NA_character_
    key <- paste0("chm_phase_", step)
    if (i18n$has(key)) {
      if (!is.na(model) && nzchar(model)) {
        return(i18n$t(key, model = model))
      }
      return(i18n$t(key))
    }
    return(task)
  }

  # Per-tile IGN ortho download inside the CHM pipeline.
  #   "chm_tile_start:rvb:28"  -> "Téléchargement RVB : 28 tuiles…"
  #   "chm_tile:rvb:5:28"      -> "Téléchargement RVB : tuile 5/28 …"
  if (grepl("^chm_tile_start:", task)) {
    parts <- strsplit(sub("^chm_tile_start:", "", task), ":", fixed = TRUE)[[1]]
    type_label <- toupper(parts[1] %||% "ortho")
    n_tiles <- suppressWarnings(as.integer(parts[2] %||% "0"))
    return(i18n$t("chm_tile_start", type = type_label, n = n_tiles))
  }
  if (grepl("^chm_tile:", task)) {
    parts <- strsplit(sub("^chm_tile:", "", task), ":", fixed = TRUE)[[1]]
    type_label <- toupper(parts[1] %||% "ortho")
    idx <- suppressWarnings(as.integer(parts[2] %||% "0"))
    n_tiles <- suppressWarnings(as.integer(parts[3] %||% "0"))
    return(i18n$t("chm_tile", type = type_label, idx = idx, n = n_tiles))
  }

  # Per-inference-tile progress during the ML step.
  #   "chm_inference_start:3"       -> "Inférence : 3 tuile(s)…"
  #   "chm_inference_tile:2:3"      -> "Inférence tuile 2/3…"
  if (grepl("^chm_inference_start:", task)) {
    n_tiles <- suppressWarnings(
      as.integer(sub("^chm_inference_start:", "", task))
    )
    return(i18n$t("chm_inference_start", n = n_tiles))
  }
  if (grepl("^chm_inference_tile:", task)) {
    parts <- strsplit(sub("^chm_inference_tile:", "", task), ":", fixed = TRUE)[[1]]
    idx <- suppressWarnings(as.integer(parts[1] %||% "0"))
    n_tiles <- suppressWarnings(as.integer(parts[2] %||% "0"))
    return(i18n$t("chm_inference_tile", idx = idx, n = n_tiles))
  }

  # Handle new format: "download:source_key"
  if (grepl("^download:", task)) {
    source_key <- sub("^download:", "", task)
    source_name <- i18n$t(source_key)
    return(i18n$t("downloading_source", source = source_name))
  }

  # Handle new format: "compute:indicator_key"
  if (grepl("^compute:", task)) {
    indicator_key <- sub("^compute:", "", task)
    indicator_name <- i18n$t(indicator_key)
    return(i18n$t("computing_indicator_name", indicator = indicator_name))
  }

  # Fallback: return task as-is
  task
}

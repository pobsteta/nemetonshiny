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
  indicator_W1 = list(fr = "R\u00e9seau hydrographique", en = "Water Network"),
  indicator_W2 = list(fr = "Zones humides", en = "Wetlands"),
  indicator_W3 = list(fr = "Indice topographique d'humidit\u00e9", en = "Topographic Wetness Index"),
  indicator_A1 = list(fr = "Tampon forestier", en = "Forest Buffer"),
  indicator_A2 = list(fr = "Qualit\u00e9 de l'air", en = "Air Quality"),
  indicator_F1 = list(fr = "Fertilit\u00e9 des sols", en = "Soil Fertility"),
  indicator_F2 = list(fr = "Risque d'\u00e9rosion", en = "Erosion Risk"),
  indicator_L1 = list(fr = "Sylvosph\u00e8re (effet lisi\u00e8re)", en = "Sylvosphere (Edge Effect)"),
  indicator_L2 = list(fr = "Fragmentation paysag\u00e8re", en = "Landscape Fragmentation"),
  indicator_T1 = list(fr = "Anciennet\u00e9 foresti\u00e8re", en = "Forest Age"),
  indicator_T2 = list(fr = "Taux de changement", en = "Change Rate"),
  indicator_R1 = list(fr = "Risque incendie", en = "Fire Risk"),
  indicator_R2 = list(fr = "Risque temp\u00eate", en = "Storm Risk"),
  indicator_R3 = list(fr = "Risque s\u00e9cheresse", en = "Drought Risk"),
  indicator_R4 = list(fr = "Risque abroutissement", en = "Browsing Risk"),
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

  # ============================================================
  # Theia / DATA TERRA configuration
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
  indicateur_w1_reseau = list(fr = "R\u00e9seau hydrographique", en = "Water Network"),
  indicateur_w2_zones_humides = list(fr = "Zones humides", en = "Wetlands"),
  indicateur_w3_humidite = list(fr = "Indice topographique d'humidit\u00e9", en = "Topographic Wetness Index"),
  indicateur_a1_couverture = list(fr = "Tampon forestier", en = "Forest Buffer"),
  indicateur_a2_qualite_air = list(fr = "Qualit\u00e9 de l'air", en = "Air Quality"),
  indicateur_f2_erosion = list(fr = "Risque d'\u00e9rosion (RUSLE)", en = "Erosion Risk (RUSLE)"),
  indicateur_f1_fertilite = list(fr = "Fertilit\u00e9 des sols (TWI+pente)", en = "Soil Fertility (TWI+slope)"),
  indicateur_l1_sylvosphere = list(fr = "Sylvosph\u00e8re (effet lisi\u00e8re)", en = "Sylvosphere (Edge Effect)"),
  indicateur_l2_fragmentation = list(fr = "Fragmentation paysag\u00e8re", en = "Landscape Fragmentation"),
  indicateur_t1_anciennete = list(fr = "Anciennet\u00e9 foresti\u00e8re", en = "Forest Age"),
  indicateur_t2_changement = list(fr = "Taux de changement", en = "Change Rate"),
  indicateur_r1_feu = list(fr = "Risque incendie", en = "Fire Risk"),
  indicateur_r2_tempete = list(fr = "Risque temp\u00eate", en = "Storm Risk"),
  indicateur_r3_secheresse = list(fr = "Risque s\u00e9cheresse", en = "Drought Risk"),
  indicateur_r4_abroutissement = list(fr = "Risque abroutissement", en = "Browsing Risk"),
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
  monitoring_date_range = list(
    fr = "P\u00e9riode d'observation",
    en = "Observation period"
  ),
  monitoring_bands = list(
    fr = "Indices spectraux",
    en = "Spectral indices"
  ),
  monitoring_threshold_ndvi = list(
    fr = "Seuil minimum NDVI",
    en = "Minimum NDVI threshold"
  ),
  monitoring_threshold_nbr = list(
    fr = "Seuil minimum NBR",
    en = "Minimum NBR threshold"
  ),
  monitoring_window_days = list(
    fr = "Fen\u00eatre roulante (jours)",
    en = "Rolling window (days)"
  ),
  monitoring_run_btn = list(
    fr = "Lancer le diagnostic FAST",
    en = "Run FAST diagnosis"
  ),
  monitoring_run_cancel_btn = list(
    fr = "Annuler / Réinitialiser",
    en = "Cancel / Reset"
  ),
  monitoring_run_cancel_done = list(
    fr = "Bouton réinitialisé. Vous pouvez relancer dès que le problème est corrigé. Note : le worker en cours continue en arrière-plan (les INSERT en base sont idempotents).",
    en = "Button reset. You can relaunch as soon as the problem is fixed. Note: the running worker continues in the background (DB INSERTs are idempotent)."
  ),
  monitoring_register_btn = list(
    fr = "Enregistrer ce projet comme zone de suivi",
    en = "Register this project as a monitoring zone"
  ),
  monitoring_register_no_project = list(
    fr = "Chargez un projet avant d'enregistrer une zone de suivi.",
    en = "Load a project before registering a monitoring zone."
  ),
  monitoring_register_no_samples = list(
    fr = "Générez d'abord un plan d'échantillonnage (onglet Échantillonnage).",
    en = "Generate a sampling plan first (Sampling tab)."
  ),
  monitoring_register_no_db = list(
    fr = "Base de suivi non configurée — impossible d'enregistrer la zone.",
    en = "Monitoring database not configured — cannot register zone."
  ),
  monitoring_register_running = list(
    fr = "Enregistrement de la zone en cours…",
    en = "Registering zone…"
  ),
  monitoring_register_success = list(
    fr = "Zone « %s » enregistrée (%d placette(s)).",
    en = "Zone “%s” registered (%d plot(s))."
  ),
  monitoring_register_already = list(
    fr = "Zone « %s » déjà enregistrée (%d placette(s)) — sélection réactivée.",
    en = "Zone “%s” already registered (%d plot(s)) — selection restored."
  ),
  monitoring_register_error = list(
    fr = "Échec de l'enregistrement",
    en = "Registration failed"
  ),
  monitoring_validate_zone = list(
    fr = "S\u00e9lectionnez une zone de suivi avant de lancer le t\u00e9l\u00e9chargement.",
    en = "Select a monitoring zone before starting the download."
  ),
  monitoring_validate_bands = list(
    fr = "S\u00e9lectionnez au moins un indice spectral (NDVI ou NBR).",
    en = "Select at least one spectral index (NDVI or NBR)."
  ),
  monitoring_validate_dates = list(
    fr = "P\u00e9riode d'observation invalide.",
    en = "Invalid observation period."
  ),
  monitoring_ingest_starting = list(
    fr = "T\u00e9l\u00e9chargement Sentinel-2 en cours\u2026 cela peut prendre quelques minutes.",
    en = "Sentinel-2 download running\u2026 this can take a few minutes."
  ),
  monitoring_ingest_success = list(
    fr = "T\u00e9l\u00e9chargement termin\u00e9 : %d sc\u00e8ne(s), %d observation(s) ins\u00e9r\u00e9e(s).",
    en = "Download finished: %d scene(s), %d observation(s) inserted."
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
    fr = "R\u00e9amorcer le cache COG (vider et recommencer)",
    en = "Reset COG cache (wipe and restart)"
  ),
  monitoring_reprime_cache_help = list(
    fr = "<strong>D\u00e9coch\u00e9</strong> (d\u00e9faut) : nemeton v\u00e9rifie le cache disque et ne t\u00e9l\u00e9charge que les bandes manquantes. INSERTs DB idempotents (<code>ON CONFLICT DO NOTHING</code>).<br><br><strong>Coch\u00e9</strong> : vide d'abord <code>cache_dir</code>, puis re-t\u00e9l\u00e9charge int\u00e9gralement sc\u00e8ne par sc\u00e8ne. \u00c0 utiliser pour r\u00e9cup\u00e9rer d'un cache corrompu.",
    en = "<strong>Unchecked</strong> (default): nemeton checks the disk cache and only downloads missing bands. DB INSERTs are idempotent (<code>ON CONFLICT DO NOTHING</code>).<br><br><strong>Checked</strong>: wipes <code>cache_dir</code> first, then re-downloads every scene/band from scratch. Use to recover from a corrupted cache."
  ),
  monitoring_db_unavailable = list(
    fr = "Base de suivi non configur\u00e9e.",
    en = "Monitoring database not configured."
  ),
  monitoring_db_check_env = list(
    fr = "Renseignez NEMETON_DB_URL ou les variables NEMETON_DB_HOST/_PORT/_NAME/_USER/_PASSWORD, ou ouvrez un projet pour utiliser le mode local (DuckDB).",
    en = "Set NEMETON_DB_URL or the NEMETON_DB_HOST/_PORT/_NAME/_USER/_PASSWORD variables, or open a project to use the local mode (DuckDB)."
  ),
  monitoring_db_connected = list(
    fr = "Base de suivi connect\u00e9e \u2014 %d zone(s) disponible(s).",
    en = "Monitoring database connected \u2014 %d zone(s) available."
  ),
  monitoring_db_local = list(
    fr = "Mode local (DuckDB) \u2014 base de suivi monoposte stock\u00e9e dans le projet.",
    en = "Local mode (DuckDB) \u2014 single-user monitoring database stored in the project."
  ),
  monitoring_db_local_hint = list(
    fr = "Pour partager le suivi entre plusieurs utilisateurs, configurez Postgres+TimescaleDB via NEMETON_DB_URL.",
    en = "To share monitoring between multiple users, configure Postgres+TimescaleDB via NEMETON_DB_URL."
  ),
  monitoring_db_duckdb_missing = list(
    fr = "Le paquet R {.pkg duckdb} n'est pas install\u00e9, le mode local n'est pas disponible. Installez avec install.packages(\"duckdb\") ou configurez Postgres.",
    en = "The R {.pkg duckdb} package is not installed; local mode is unavailable. Run install.packages(\"duckdb\") or configure Postgres."
  ),
  monitoring_db_no_project = list(
    fr = "Aucun projet charg\u00e9. S\u00e9lectionnez ou cr\u00e9ez un projet dans l'onglet S\u00e9lection pour activer le mode local (DuckDB).",
    en = "No project loaded. Pick or create a project in the Selection tab to enable local mode (DuckDB)."
  ),
  monitoring_db_connecting = list(
    fr = "Tentative de connexion \u00e0 la base de suivi\u2026",
    en = "Connecting to the monitoring database\u2026"
  ),
  monitoring_db_local_creating = list(
    fr = "Cr\u00e9ation de la base DuckDB locale dans le projet\u2026",
    en = "Creating the local DuckDB database in the project\u2026"
  ),
  monitoring_db_local_ready = list(
    fr = "Base locale (DuckDB) pr\u00eate.",
    en = "Local database (DuckDB) ready."
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
    fr = "Carte FORDEAD",
    en = "FORDEAD map"
  ),
  monitoring_fordead_map_placeholder_title = list(
    fr = "Carte FORDEAD — bientôt disponible",
    en = "FORDEAD map — coming soon"
  ),
  monitoring_fordead_map_placeholder_body = list(
    fr = "La carte des classes de dépérissement (sain / faible / moyenne / forte) sera affichée ici une fois que le cœur Nemeton exportera le raster classifié issu du run FORDEAD. En attendant, consulter l'onglet « Alertes FORDEAD » pour la liste des placettes flaguées et l'export QGIS associé.",
    en = "The dieback class map (healthy / low / medium / high) will appear here once the Nemeton core exports the classified raster from the FORDEAD run. Meanwhile, see the “FORDEAD alerts” tab for the flagged plots list and the matching QGIS export."
  ),
  # v0.35.0 — sous-onglets Alertes séparés par mode, symétriques aux
  # cartes : Alertes FAST (placettes au-dessus du seuil NDVI/NBR
  # rolling-window) vs Alertes FORDEAD (placettes flaguées par
  # run_fordead_dieback). Visibilité pilotée par input$mode.
  monitoring_subtab_alerts_fast = list(
    fr = "Alertes FAST",
    en = "FAST alerts"
  ),
  monitoring_subtab_alerts_fordead = list(
    fr = "Alertes FORDEAD",
    en = "FORDEAD alerts"
  ),
  monitoring_fast_alerts_placeholder_title = list(
    fr = "Alertes FAST — bientôt disponible",
    en = "FAST alerts — coming soon"
  ),
  monitoring_fast_alerts_placeholder_body = list(
    fr = "La liste des placettes dont NDVI ou NBR est passé sous le seuil sur la fenêtre roulante sera affichée ici une fois que le cœur Nemeton exportera list_fast_alerts_for_zone(). En attendant, consulter la « Carte FAST » pour le raster NDVI/NBR à pleine résolution Sentinel-2.",
    en = "The list of plots whose NDVI or NBR dropped below the rolling-window threshold will appear here once the Nemeton core exports list_fast_alerts_for_zone(). Meanwhile, see the “FAST map” for the full-resolution Sentinel-2 NDVI/NBR raster."
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
  monitoring_fast_alerts_empty_body = list(
    fr = "Aucune placette n'est passée sous le seuil NDVI ou NBR configuré. Élargir la fenêtre, relever le seuil ou changer la zone pour explorer d'autres scénarios.",
    en = "No plot crossed the configured NDVI or NBR threshold. Widen the window, raise the threshold, or change zone to explore further."
  ),
  monitoring_fast_alert_popup_plot = list(
    fr = "Placette",
    en = "Plot"
  ),
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

  # ----- Pixel map sub-tab (spec 010) -----
  monitoring_pixel_map_title = list(
    fr = "Carte pixel \u2014 NDVI / NBR \u00e0 la r\u00e9solution Sentinel-2 (10 m)",
    en = "Pixel map \u2014 NDVI / NBR at Sentinel-2 native resolution (10 m)"
  ),
  monitoring_pixel_map_index = list(
    fr = "Indice spectral",
    en = "Spectral index"
  ),
  monitoring_pixel_map_date = list(
    fr = "Date d'observation",
    en = "Observation date"
  ),
  monitoring_pixel_map_click_hint = list(
    fr = "Clique sur un pixel pour voir sa s\u00e9rie compl\u00e8te (NDVI + NBR). Clique sur une placette pour la s\u00e9rie agr\u00e9g\u00e9e plot.",
    en = "Click a pixel for its full series (NDVI + NBR). Click a plot marker for the placette-aggregated series."
  ),
  monitoring_pixel_map_modal_title_fmt = list(
    fr = "Pixel \u00e0 (lat: %.5f, lon: %.5f)",
    en = "Pixel at (lat: %.5f, lon: %.5f)"
  ),
  monitoring_pixel_map_placette_modal_title_fmt = list(
    fr = "Placette %s \u2014 s\u00e9rie NDVI / NBR (moyenne plot)",
    en = "Plot %s \u2014 NDVI / NBR series (plot mean)"
  ),
  monitoring_pixel_map_no_placette_data = list(
    fr = "Pas de donn\u00e9es pour cette placette dans la fen\u00eatre courante.",
    en = "No data for this plot in the current window."
  ),
  monitoring_pixel_map_no_cache = list(
    fr = "Pas de cache disque disponible. Lance une ingestion FAST pour peupler le cache COG.",
    en = "No disk cache available. Run a FAST ingestion to populate the COG cache."
  ),
  monitoring_pixel_map_loading = list(
    fr = "Construction du stack d'indices\u2026",
    en = "Building index stack\u2026"
  ),
  monitoring_pixel_map_no_pixel = list(
    fr = "Aucune donn\u00e9e pour ce pixel (hors emprise ou bandes manquantes).",
    en = "No data for this pixel (out of bounds or missing bands)."
  ),
  monitoring_pixel_map_scene_count_fmt = list(
    fr = "%d sc\u00e8nes disponibles dans le cache.",
    en = "%d scenes available in the cache."
  ),
  monitoring_alerts_placeholder = list(
    fr = "La carte des alertes appara\u00eetra apr\u00e8s le premier t\u00e9l\u00e9chargement.",
    en = "The alerts map will appear after the first download."
  ),
  # v0.36.5 \u2014 card \u00ab Zone saine \u00bb dans Alertes FORDEAD quand un run
  # se termine avec n_alerts_inserted = 0. \u00c9vite que l'utilisateur
  # confonde \u00ab pas encore lanc\u00e9 \u00bb / \u00ab run en cours \u00bb / \u00ab 0 anomalie \u00bb.
  monitoring_fordead_no_alerts_title = list(
    fr = "Zone saine \u2014 aucune anomalie d\u00e9tect\u00e9e",
    en = "Healthy zone \u2014 no anomaly detected"
  ),
  monitoring_fordead_no_alerts_body = list(
    fr = "Aucune placette ne pr\u00e9sente d'anomalie de d\u00e9p\u00e9rissement sur la p\u00e9riode d'observation. Le pipeline FORDEAD s'est termin\u00e9 avec succ\u00e8s mais n'a remont\u00e9 aucun pixel au-dessus du seuil d'anomalie configur\u00e9.",
    en = "No plot shows a dieback anomaly over the observation window. The FORDEAD pipeline completed successfully but did not flag any pixel above the configured anomaly threshold."
  ),
  monitoring_fordead_no_alerts_meta = list(
    fr = "Diagnostic termin\u00e9 en %.0f s.",
    en = "Diagnosis completed in %.0f s."
  ),

  # ----- Mode toggle (rapide / sanitaire) -----
  monitoring_mode_label = list(
    fr = "Mode de suivi",
    en = "Monitoring mode"
  ),
  monitoring_mode_quick = list(
    fr = "Surveillance rapide (FAST)",
    en = "Quick surveillance (FAST)"
  ),
  monitoring_mode_health = list(
    fr = "Diagnostic sanitaire (FORDEAD)",
    en = "Health diagnosis (FORDEAD)"
  ),
  monitoring_mode_quick_help = list(
    fr = "D\u00e9tection de chocs r\u00e9cents (coupe, chablis, incendie) via NDVI/NBR rolling-window. Quelques secondes.",
    en = "Detect recent shocks (cut, windthrow, fire) using NDVI/NBR rolling-window. Seconds."
  ),
  monitoring_mode_health_help = list(
    fr = "D\u00e9tection de d\u00e9p\u00e9rissement progressif (scolyte, s\u00e9cheresse) via FORDEAD. Plusieurs minutes \u00e0 heures.",
    en = "Detect progressive dieback (bark beetle, drought) using FORDEAD. Minutes to hours."
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
    fr = "Diagnostic FORDEAD lanc\u00e9. Dur\u00e9e estim\u00e9e : plusieurs minutes.",
    en = "FORDEAD diagnosis started. Estimated duration: several minutes."
  ),
  monitoring_health_success = list(
    fr = "Diagnostic termin\u00e9 : %d alertes ins\u00e9r\u00e9es en %.0f s.",
    en = "Diagnosis completed: %d alerts inserted in %.0f s."
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
  monitoring_qgis_btn = list(
    fr = "G\u00e9n\u00e9rer placettes QGIS (v\u00e9rification terrain)",
    en = "Generate QGIS plots (field verification)"
  ),
  monitoring_qgis_n_label = list(
    fr = "Nombre de placettes",
    en = "Number of plots"
  ),
  monitoring_qgis_method = list(
    fr = "M\u00e9thode d'\u00e9chantillonnage",
    en = "Sampling method"
  ),
  monitoring_qgis_no_alerts = list(
    fr = "Aucune alerte FORDEAD pendante dans cette zone.",
    en = "No pending FORDEAD alerts in this zone."
  ),
  monitoring_qgis_generated = list(
    fr = "%d placettes g\u00e9n\u00e9r\u00e9es. T\u00e9l\u00e9chargez le projet QGIS.",
    en = "%d plots generated. Download the QGIS project."
  ),

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

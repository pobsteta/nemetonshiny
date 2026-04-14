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
  tab_carte_tenements = list(fr = "Carte t\u00e8nement", en = "Tenement map"),
  tab_tableau_ug = list(fr = "Tableau", en = "Table"),
  ug_map_card_title = list(fr = "Carte des unit\u00e9s de gestion foresti\u00e8re", en = "Forest Management Units Map"),
  ug_map_summary_count = list(fr = "%d t\u00e8nement(s)", en = "%d tenement(s)"),
  ug_map_summary_surface = list(fr = "Surface t\u00e8nements : %s ha / %s ha (parcelles cadastrales)", en = "Tenement area: %s ha / %s ha (cadastral parcels)"),

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
    fr = "Limite de 20 parcelles atteinte",
    en = "Maximum 20 parcels reached"
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
  project_date = list(fr = "Date de cr\u00e9ation", en = "Creation date"),
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
  computation_failed = list(
    fr = "%d en \u00e9chec",
    en = "%d failed"
  ),
  unknown_error = list(fr = "Erreur inconnue", en = "Unknown error"),
  and_n_more_errors = list(fr = "Et %d autre(s) erreur(s)...", en = "And %d more error(s)..."),
  retry = list(fr = "R\u00e9essayer", en = "Retry"),
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
    fr = "Cliquez sur les parcelles cadastrales pour les s\u00e9lectionner. Un second clic les d\u00e9s\u00e9lectionne. Maximum 20 parcelles.",
    en = "Click on cadastral parcels to select them. A second click deselects them. Maximum 20 parcels."
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
  # Language
  # ============================================================
  language_changed = list(
    fr = "Langue chang\u00e9e. Rechargez la page pour appliquer.",
    en = "Language changed. Reload the page to apply."
  ),

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
  ug_title = list(fr = "Unit\u00e9s de Gestion Foresti\u00e8res", en = "Forest Management Units"),
  ug_merge = list(fr = "Regrouper", en = "Merge"),
  ug_split = list(fr = "Dissocier", en = "Split"),
  ug_rename = list(fr = "Renommer", en = "Rename"),
  ug_group = list(fr = "Groupe d'am\u00e9nagement", en = "Management group"),
  ug_apply_group = list(fr = "Appliquer le groupe", en = "Apply group"),
  ug_surface = list(fr = "Surface", en = "Area"),
  ug_tenements = list(fr = "T\u00e8nements", en = "Tenements"),
  ug_cadastral_refs = list(fr = "R\u00e9f\u00e9rences cadastrales", en = "Cadastral references"),
  ug_composition = list(fr = "Composition cadastrale", en = "Cadastral composition"),
  ug_recompute = list(fr = "Recalculer les indicateurs", en = "Recompute indicators"),
  ug_recomputing = list(fr = "Recalcul des indicateurs par UG en cours...", en = "Recomputing indicators per UG..."),
  ug_recompute_done = list(fr = "Indicateurs recalcul\u00e9s pour %d UG", en = "Indicators recomputed for %d UGs"),
  ug_recompute_failed = list(fr = "\u00c9chec du recalcul des indicateurs", en = "Failed to recompute indicators"),
  ug_confirm_merge = list(fr = "Fusionner les %d UG s\u00e9lectionn\u00e9es ?", en = "Merge %d selected UGs?"),
  ug_label_prompt = list(fr = "Nom de la nouvelle UG", en = "New UG name"),
  ug_label_required = list(fr = "Le nom de l'UG est requis", en = "UG name is required"),
  ug_no_data = list(fr = "Aucune UG d\u00e9finie. Chargez un projet pour commencer.", en = "No UG defined. Load a project to start."),
  ug_no_indicators = list(fr = "Aucun indicateur disponible. Lancez un calcul d'abord.", en = "No indicators available. Run a computation first."),
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
  ug_create_from_map = list(fr = "Cr\u00e9er UG depuis la s\u00e9lection", en = "Create UG from selection"),
  ug_create_btn = list(fr = "Cr\u00e9er l'UG", en = "Create UG"),
  ug_create_confirm = list(fr = "Cr\u00e9er une UG avec %d tenement(s) s\u00e9lectionn\u00e9(s) ?", en = "Create a UG with %d selected tenement(s)?"),
  ug_map_select_tenements_first = list(fr = "S\u00e9lectionnez des tenements sur la carte d'abord.", en = "Select tenements on the map first."),
  ug_clear_selection = list(fr = "Effacer la s\u00e9lection", en = "Clear selection"),
  ug_import_split = list(fr = "Importer un d\u00e9coupage", en = "Import subdivision"),
  ug_undo_split = list(fr = "Annuler le d\u00e9coupage", en = "Undo subdivision"),
  ug_split_select_parcel = list(fr = "Parcelle \u00e0 d\u00e9couper", en = "Parcel to subdivide"),
  ug_split_file = list(fr = "Fichier de d\u00e9coupage", en = "Subdivision file"),
  ug_split_hint = list(fr = "Importez un fichier GeoJSON, Shapefile ou GeoPackage contenant les polygones de subdivision.", en = "Import a GeoJSON, Shapefile or GeoPackage containing subdivision polygons."),
  ug_split_apply = list(fr = "Appliquer le d\u00e9coupage", en = "Apply subdivision"),
  ug_split_no_file = list(fr = "Veuillez s\u00e9lectionner un fichier.", en = "Please select a file."),
  ug_split_empty_file = list(fr = "Le fichier import\u00e9 ne contient aucun polygone.", en = "The imported file contains no polygons."),
  ug_split_success = list(fr = "Parcelle %s d\u00e9coup\u00e9e en %d tenements", en = "Parcel %s subdivided into %d tenements"),
  ug_split_error = list(fr = "Erreur lors du d\u00e9coupage :", en = "Subdivision error:"),
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
  ug_move_to = list(fr = "D\u00e9placer vers une UG", en = "Move to UG"),
  ug_move_desc = list(fr = "D\u00e9placer %d tenement(s) s\u00e9lectionn\u00e9(s) vers une UG existante.", en = "Move %d selected tenement(s) to an existing UG."),
  ug_move_target = list(fr = "UG de destination", en = "Target UG"),
  ug_move_confirm = list(fr = "D\u00e9placer", en = "Move"),
  ug_move_success = list(fr = "%d tenement(s) d\u00e9plac\u00e9(s) vers l'UG \u00ab %s \u00bb", en = "%d tenement(s) moved to UG '%s'")
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
#' Exports the translation dictionary to JSON files for use with shiny.i18n
#' or other i18n systems.
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
  if (task %in% c("download_start", "compute_start", "complete", "error", "resuming")) {
    return(i18n$t(paste0("task_", task)))
  }

  # Handle download_complete
  if (task == "download_complete") {
    return(i18n$t("download_complete"))
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

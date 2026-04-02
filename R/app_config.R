#' nemetonApp Configuration
#'
#' @description
#' Configuration constants and settings for the nemetonApp Shiny application.
#'
#' @name app_config
#' @keywords internal
NULL


#' Application configuration constants
#'
#' @noRd
APP_CONFIG <- list(
  # App metadata
  app_name = "N\u00e9m\u00e9ton",
  app_version = "0.11.0",
  app_title_fr = "N\u00e9m\u00e9ton - Diagnostic Forestier",
  app_title_en = "N\u00e9m\u00e9ton - Forest Diagnostic",

  # Limits
  max_parcels = 20L,
  max_project_name_length = 100L,
  max_description_length = 500L,

  # Timeouts (milliseconds)
  api_timeout = 30000L,
  wfs_timeout = 60000L,
  computation_timeout = 600000L,  # 10 minutes

  # Retry settings
  max_retries = 3L,
  retry_delay = 2000L,  # 2 seconds

  # Performance
  parallel_workers = NULL,  # NULL = auto-detect

  # Cache settings
  cache_format = "parquet",

  # Project states
  project_states = c("draft", "downloading", "computing", "completed", "error"),

  # CRS (ADR-008)
  # Stockage interne : ETRS89/LAEA paneuropeen
  storage_crs = 3035L,  # EPSG:3035 ETRS89/LAEA (paneuropeen)
  # Calculs metriques : reprojection automatique en CRS national
  default_crs = 2154L,  # EPSG:2154 Lambert-93 (France, fallback)

  # LLM settings
  llm_provider = "mistral",
  llm_models = list(
    anthropic = "claude-sonnet-4-5-20250929",
    mistral = "mistral-large-latest",
    openai = "gpt-4o",
    google = "gemini-2.0-flash",
    deepseek = "deepseek-chat",
    ollama = "llama3.1"
  )
)


#' Get app configuration value
#'
#' @param key Character. Configuration key to retrieve.
#' @param default Default value if key not found.
#' @return Configuration value.
#' @noRd
get_app_config <- function(key, default = NULL) {
  if (key %in% names(APP_CONFIG)) {
    return(APP_CONFIG[[key]])
  }
  return(default)
}


#' Indicator families configuration
#'
#' @description
#' Configuration for the 12 indicator families used in nemeton.
#'
#' @noRd
INDICATOR_FAMILIES <- list(
  C = list(
    code = "C",
    name_fr = "Carbone & Vitalit\u00e9",
    name_en = "Carbon & Vitality",
    icon = "tree-fill",
    color = "#228B22",
    indicators = c("C1", "C2"),
    column_names = c("indicateur_c1_biomasse", "indicateur_c2_ndvi"),
    indicator_labels = list(
      C1 = list(fr = "Biomasse carbone (tC/ha)", en = "Carbon Biomass (tC/ha)"),
      C2 = list(fr = "NDVI - Vitalit\u00e9", en = "NDVI - Vitality")
    ),
    indicator_tooltips = list(
      C1 = list(
        fr = "Stock de carbone dans la biomasse a\u00e9rienne (troncs, branches, feuilles). Estim\u00e9 \u00e0 partir de donn\u00e9es LiDAR ou de mod\u00e8les forestiers. Valeurs typiques : 50-200 tC/ha.",
        en = "Carbon stock in above-ground biomass (trunks, branches, leaves). Estimated from LiDAR data or forest models. Typical values: 50-200 tC/ha."
      ),
      C2 = list(
        fr = "Indice de v\u00e9g\u00e9tation par diff\u00e9rence normalis\u00e9e (NDVI). Mesure la vitalit\u00e9 et l'activit\u00e9 photosynth\u00e9tique de la v\u00e9g\u00e9tation. Valeurs de 0 (sol nu) \u00e0 1 (v\u00e9g\u00e9tation dense).",
        en = "Normalized Difference Vegetation Index (NDVI). Measures vegetation vitality and photosynthetic activity. Values from 0 (bare soil) to 1 (dense vegetation)."
      )
    )
  ),
  B = list(
    code = "B",
    name_fr = "Biodiversit\u00e9",
    name_en = "Biodiversity",
    icon = "bug-fill",
    color = "#9932CC",
    indicators = c("B1", "B2", "B3"),
    column_names = c("indicateur_b1_protection", "indicateur_b2_structure", "indicateur_b3_connectivite"),
    indicator_labels = list(
      B1 = list(fr = "Protection biodiversit\u00e9", en = "Biodiversity Protection"),
      B2 = list(fr = "Diversit\u00e9 structurale", en = "Structural Diversity"),
      B3 = list(fr = "Connectivit\u00e9 \u00e9cologique", en = "Ecological Connectivity")
    ),
    indicator_tooltips = list(
      B1 = list(
        fr = "Niveau de protection r\u00e9glementaire (ZNIEFF, Natura 2000, R\u00e9serves). Score de 0 (aucune protection) \u00e0 100 (protection maximale).",
        en = "Level of regulatory protection (ZNIEFF, Natura 2000, Reserves). Score from 0 (no protection) to 100 (maximum protection)."
      ),
      B2 = list(
        fr = "Diversit\u00e9 des strates verticales et horizontales du peuplement. Bas\u00e9 sur l'h\u00e9t\u00e9rog\u00e9n\u00e9it\u00e9 des hauteurs et des essences.",
        en = "Diversity of vertical and horizontal stand structure. Based on height and species heterogeneity."
      ),
      B3 = list(
        fr = "Capacit\u00e9 de la parcelle \u00e0 servir de corridor \u00e9cologique. Mesure la continuit\u00e9 foresti\u00e8re et la proximit\u00e9 d'autres habitats naturels.",
        en = "Parcel's capacity to serve as an ecological corridor. Measures forest continuity and proximity to other natural habitats."
      )
    )
  ),
  W = list(
    code = "W",
    name_fr = "Eau",
    name_en = "Water",
    icon = "droplet-fill",
    color = "#1E90FF",
    indicators = c("W1", "W2", "W3"),
    column_names = c("indicateur_w1_reseau", "indicateur_w2_zones_humides", "indicateur_w3_humidite"),
    indicator_labels = list(
      W1 = list(fr = "R\u00e9seau hydrographique", en = "Water Network"),
      W2 = list(fr = "Zones humides", en = "Wetlands"),
      W3 = list(fr = "Indice topographique d'humidit\u00e9", en = "Topographic Wetness Index")
    ),
    indicator_tooltips = list(
      W1 = list(
        fr = "Densit\u00e9 et proximit\u00e9 du r\u00e9seau hydrographique (cours d'eau, lacs). Impact sur la biodiversit\u00e9 aquatique et la r\u00e9gulation hydrique.",
        en = "Density and proximity of water network (streams, lakes). Impact on aquatic biodiversity and water regulation."
      ),
      W2 = list(
        fr = "Pr\u00e9sence et proximit\u00e9 de zones humides inventori\u00e9es. Milieux \u00e0 forte valeur \u00e9cologique pour la biodiversit\u00e9 et le stockage de carbone.",
        en = "Presence and proximity of inventoried wetlands. High ecological value habitats for biodiversity and carbon storage."
      ),
      W3 = list(
        fr = "Indice topographique d'humidit\u00e9 (TWI). Pr\u00e9dit l'accumulation d'eau selon la topographie. Valeurs \u00e9lev\u00e9es = zones potentiellement humides.",
        en = "Topographic Wetness Index (TWI). Predicts water accumulation based on topography. High values = potentially wet areas."
      )
    )
  ),
  A = list(
    code = "A",
    name_fr = "Air & Microclimat",
    name_en = "Air & Microclimate",
    icon = "wind",
    color = "#87CEEB",
    indicators = c("A1", "A2"),
    column_names = c("indicateur_a1_couverture", "indicateur_a2_qualite_air"),
    indicator_labels = list(
      A1 = list(fr = "Tampon forestier", en = "Forest Buffer"),
      A2 = list(fr = "Qualit\u00e9 de l'air", en = "Air Quality")
    ),
    indicator_tooltips = list(
      A1 = list(
        fr = "Couverture foresti\u00e8re dans un rayon de 500m. Mesure la capacit\u00e9 de la for\u00eat \u00e0 att\u00e9nuer les effets climatiques et filtrer l'air.",
        en = "Forest cover within 500m radius. Measures the forest's capacity to mitigate climate effects and filter air."
      ),
      A2 = list(
        fr = "Indice de qualit\u00e9 de l'air bas\u00e9 sur l'\u00e9loignement des sources de pollution et la densit\u00e9 foresti\u00e8re environnante.",
        en = "Air quality index based on distance from pollution sources and surrounding forest density."
      )
    )
  ),
  F = list(
    code = "F",
    name_fr = "Fertilit\u00e9 des Sols",
    name_en = "Soil Fertility",
    icon = "globe-americas",
    color = "#8B4513",
    indicators = c("F1", "F2"),
    column_names = c("indicateur_f2_erosion", "indicateur_f1_fertilite"),
    indicator_labels = list(
      F1 = list(fr = "Risque d'\u00e9rosion", en = "Erosion Risk"),
      F2 = list(fr = "Fertilit\u00e9 des sols", en = "Soil Fertility")
    ),
    indicator_tooltips = list(
      F1 = list(
        fr = "Risque d'\u00e9rosion des sols bas\u00e9 sur la pente, le type de sol et la couverture v\u00e9g\u00e9tale. Score \u00e9lev\u00e9 = faible risque.",
        en = "Soil erosion risk based on slope, soil type and vegetation cover. High score = low risk."
      ),
      F2 = list(
        fr = "Potentiel de fertilit\u00e9 des sols bas\u00e9 sur les caract\u00e9ristiques p\u00e9dologiques (texture, profondeur, mati\u00e8re organique).",
        en = "Soil fertility potential based on pedological characteristics (texture, depth, organic matter)."
      )
    )
  ),
  L = list(
    code = "L",
    name_fr = "Paysage",
    name_en = "Landscape",
    icon = "image-fill",
    color = "#32CD32",
    indicators = c("L1", "L2"),
    column_names = c("indicateur_l2_fragmentation", "indicateur_l1_sylvosphere"),
    indicator_labels = list(
      L1 = list(fr = "Sylvosph\u00e8re (effet lisi\u00e8re)", en = "Sylvosphere (Edge Effect)"),
      L2 = list(fr = "Fragmentation paysag\u00e8re", en = "Landscape Fragmentation")
    ),
    indicator_tooltips = list(
      L1 = list(
        fr = "Proportion de la parcelle sous influence des lisi\u00e8res (sylvosph\u00e8re). Les lisi\u00e8res favorisent certaines esp\u00e8ces mais fragmentent l'habitat int\u00e9rieur.",
        en = "Proportion of parcel under edge influence (sylvosphere). Edges favor some species but fragment interior habitat."
      ),
      L2 = list(
        fr = "Niveau de fragmentation du paysage forestier environnant. Bas\u00e9 sur la taille et la connectivit\u00e9 des massifs forestiers proches.",
        en = "Fragmentation level of surrounding forest landscape. Based on size and connectivity of nearby forest patches."
      )
    )
  ),
  T = list(
    code = "T",
    name_fr = "Dynamique Temporelle",
    name_en = "Temporal Dynamics",
    icon = "clock-fill",
    color = "#FFD700",
    indicators = c("T1", "T2"),
    column_names = c("indicateur_t1_anciennete", "indicateur_t2_changement"),
    indicator_labels = list(
      T1 = list(fr = "Anciennet\u00e9 foresti\u00e8re", en = "Forest Age"),
      T2 = list(fr = "Taux de changement", en = "Change Rate")
    ),
    indicator_tooltips = list(
      T1 = list(
        fr = "Anciennet\u00e9 de l'\u00e9tat bois\u00e9 depuis les cartes de Cassini (XVIIIe si\u00e8cle). Les for\u00eats anciennes abritent une biodiversit\u00e9 sp\u00e9cifique.",
        en = "Age of wooded state since Cassini maps (18th century). Ancient forests harbor specific biodiversity."
      ),
      T2 = list(
        fr = "Taux de changement de la couverture foresti\u00e8re sur les 30 derni\u00e8res ann\u00e9es. Valeurs positives = extension, n\u00e9gatives = r\u00e9gression.",
        en = "Rate of forest cover change over the last 30 years. Positive values = expansion, negative = regression."
      )
    )
  ),
  R = list(
    code = "R",
    name_fr = "Risques & R\u00e9silience",
    name_en = "Risks & Resilience",
    icon = "exclamation-triangle-fill",
    color = "#DC143C",
    indicators = c("R1", "R2", "R3", "R4"),
    column_names = c("indicateur_r1_feu", "indicateur_r2_tempete", "indicateur_r3_secheresse", "indicateur_r4_abroutissement"),
    indicator_labels = list(
      R1 = list(fr = "Risque incendie", en = "Fire Risk"),
      R2 = list(fr = "Risque temp\u00eate", en = "Storm Risk"),
      R3 = list(fr = "Risque s\u00e9cheresse", en = "Drought Risk"),
      R4 = list(fr = "Risque abroutissement", en = "Browsing Risk")
    ),
    indicator_tooltips = list(
      R1 = list(
        fr = "Susceptibilit\u00e9 au feu bas\u00e9e sur le climat, la v\u00e9g\u00e9tation inflammable et l'historique des incendies. Score \u00e9lev\u00e9 = faible risque.",
        en = "Fire susceptibility based on climate, flammable vegetation and fire history. High score = low risk."
      ),
      R2 = list(
        fr = "Vuln\u00e9rabilit\u00e9 aux temp\u00eates bas\u00e9e sur l'exposition, la hauteur des arbres et les vents dominants. Score \u00e9lev\u00e9 = faible risque.",
        en = "Storm vulnerability based on exposure, tree height and prevailing winds. High score = low risk."
      ),
      R3 = list(
        fr = "Sensibilit\u00e9 \u00e0 la s\u00e9cheresse (indice SPEI). Bas\u00e9 sur le bilan hydrique et les projections climatiques. Score \u00e9lev\u00e9 = faible risque.",
        en = "Drought sensitivity (SPEI index). Based on water balance and climate projections. High score = low risk."
      ),
      R4 = list(
        fr = "Pression de la faune sauvage (cervid\u00e9s) sur la r\u00e9g\u00e9n\u00e9ration foresti\u00e8re. Bas\u00e9 sur les donn\u00e9es cyn\u00e9g\u00e9tiques. Score \u00e9lev\u00e9 = faible pression.",
        en = "Wildlife pressure (deer) on forest regeneration. Based on hunting data. High score = low pressure."
      )
    )
  ),
  S = list(
    code = "S",
    name_fr = "Social & R\u00e9cr\u00e9atif",
    name_en = "Social & Recreational",
    icon = "people-fill",
    color = "#FF69B4",
    indicators = c("S1", "S2", "S3"),
    column_names = c("indicateur_s1_routes", "indicateur_s2_bati", "indicateur_s3_population"),
    indicator_labels = list(
      S1 = list(fr = "Distance aux routes", en = "Road Distance"),
      S2 = list(fr = "Distance aux b\u00e2timents", en = "Building Distance"),
      S3 = list(fr = "Proximit\u00e9 population", en = "Population Proximity")
    ),
    indicator_tooltips = list(
      S1 = list(
        fr = "Distance moyenne aux routes (BD TOPO). Une distance faible facilite l'acc\u00e8s mais peut augmenter les perturbations.",
        en = "Average distance to roads (BD TOPO). Low distance facilitates access but may increase disturbance."
      ),
      S2 = list(
        fr = "Distance moyenne aux b\u00e2timents (BD TOPO). Indicateur de proximit\u00e9 urbaine et de pression anthropique potentielle.",
        en = "Average distance to buildings (BD TOPO). Indicator of urban proximity and potential human pressure."
      ),
      S3 = list(
        fr = "Population dans un rayon de 10 km. Mesure le potentiel d'usage r\u00e9cr\u00e9atif et la pression sociale sur la for\u00eat.",
        en = "Population within 10 km radius. Measures recreational use potential and social pressure on the forest."
      )
    )
  ),
  P = list(
    code = "P",
    name_fr = "Production",
    name_en = "Production",
    icon = "box-seam-fill",
    color = "#006400",
    indicators = c("P1", "P2", "P3"),
    column_names = c("indicateur_p1_volume", "indicateur_p2_station", "indicateur_p3_qualite_bois"),
    indicator_labels = list(
      P1 = list(fr = "Volume de bois (m\u00b3/ha)", en = "Timber Volume (m\u00b3/ha)"),
      P2 = list(fr = "Productivit\u00e9", en = "Productivity"),
      P3 = list(fr = "Qualit\u00e9 du bois", en = "Timber Quality")
    ),
    indicator_tooltips = list(
      P1 = list(
        fr = "Volume de bois sur pied estim\u00e9 (m\u00b3/ha). Calcul\u00e9 \u00e0 partir de donn\u00e9es LiDAR ou de tarifs de cubage. Valeurs typiques : 100-400 m\u00b3/ha.",
        en = "Estimated standing timber volume (m\u00b3/ha). Calculated from LiDAR data or volume tables. Typical values: 100-400 m\u00b3/ha."
      ),
      P2 = list(
        fr = "Classe de fertilit\u00e9 de la station foresti\u00e8re. Bas\u00e9e sur le sol, le climat et la croissance potentielle des arbres.",
        en = "Forest site fertility class. Based on soil, climate and potential tree growth."
      ),
      P3 = list(
        fr = "Qualit\u00e9 potentielle du bois bas\u00e9e sur les essences pr\u00e9sentes et les conditions de croissance.",
        en = "Potential timber quality based on species present and growing conditions."
      )
    )
  ),
  E = list(
    code = "E",
    name_fr = "\u00c9nergie & Climat",
    name_en = "Energy & Climate",
    icon = "lightning-fill",
    color = "#FF8C00",
    indicators = c("E1", "E2"),
    column_names = c("indicateur_e1_bois_energie", "indicateur_e2_evitement"),
    indicator_labels = list(
      E1 = list(fr = "Bois-\u00e9nergie", en = "Wood Energy"),
      E2 = list(fr = "\u00c9vitement CO2", en = "CO2 Avoidance")
    ),
    indicator_tooltips = list(
      E1 = list(
        fr = "Potentiel de production de bois-\u00e9nergie (MWh/ha/an). Bas\u00e9 sur la biomasse disponible et l'accessibilit\u00e9.",
        en = "Wood energy production potential (MWh/ha/year). Based on available biomass and accessibility."
      ),
      E2 = list(
        fr = "\u00c9missions de CO2 \u00e9vit\u00e9es par substitution aux \u00e9nergies fossiles (tCO2/ha/an). Contribution \u00e0 la transition \u00e9nerg\u00e9tique.",
        en = "CO2 emissions avoided by substituting fossil fuels (tCO2/ha/year). Contribution to energy transition."
      )
    )
  ),
  N = list(
    code = "N",
    name_fr = "Naturalit\u00e9",
    name_en = "Naturalness",
    icon = "flower1",
    color = "#2E8B57",
    indicators = c("N1", "N2", "N3"),
    column_names = c("indicateur_n1_distance", "indicateur_n2_continuite", "indicateur_n3_naturalite"),
    indicator_labels = list(
      N1 = list(fr = "Distance infrastructures", en = "Infrastructure Distance"),
      N2 = list(fr = "Continuit\u00e9 foresti\u00e8re", en = "Forest Continuity"),
      N3 = list(fr = "Score de naturalit\u00e9", en = "Naturalness Score")
    ),
    indicator_tooltips = list(
      N1 = list(
        fr = "\u00c9loignement des infrastructures humaines (routes, b\u00e2timents). Une grande distance indique un environnement plus naturel.",
        en = "Distance from human infrastructure (roads, buildings). Greater distance indicates more natural environment."
      ),
      N2 = list(
        fr = "Continuit\u00e9 spatio-temporelle du couvert forestier. Les for\u00eats continues depuis longtemps ont une plus grande naturalit\u00e9.",
        en = "Spatio-temporal continuity of forest cover. Forests continuous for longer have greater naturalness."
      ),
      N3 = list(
        fr = "Score composite de naturalit\u00e9 int\u00e9grant structure, continuit\u00e9, \u00e9loignement et perturbations anthropiques.",
        en = "Composite naturalness score integrating structure, continuity, remoteness and human disturbance."
      )
    )
  )
)


#' Get all indicator family codes
#'
#' @return Character vector of family codes
#' @noRd
get_family_codes <- function() {
  names(INDICATOR_FAMILIES)
}


#' Get family configuration
#'
#' @param code Character. Family code (e.g., "C", "B", "W")
#' @return List with family configuration, or NULL if not found
#' @noRd
get_family_config <- function(code) {
  INDICATOR_FAMILIES[[toupper(code)]]
}


#' Get all indicator codes
#'
#' @return Character vector of all indicator codes
#' @noRd
get_all_indicator_codes <- function() {
  unlist(lapply(INDICATOR_FAMILIES, function(f) f$indicators), use.names = FALSE)
}


#' Get all indicator column names
#'
#' @return Character vector of all long-form column names
#' @noRd
get_all_column_names <- function() {
  unlist(lapply(INDICATOR_FAMILIES, function(f) f$column_names), use.names = FALSE)
}


#' Get column-to-family mapping
#'
#' @description
#' Returns a named character vector mapping column names to family codes.
#' Supports both short codes (C1, B2) and long-form names (indicateur_c1_biomasse).
#'
#' @return Named character vector (names = column names, values = family codes)
#' @noRd
get_column_family_map <- function() {
  result <- character(0)
  for (fam in INDICATOR_FAMILIES) {
    # Map long-form column_names to family code
    if (!is.null(fam$column_names)) {
      names_vec <- rep(fam$code, length(fam$column_names))
      names(names_vec) <- fam$column_names
      result <- c(result, names_vec)
    }
    # Map short indicators to family code
    names_vec2 <- rep(fam$code, length(fam$indicators))
    names(names_vec2) <- fam$indicators
    result <- c(result, names_vec2)
  }
  result
}


#' Data sources configuration
#'
#' @noRd
DATA_SOURCES <- list(
  cadastre = list(
    name = "cadastre",
    primary = "api_cadastre",
    fallback = "happign",
    required = TRUE
  ),
  bdforet = list(
    name = "bdforet",
    primary = "ign_wfs",
    fallback = "local_cache",
    required = TRUE
  ),
  protection = list(
    name = "protection",
    primary = "inpn_wfs",
    fallback = "local_cache",
    required = FALSE
  ),
  oso = list(
    name = "oso",
    primary = "recherche_data_gouv",
    fallback = "local_cache",
    required = FALSE
  ),
  hydro = list(
    name = "hydro",
    primary = "sandre_wfs",
    fallback = "local_cache",
    required = FALSE
  ),
  mnt = list(
    name = "mnt",
    primary = "ign_wfs",
    fallback = "local_cache",
    required = FALSE
  )
)


#' Get data source configuration
#'
#' @param name Character. Data source name
#' @return List with source configuration
#' @noRd
get_data_source_config <- function(name) {
  DATA_SOURCES[[name]]
}

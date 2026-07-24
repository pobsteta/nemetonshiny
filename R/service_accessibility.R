# ===========================================================================
# Service — Accessibilité forestière (ForêtAccess, onglet Terrain)
# ===========================================================================
#
# Adaptateur applicatif (non-Shiny) autour du paquet `foretaccess` (cartographie
# de l'accessibilité, réimplémentation de Sylvaccess — INRAE). Conformément aux
# règles 1/2 (aucune logique métier ici), ce fichier ne fait qu'orchestrer :
#   - résoudre l'AOI (géométrie du projet) et le MNT (déjà sur disque) ;
#   - acquérir la desserte (IGN BD TOPO V3) pour l'emprise ;
#   - appeler `foretaccess::preprocess()` + les moteurs terrestres exportés ;
#   - persister les rasters de classes + un GeoPackage vecteur exportable.
#
# Moteurs TERRESTRES (skidder, porteur, camion DFCI), en R pur, plus le moteur
# CÂBLE-MÂT (`potentiel_cable`, noyau Rust de foretaccess) : balayage 360°/pixel
# depuis les places de dépôt, très coûteux (plusieurs minutes à la dizaine de
# minutes selon l'emprise) — d'où son opt-in dans l'UI.
#
# Le calcul est LONG (rasterisation, focal, propagation least-cost) : il tourne
# dans un worker `future` (cf. mod_accessibility.R). Un `SpatRaster` terra n'est
# PAS sérialisable entre process (pointeur externe) : le worker ÉCRIT les
# rasters sur disque et ne renvoie que des CHEMINS + les data.frames de récap
# (sérialisables). Le process principal relit les `.tif` pour l'affichage.

#' Accessibility engines exposed by the app
#'
#' The three pure-R terrestrial engines + the **cable-crane** engine
#' (`foretaccess::potentiel_cable()`, Rust core), unblocked by foretaccess 1.19.0.
#'
#' The cable was long excluded for lack of a **landing-sites** layer (`departs`):
#' `potentiel_cable(departs = NULL)` fell back onto the whole road network
#' (~10 681 cells x 360 azimuths, > 1 h, knowingly optimistic). foretaccess now
#' produces that layer: `qualifier_desserte()` (NDP 1, LiDAR width) →
#' `places_depot()` (selective) → `departs` fed to `potentiel_cable()`. Without a
#' LiDAR point cloud the app falls back to `places_depot()` on the raw BD TOPO
#' (less selective, but correct). See `run_accessibility()`.
#' @noRd
ACCESSIBILITY_ENGINES <- c("skidder", "porteur", "camion_dfci", "cable")

# NOTE : `.resolve_project_aoi_2154()` (résolution AOI projet -> EPSG:2154) et
# `.acquire_mnt_highres()` (MNT 5 m HIGHRES) vivent désormais dans
# `R/service_foretaccess_io.R`, partagés avec le futur onglet Desserte.

#' Directory holding the accessibility artefacts of a project
#' @noRd
.accessibility_cache_dir <- function(project_path) {
  file.path(project_path, "cache", "accessibility")
}

#' Path to a project's accessibility-run GeoPackage (may not exist yet)
#'
#' Single source of the run's vector layers (`foret`, `desserte`, `places_depot`).
#' Used by both the Accessibility and Desserte maps to overlay the landing points.
#' @noRd
.accessibility_gpkg_path <- function(project_path) {
  if (is.null(project_path) || !nzchar(project_path)) return(NULL)
  file.path(.accessibility_cache_dir(project_path), "accessibilite.gpkg")
}

#' Read the `places_depot` layer (landing points) from a run GeoPackage, in WGS84
#'
#' Returns the landing points as an sf in EPSG:4326 (ready for Leaflet), or NULL
#' when the GeoPackage is missing, has no `places_depot` layer (NDP 0 / no cable
#' run), or the layer is empty.
#' @noRd
.acc_read_places_depot <- function(gpkg_path) {
  if (is.null(gpkg_path) || !file.exists(gpkg_path)) return(NULL)
  layers <- tryCatch(sf::st_layers(gpkg_path)$name, error = function(e) character(0))
  if (!("places_depot" %in% layers)) return(NULL)
  pd <- tryCatch(sf::st_read(gpkg_path, layer = "places_depot", quiet = TRUE),
                 error = function(e) NULL)
  if (!inherits(pd, "sf") || nrow(pd) == 0L) return(NULL)
  tryCatch(sf::st_transform(pd, 4326), error = function(e) pd)
}

#' Reconstruct a run result from a project's cached accessibility rasters
#'
#' Lets the tab show a **previously computed** analysis without recomputing:
#' scans `cache/accessibility/` for the known class rasters (`acc_<engine>.tif`
#' and `acc_classes_debardage.tif`) and rebuilds a minimal `run_accessibility()`
#' result (same shape as a live run, marked `from_cache = TRUE`). The per-engine
#' recap tables are NOT persisted, so `recaps` is empty — only the display layers
#' (rasters + exportable GeoPackage) are restored. Returns `NULL` when the project
#' has no cached raster yet.
#'
#' @param project_path Project directory, or `NULL`.
#' @return A result list compatible with the map/layer UI, or `NULL`.
#' @noRd
.load_cached_accessibility <- function(project_path) {
  if (is.null(project_path) || !nzchar(project_path)) return(NULL)
  cache_dir <- .accessibility_cache_dir(project_path)
  if (!dir.exists(cache_dir)) return(NULL)
  # Engines d'abord (dans l'ordre canonique), puis les classes de débardage :
  # le 1er élément devient la couche sélectionnée par défaut dans l'UI.
  known <- c(
    stats::setNames(paste0("acc_", ACCESSIBILITY_ENGINES, ".tif"),
                    ACCESSIBILITY_ENGINES),
    classes_debardage = "acc_classes_debardage.tif")
  raster_paths <- list()
  engines <- character(0)
  for (nm in names(known)) {
    p <- file.path(cache_dir, known[[nm]])
    if (file.exists(p)) {
      raster_paths[[nm]] <- p
      if (nm %in% ACCESSIBILITY_ENGINES) engines <- c(engines, nm)
    }
  }
  if (length(raster_paths) == 0L) return(NULL)
  gpkg <- file.path(cache_dir, "accessibilite.gpkg")
  # Raster ACCESSFOR (IGN) déjà calculé et écrit lors du run (validation
  # systématique) : s'il est sur disque, on le restaure pour que la couche
  # « Classes de débardage/ACCESSFOR (IGN) » réaffiche le volet sans requête WFS.
  af <- file.path(cache_dir, "accessfor_skidder.tif")
  list(
    status = "success",
    engines = engines,
    recaps = list(),
    raster_paths = raster_paths,
    gpkg_path = if (file.exists(gpkg)) gpkg else NULL,
    n_desserte = NA_integer_,
    accessfor_raster_path = if (file.exists(af)) af else NULL,
    from_cache = TRUE)
}

#' Flag the DFCI source tronçons on a road network (with provenance)
#'
#' `camion_dfci()` needs starting points (the `dfci` flag on `desserte`, turned
#' into `dfci_source_mask` by `preprocess()`) or it aborts. Resolution order:
#' 1. **OSM `ref:FR:DFCI`** network via `foretaccess::acquire_dfci()` +
#'    `flag_dfci()` — the authoritative source (`"osm"`).
#' 2. **Geometric fallback** built into `flag_dfci()` when no OSM DFCI line is
#'    found (emprise / traversal / turn-around heuristics) — `"geometrique"`.
#' 3. **App heuristic** of last resort when nothing was flagged: forest
#'    roads/tracks (`classe` in route/piste) are treated as sources —
#'    `"heuristique"` (surfaced to the user as a warning badge).
#'
#' @param desserte Road network `sf` (carries a `classe` column).
#' @param aoi_ext The buffered AOI (for the OSM DFCI fetch).
#' @param epsg Target EPSG (2154).
#' @param cache_dir Cache directory for the OSM DFCI fetch.
#' @return `list(desserte = <sf with dfci flag>, source = "osm"|"geometrique"|"heuristique")`.
#' @noRd
.resolve_desserte_dfci <- function(desserte, aoi_ext, epsg, cache_dir) {
  dfci_l <- tryCatch(
    foretaccess::acquire_dfci(aoi_ext, crs = epsg, cache_dir = cache_dir),
    error = function(e) NULL)
  desserte <- tryCatch(
    foretaccess::flag_dfci(desserte, dfci_l),
    error = function(e) { desserte$dfci <- 0L; desserte })
  n_flag <- suppressWarnings(sum(as.numeric(desserte[["dfci"]]), na.rm = TRUE))
  if (is.finite(n_flag) && n_flag > 0) {
    src <- if (inherits(dfci_l, "sf") && nrow(dfci_l) > 0L) "osm" else "geometrique"
  } else {
    cl <- as.character(desserte[["classe"]])
    desserte$dfci <- as.integer(!is.na(cl) & cl %in% c("route", "piste"))
    src <- "heuristique"
  }
  list(desserte = desserte, source = src)
}

#' Run the accessibility engines for a project (worker-side)
#'
#' Heavy, self-contained function meant to run in a `future` worker. Acquires
#' the **IGN RGE ALTI 5 m** DEM (Sylvaccess-calibrated resolution) and the road
#' network (**IGN BD TOPO V3**) for the buffered AOI, derives the forest mask
#' from **IGN BD Forêt V2** clipped to that emprise, runs
#' `foretaccess::preprocess()` then the requested engines, writes
#' each engine's categorical class raster to
#' `cache/accessibility/acc_<engine>.tif`, and writes an exportable GeoPackage
#' (`foret` + `desserte` layers). Returns only serialisable data (paths + recap
#' data.frames), never terra/sf objects tied to this process.
#'
#' The DEM, road network and BD Forêt layer are cached under
#' `cache_dir/emprise_<m>m/` by `acquire_mnt()` / `acquire_desserte()` /
#' `acquire_foret()`: a second run with the same buffer reuses them (no
#' re-download).
#'
#' Best-effort and structured: every failure path returns
#' `list(status = "error", reason = ...)` instead of throwing, so the caller's
#' status observer can surface a clean toast.
#'
#' @param aoi_path GeoPackage path holding the forest polygons (AOI). The AOI is
#'   passed as a FILE, never as a live `sf`: an `sf`/geometry can carry an
#'   external pointer that fails to serialise across the `future` process
#'   boundary ("external pointer is not valid"). The worker reads it here.
#' @param engines Character vector, subset of `ACCESSIBILITY_ENGINES`.
#' @param cache_dir Destination directory for the artefacts (and DEM/road cache).
#' @param buffer_m Numeric buffer, in metres, grown around the forest AOI for the
#'   DEM and road-network acquisition. Access to a stand comes from roads that
#'   lie OUTSIDE it, and the least-cost propagation needs the surrounding terrain;
#'   without a buffer the road network is clipped at the parcel edge and access
#'   is truncated. The forest **mask** stays the original AOI — only the analysed
#'   emprise widens. `0` (default) keeps the historical behaviour (forest extent
#'   only). Each buffer value acquires the DEM/roads in its own sub-cache so
#'   changing it re-fetches cleanly instead of reusing a stale emprise.
#' @return A named list describing the run (see details).
#' @noRd
run_accessibility <- function(aoi_path, engines, cache_dir, buffer_m = 0,
                              ndp1_lidar = FALSE, project_path = NULL) {
  if (!requireNamespace("foretaccess", quietly = TRUE)) {
    return(list(status = "error", reason = "accessibility_no_foretaccess"))
  }
  if (is.null(aoi_path) || !file.exists(aoi_path)) {
    return(list(status = "error", reason = "accessibility_need_project"))
  }
  aoi <- tryCatch(sf::st_read(aoi_path, quiet = TRUE), error = function(e) NULL)
  if (is.null(aoi) || !inherits(aoi, "sf") || nrow(aoi) == 0L) {
    return(list(status = "error", reason = "accessibility_need_project"))
  }
  engines <- intersect(engines, ACCESSIBILITY_ENGINES)
  if (length(engines) == 0L) {
    return(list(status = "error", reason = "accessibility_need_engine"))
  }

  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)

  # Tout le pipeline travaille en Lambert-93 (EPSG:2154), CRS national FR et
  # celui dans lequel acquire_mnt fournit le MNT : preprocess() valide l'égalité
  # stricte des CRS (mnt vs vecteurs).
  epsg <- 2154L
  aoi <- tryCatch(sf::st_transform(aoi, epsg), error = function(e) aoi)

  # Emprise d'ACQUISITION : AOI dilatée d'une zone tampon (buffer_m). L'accès à un
  # peuplement vient des routes situées HORS de lui, et la propagation least-cost
  # a besoin du relief alentour ; sans tampon, la desserte est coupée au bord de
  # la parcelle et l'accès est tronqué. Le MASQUE forêt (foret =) reste l'AOI
  # d'origine ci-dessous : seule l'emprise analysée s'élargit.
  buffer_m <- suppressWarnings(as.numeric(buffer_m %||% 0))
  if (!is.finite(buffer_m) || buffer_m < 0) buffer_m <- 0
  aoi_ext <- aoi
  if (buffer_m > 0) {
    aoi_ext <- tryCatch(sf::st_buffer(aoi, buffer_m), error = function(e) aoi)
  }
  # Chaque valeur de tampon acquiert MNT/desserte dans son propre sous-cache : un
  # changement de tampon re-télécharge sur la nouvelle emprise au lieu de réutiliser
  # un MNT/desserte figé à l'ancienne emprise (acquire_* cache par nom de fichier).
  acq_dir <- file.path(cache_dir, sprintf("emprise_%gm", buffer_m))
  dir.create(acq_dir, recursive = TRUE, showWarnings = FALSE)

  # 1. MNT IGN RGE ALTI 5 m (résolution pour laquelle Sylvaccess/ForêtAccess est
  # calibré). On privilégie la couche HAUTE RÉSOLUTION `…HIGHRES` (MNT 5 m
  # genuine, cf. .acquire_mnt_highres) car la couche standard utilisée par
  # `foretaccess::acquire_mnt` est plafonnée au zoom 11 (~25 m ré-échantillonné)
  # et produit un escalier -> pentes parasites -> artefacts « inexploitable ».
  # Repli sur acquire_mnt (couche standard) si le fetch HIGHRES échoue. Les deux
  # mettent en cache dans acq_dir : un 2e run réutilise.
  mnt_path <- .acquire_mnt_highres(aoi_ext, res_m = 5, crs = epsg, cache_dir = acq_dir)
  if (is.null(mnt_path)) {
    mnt_path <- tryCatch(
      foretaccess::acquire_mnt(aoi_ext, res_m = 5, crs = epsg, cache_dir = acq_dir),
      error = function(e) structure(list(msg = conditionMessage(e)), class = "acc_err"))
  }
  if (inherits(mnt_path, "acc_err")) {
    return(list(status = "error", reason = "accessibility_mnt_failed",
                detail = mnt_path$msg))
  }
  mnt <- tryCatch(terra::rast(mnt_path), error = function(e) NULL)
  if (is.null(mnt)) return(list(status = "error", reason = "accessibility_mnt_failed"))

  # 2. Desserte (réseau routier/pistes) via IGN BD TOPO V3 sur l'emprise
  # tamponnée (cachée).
  desserte <- tryCatch(
    foretaccess::acquire_desserte(aoi_ext, crs = epsg, cache_dir = acq_dir),
    error = function(e) structure(list(msg = conditionMessage(e)), class = "acc_err"))
  if (inherits(desserte, "acc_err")) {
    return(list(status = "error", reason = "accessibility_desserte_failed",
                detail = desserte$msg))
  }
  if (!inherits(desserte, "sf") || nrow(desserte) == 0L) {
    return(list(status = "error", reason = "accessibility_desserte_empty"))
  }

  # Flag DFCI sur la desserte : sans lui, `camion_dfci()` s'arrête (« Aucune
  # desserte-source DFCI »), car `preprocess()` construit `dfci_source_mask` à
  # partir de la colonne `dfci` de la desserte. Source OSM `ref:FR:DFCI` en
  # priorité (foretaccess::acquire_dfci + flag_dfci), repli géométrique de
  # flag_dfci, puis heuristique app routes/pistes en dernier recours. La
  # provenance (`dfci_source`) est remontée pour signaler le cas heuristique à
  # l'utilisateur. Posé seulement si le moteur DFCI est demandé.
  dfci_source <- NA_character_
  if ("camion_dfci" %in% engines) {
    fl <- .resolve_desserte_dfci(desserte, aoi_ext, epsg, acq_dir)
    desserte <- fl$desserte
    dfci_source <- fl$source
  }

  # 3. Masque forêt = forêt réelle (IGN BD Forêt V2) restreinte à l'emprise
  # tamponnée, PAS la simple géométrie déclarée du projet. `acquire_foret`
  # clippe la BD Forêt sur `aoi_ext` par st_intersection : le résultat est
  # exactement (emprise projet + buffer) ∩ forêt BD Forêt V2. Ainsi l'analyse
  # couvre toute la forêt accessible dans le tampon (y compris hors parcelles du
  # projet) et ne peint pas en « forêt » des zones non boisées. Repli sur la
  # géométrie projet si la BD Forêt est indisponible ou vide sur l'emprise.
  foret_bd <- tryCatch(
    foretaccess::acquire_foret(aoi_ext, crs = epsg, cache_dir = acq_dir),
    error = function(e) NULL)
  foret_mask <- if (inherits(foret_bd, "sf") && nrow(foret_bd) > 0L) foret_bd else aoi

  # 4. Prétraitement commun (pente, exposition, masques, rasterisation).
  pre <- tryCatch(
    foretaccess::preprocess(mnt = mnt, desserte = desserte, foret = foret_mask),
    error = function(e) structure(list(msg = conditionMessage(e)), class = "acc_err"))
  if (inherits(pre, "acc_err")) {
    return(list(status = "error", reason = "accessibility_preprocess_failed",
                detail = pre$msg))
  }

  # 5bis. Couche `departs` (places de dépôt) pour le moteur câble (foretaccess
  # 1.19.0). En NDP 1 (opt-in + nuage LiDAR présent), `qualifier_desserte()` mesure
  # la largeur carrossable → `places_depot()` devient sélective (départs réalistes)
  # → `potentiel_cable()` plus juste. Sans nuage : `places_depot()` sur la BD TOPO
  # brute (largeur absente → peu sélectif, mais correct). `cable_departs_source`
  # est remonté pour le badge de provenance (comme `dfci_source`).
  departs <- NULL
  cable_departs_source <- NA_character_
  if ("cable" %in% engines) {
    des_cable <- desserte
    # Chemin CANONIQUE du nuage LiDAR HD (convention cache/layers/<source>, cf.
    # resolve_regen_lidar_grid) ; repli sur cache_dir si project_path absent.
    laz_dir <- if (!is.null(project_path))
      file.path(project_path, "cache", "layers", "lidar_nuage") else
      file.path(dirname(cache_dir), "layers", "lidar_nuage")
    has_laz <- dir.exists(laz_dir) &&
      length(list.files(laz_dir, pattern = "\\.(copc\\.)?laz$")) > 0L
    # La qualification LiDAR (`qualifier_desserte`) n'est SÛRE qu'à partir de
    # foretaccess 1.19.1 : les versions antérieures pouvaient segfaulter sur une
    # desserte étendue (crash worker, non rattrapable). Garde-fou de version : en
    # deçà de 1.19.1 on reste sur la desserte brute (NDP 0) même si le NDP 1 est
    # coché — l'app tourne sur 1.19.0 sans risque, NDP 1 s'active après mise à jour.
    fa_ndp1_ok <- tryCatch(utils::packageVersion("foretaccess") >= "1.19.1",
                           error = function(e) FALSE)
    if (isTRUE(ndp1_lidar) && has_laz && fa_ndp1_ok &&
        requireNamespace("lidR", quietly = TRUE) &&
        requireNamespace("ALSroads", quietly = TRUE)) {
      dq <- tryCatch(
        foretaccess::qualifier_desserte(desserte, las_source = laz_dir, mnt = mnt),
        error = function(e) NULL)
      if (inherits(dq, "sf")) { des_cable <- dq; cable_departs_source <- "ndp1_lidar" }
      else cable_departs_source <- "ndp0_brute"   # qualif échouée -> repli brut
    } else {
      cable_departs_source <- "ndp0_brute"
    }
    # En NDP 1, `qualifier_desserte()` a mesuré la largeur carrossable
    # (`largeur_carrossable_m`) : on la passe à `places_depot()` via `largeur_champ`
    # pour des départs SÉLECTIFS (seuls les tronçons assez larges portent une place
    # de dépôt — ~1189 vs ~1877 sans filtre sur une desserte réelle). En NDP 0
    # (desserte brute, colonne absente), on laisse `largeur_champ = NULL` (défaut).
    lc <- if (identical(cable_departs_source, "ndp1_lidar") &&
              "largeur_carrossable_m" %in% names(des_cable))
      "largeur_carrossable_m" else NULL
    departs <- tryCatch(
      foretaccess::places_depot(des_cable, mnt, foret = foret_mask,
                                largeur_champ = lc),
      error = function(e) structure(list(msg = conditionMessage(e)), class = "acc_err"))
    if (inherits(departs, "acc_err")) {
      return(list(status = "error", reason = "accessibility_cable_departs_failed",
                  detail = departs$msg))
    }
  }

  # 5. Moteurs : raster de classes -> disque ; recap -> mémoire.
  engine_fun <- list(
    skidder = foretaccess::skidder,
    porteur = foretaccess::porteur,
    camion_dfci = foretaccess::camion_dfci,
    # Signature différente (departs) mais même forme de retour ($accessibilite /
    # $recap) : closure pour l'aligner sur le contrat f(pre) de la boucle.
    cable = function(pre) foretaccess::potentiel_cable(pre, departs = departs))
  recaps <- list()
  raster_paths <- list()
  for (eng in engines) {
    res <- tryCatch(engine_fun[[eng]](pre),
      error = function(e) structure(list(msg = conditionMessage(e)), class = "acc_err"))
    if (inherits(res, "acc_err")) {
      return(list(status = "error", reason = "accessibility_engine_failed",
                  detail = paste0(eng, ": ", res$msg)))
    }
    rp <- file.path(cache_dir, paste0("acc_", eng, ".tif"))
    ok <- tryCatch({
      terra::writeRaster(res$accessibilite, rp, overwrite = TRUE)
      TRUE
    }, error = function(e) FALSE)
    if (isTRUE(ok)) raster_paths[[eng]] <- rp
    recaps[[eng]] <- res$recap

    # Le skidder produit AUSSI le raster « classes de débardage » : les bandes de
    # distance Sylvaccess (0-250, …, > 2000 m) + inaccessible / inexploitable /
    # hors_foret, prêt à l'affichage avec sa table de couleurs (vert proche ->
    # rouge lointain). `pre` fournit le masque d'exclusion (classe inexploitable).
    if (identical(eng, "skidder")) {
      dbg <- tryCatch(foretaccess::classes_debardage(res, pre),
        error = function(e) NULL)
      if (!is.null(dbg)) {
        rpd <- file.path(cache_dir, "acc_classes_debardage.tif")
        okd <- tryCatch({
          terra::writeRaster(dbg, rpd, overwrite = TRUE)
          TRUE
        }, error = function(e) FALSE)
        if (isTRUE(okd)) raster_paths[["classes_debardage"]] <- rpd
      }
    }
  }

  # 6. GeoPackage exportable : masque forêt (BD Forêt V2 ∩ emprise) + desserte.
  # Best-effort.
  gpkg_path <- file.path(cache_dir, "accessibilite.gpkg")
  unlink(gpkg_path)
  tryCatch({
    sf::st_write(sf::st_transform(foret_mask, 2154), gpkg_path, layer = "foret",
                 quiet = TRUE, delete_dsn = TRUE)
    sf::st_write(sf::st_transform(desserte, 2154), gpkg_path, layer = "desserte",
                 quiet = TRUE, append = TRUE)
    # Places de dépôt calculées par `places_depot()` le long de la desserte
    # (corrigée au LiDAR en NDP 1) : on PERSISTE la géométrie (pas seulement le
    # nombre) pour l'afficher en couche « Places de dépôt » sur les cartes
    # Accessibilité et Desserte.
    if (inherits(departs, "sf") && nrow(departs) > 0L) {
      sf::st_write(sf::st_transform(departs, 2154), gpkg_path,
                   layer = "places_depot", quiet = TRUE, append = TRUE)
    }
  }, error = function(e) cli::cli_warn(
    "accessibility GPKG write failed: {conditionMessage(e)}"))

  # 7. Validation ACCESSFOR (IGN) SYSTÉMATIQUE dès que les classes de débardage
  # ont été produites (skidder) : récupère la couche nationale IGN (WFS happign),
  # la reclasse sur NOTRE grille + emprise, calcule l'accord et écrit le raster
  # ACCESSFOR affichable. Exécuté ICI (dans le worker `future`) pour ne pas bloquer
  # le thread principal. Best-effort réseau : un échec (WFS indisponible, happign
  # absent) ne fait PAS échouer l'analyse — le rendu « classes de débardage »
  # retombe alors sur un raster simple (pas de volet ACCESSFOR).
  accessfor <- NULL
  if (!is.null(raster_paths[["classes_debardage"]])) {
    accessfor <- tryCatch(
      run_accessfor_validation(raster_paths[["classes_debardage"]], "skidder"),
      error = function(e) NULL)
    if (!is.list(accessfor) || !identical(accessfor$status, "success")) {
      accessfor <- NULL
    }
  }

  list(
    status = "success",
    engines = engines,
    recaps = recaps,
    raster_paths = raster_paths,
    gpkg_path = if (file.exists(gpkg_path)) gpkg_path else NULL,
    n_desserte = nrow(desserte),
    dfci_source = dfci_source,
    cable_departs_source = cable_departs_source,
    n_departs = if (inherits(departs, "sf")) nrow(departs) else NA_integer_,
    # ACCESSFOR (référence IGN) : chemin du raster affichable (vis-à-vis des classes
    # de débardage sous le volet) + résumé d'accord pour le panneau de validation.
    accessfor_raster_path = accessfor$accessfor_raster_path,
    accessfor = accessfor)
}

#' Combine per-engine recap tables into a single display data.frame
#'
#' One row per accessibility class, one `surface_ha` column per engine
#' (translated engine header). Classes are unioned across engines and NA-safe.
#'
#' @param recaps Named list of recap data.frames (`classe`, `surface_ha`).
#' @param i18n An i18n object (for the engine column headers).
#' @return A data.frame ready for `DT::datatable`, or `NULL` when empty.
#' @noRd
.accessibility_recap_table <- function(recaps, i18n) {
  recaps <- Filter(function(x) is.data.frame(x) && nrow(x) > 0L, recaps %||% list())
  if (length(recaps) == 0L) return(NULL)
  classes <- unique(unlist(lapply(recaps, function(d) as.character(d$classe))))
  out <- data.frame(classe = classes, stringsAsFactors = FALSE)
  eng_label <- c(
    skidder = i18n$t("acc_engine_skidder"),
    porteur = i18n$t("acc_engine_porteur"),
    camion_dfci = i18n$t("acc_engine_dfci"))
  for (eng in names(recaps)) {
    d <- recaps[[eng]]
    idx <- match(out$classe, as.character(d$classe))
    col <- round(as.numeric(d$surface_ha)[idx], 2)
    col[is.na(col)] <- 0
    out[[eng_label[[eng]] %||% eng]] <- col
  }
  out
}

#' Export the accessibility GeoPackage produced by a run
#'
#' Copies the cached `accessibilite.gpkg` to the download target. Returns
#' `TRUE` on success, `FALSE` (best-effort) otherwise.
#'
#' @param result A `run_accessibility()` result list.
#' @param file Destination path handed to the browser.
#' @return Invisibly `TRUE`/`FALSE`.
#' @noRd
export_accessibility_geopackage <- function(result, file) {
  src <- tryCatch(result$gpkg_path, error = function(e) NULL)
  if (is.null(src) || !file.exists(src)) return(invisible(FALSE))
  invisible(isTRUE(tryCatch(file.copy(src, file, overwrite = TRUE),
                            error = function(e) FALSE)))
}

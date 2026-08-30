# ===========================================================================
# Service - Creation de desserte forestiere (ForetAccess, onglet Terrain)
# ===========================================================================
#
# Adaptateur applicatif (non-Shiny) autour des moteurs de CREATION de reseau de
# `foretaccess` (conception de desserte, reimplementation de SylvaRoad - INRAE).
# Regles 1/2 (aucune logique metier ici) : ce fichier ne fait qu'orchestrer -
# resoudre parcelles + entrees terrain (MNT / desserte existante / foret),
# appeler `surface_cout_construction()` puis un moteur de creation, et persister
# le reseau (raster + GeoPackage vecteur exportable).
#
# Deux moteurs de creation exposes : GLOUTON et STEINER (cf. DESSERTE_ENGINES).
#
# ATTENTION a la lecture des durees. Le glouton trace un A* par CELLULE de
# parcelle non desservie - pas par parcelle. Le nombre de traces est donc de
# l'ordre de la surface / resolution2 (309 726 cellules-source sur Dabo pour
# 4 UGF), et il est pilote par `skidding_m` : une cellule a moins de
# `skidding_m` d'une route est deja desservie et n'engendre aucun trace.
#
# Les ~11,5 min jadis mesurees sur Chastel-Nouvel (30 parcelles / 31 ha, ~302k
# cellules) l'ont ete a `skidding_m = 0`, le PIRE CAS documente par le coeur
# (" both slow and over-connected "). Avec une distance realiste, mesure sur
# Dabo (741 312 cellules) : 0 -> jamais fini en 22 min, 100 m -> 174 s,
# 300 m -> 70 s, 500 m -> 115 s. Cf. `DESSERTE_SKIDDING_DEFAULT_M`.
#
# `preprocess`/`surface_cout_construction` < 4 s : le moteur porte tout le temps.
# Le calcul tourne donc dans un worker `future` (cf. mod_desserte.R),
# comme l'accessibilite, et le moteur est OPT-IN avec avertissement " calcul
# long ". Le mode STEINER (N2 traces -> estime > 5 h a 30 parcelles) et les
# optimiseurs (`optimiser_reseau`) sont volontairement NON exposes tant qu'un
# travail perf n'a pas eu lieu cote `foretaccess` (brief coeur).
#
# Un `SpatRaster` terra n'est PAS serialisable entre process : le worker ECRIT
# le raster reseau + le GeoPackage sur disque et ne renvoie que des CHEMINS +
# des scalaires (cout, connexite, parcelles desservies). Le process principal
# relit pour l'affichage.

#' Road-network creation engines exposed by the app (v1)
#'
#' Both modes of `foretaccess::reseau_desserte()` are exposed. The former
#' exclusion ("until a perf pass happens upstream") is **obsolete**: the core has
#' since made the A* corridor-bounded, and `optimiser_reseau()`'s
#' `@section Performance` states the optimisers are "tractable at interactive
#' scale".
#'
#' **Steiner buys a far cheaper network, for a far longer wait.** Measured with
#' `skidding_m = 100` and `pondere_cout = TRUE` (so both minimise euros):
#'
#' | AOI | greedy | Steiner |
#' |---|---|---|
#' | ForetAccess - 30 parcels, 31 ha, 303 k cells | 6,1 s / 4 roads / **cost 65 983** | 694,6 s / 5 roads / **cost 10 420** |
#' | Dabo - 4 parcels, 774 ha, 1,35 M cells | 28,3 s / 36 roads | 78,4 s / 0 road |
#'
#' On ForetAccess Steiner divides the cost by **6,3** - it shares common trunks
#' between scattered parcels where greedy connects each to its nearest network
#' point - for **114x** the wall clock. On Dabo it returns nothing, which is
#' correct rather than broken: its spanning tree is built over *terminals*, a
#' coarser notion of "served" than greedy's per-cell one, and Dabo's four large
#' parcels already touch the network.
#'
#' So the choice is a genuine quality/time trade-off, and the user must make it
#' knowingly: on a large AOI Steiner is measured in tens of minutes.
#' @noRd
DESSERTE_ENGINES <- c("glouton", "steiner")

#' Default skidding/forwarding distance (m) for the creation engine
#'
#' A parcel cell within this distance of a road counts as already served and
#' spawns no trace. The core default is `0`, i.e. the documented worst case
#' ("both slow and over-connected"); the app must always pass a realistic value.
#'
#' Measured on Dabo (4 UGF / 774 ha, 741 312 grid cells at 5 m, 309 726 source
#' cells), `foretaccess 2.0.1` :
#'
#' | `skidding_m` | duration | roads |
#' |---:|---:|---:|
#' | 0 | never finished in 22 min | - |
#' | 100 m | 174 s | 39 |
#' | **300 m** | **70 s** | **0** |
#' | 500 m | 115 s | 0 |
#'
#' 300 m is the app default: a realistic ground-skidding distance, and the
#' fastest of the measured values. Non-monotonicity above it is expected - the
#' "is a road within reach?" test sweeps a disc of radius `skidding_m`, so its
#' cost grows as r2 while the number of traces falls.
#' @noRd
DESSERTE_SKIDDING_DEFAULT_M <- 300

#' Default platform width (m) for the earthwork cost method
#'
#' Only used by `methode_pente = "terrassement"`, whose volume grows as the
#' SQUARE of this width. The step-function scale is blind to it.
#' @noRd
DESSERTE_LARGEUR_DEFAULT_M <- 4

#' Default constructibility ceiling (% of terrain slope)
#'
#' Above this slope no road is traced, whichever pricing method is used. 60 % is
#' the ceiling the core's step function already implies - its first class priced
#' `Inf`. The core accepts `NULL` and derives it, but a `NULL` in the persisted
#' `meta` would make the cache comparison ambiguous, so the app writes the value.
#'
#' **Couplage assume** : si le bareme du coeur change sa derniere classe, cette
#' constante ne suivra pas. Elle est ici pour que le cache reste comparable.
#' @noRd
DESSERTE_PENTE_MAX_DEFAULT_PCT <- 60

#' Directory holding the desserte artefacts of a project
#' @noRd
.desserte_cache_dir <- function(project_path) {
  file.path(project_path, "cache", "desserte")
}

# Phases publiees par `run_desserte()` sur le canal disque, dans l'ordre. Le
# moteur porte l'essentiel du temps : sans ce canal, l'utilisateur n'a qu'un
# chrono qui tourne pendant des dizaines de minutes sans savoir ou en est le
# calcul - d'ou l'impression que " ca n'affiche rien ".
#
# `foretaccess::reseau_desserte()` n'expose AUCUN rappel de progression
# (verifie : pas de `progress`/`callback` dans ses arguments), donc on ne peut
# pas descendre sous la granularite de l'etape. La phase `moteur` reste longue.
DESSERTE_PHASES <- c("mnt", "desserte", "foret", "preprocess", "cout", "moteur")

# Ecrit la phase courante dans `engine_status.json` du cache desserte. Meme
# canal et meme contrat que `.regen_write_phase()` : tmp + rename atomique pour
# qu'un poll ne lise jamais un JSON tronque, et jamais fatal - un echec
# d'ecriture de statut ne doit pas interrompre le calcul.
.dess_write_phase <- function(cache_dir, phase) {
  tryCatch({
    payload <- list(phase = phase, ts = as.integer(Sys.time()))
    tmp <- file.path(cache_dir, ".engine_status.json.tmp")
    fin <- file.path(cache_dir, "engine_status.json")
    writeLines(jsonlite::toJSON(payload, auto_unbox = TRUE, null = "null"), tmp)
    file.rename(tmp, fin)
  }, error = function(e) invisible(NULL))
}

#' Network-integrity summary of the designed road network (spec 025)
#'
#' Wraps `foretaccess::verifier_integrite_desserte()` on the network the user
#' ends up with - **existing union created** - which is the only thing that answers
#' "does what I just designed hold together?". `raccorde` only says whether the
#' created roads are attached; it says nothing about the resulting graph.
#'
#' Guarded on `dessertR`: the check reaches it through
#' `.integrite_calculer()` -> `.dsr("dsr_reseau")`, and **`foretaccess` does not
#' declare that dependency** (absent from its Imports/Suggests/Remotes, resolved
#' at call time by `getExportedValue()`). Without it the core does not error - it
#' degrades to `.integrite_vide()`, whose `n_infractions` is `NA`. Returning
#' `NULL` here instead lets the UI say "unavailable" rather than render an empty
#' verdict that reads like a clean bill of health.
#'
#' Best-effort by design: an integrity failure must never cost the run its
#' network, which is already written to disk at this point.
#'
#' @param desserte Existing road network (`sf`, carries `classe`).
#' @param lignes Created roads (`sf`) or `NULL` when the engine built none.
#' @param aoi Parcels served, used to locate edge effects.
#' Le moteur optionnel `dessertR` est-il disponible ?
#'
#' Delegue au predicat du coeur (`foretaccess >= 2.1.0`) plutot que de refaire un
#' `requireNamespace("dessertR")` local. Le coeur fait autorite sur ce que
#' " disponible " veut dire pour SES quatre fonctions qui en dependent
#' (`qualifier_desserte`, `verifier_integrite_desserte`, `detecter_desserte`,
#' `acquire_desserte_lidar`) : si cette definition se durcit un jour, nous
#' suivons sans rien changer.
#'
#' A interroger AVANT de proposer l'action, pas apres : c'est ce qui permet de
#' dire " indisponible " au lieu d'afficher un resultat vide qui se lit comme un
#' bon bulletin de sante - le defaut corrige dans `foretaccess 2.1.0`.
#'
#' `dessertR` n'est pas declarable en `Suggests` cote coeur tant que `rlas` reste
#' archive sur le CRAN : sa declaration casse l'installation pour tous.
#'
#' @return `TRUE`/`FALSE`. `FALSE` si `foretaccess` lui-meme manque - on ne
#'   propose pas une action qu'on ne saurait pas executer.
#' @noRd
.dessertR_dispo <- function() {
  if (!requireNamespace("foretaccess", quietly = TRUE)) return(FALSE)
  isTRUE(tryCatch(foretaccess::dessertR_disponible(), error = function(e) FALSE))
}

#' @return Named list of scalars, or `NULL` when unavailable.
#' @noRd
.desserte_integrite <- function(desserte, lignes, aoi) {
  if (!.dessertR_dispo()) return(NULL)
  if (!inherits(desserte, "sf") || nrow(desserte) == 0L) return(NULL)
  geom_only <- function(x, classe) {
    sf::st_sf(classe = rep(classe, nrow(x)), geometry = sf::st_geometry(x))
  }
  reseau <- tryCatch({
    base <- sf::st_sf(classe = as.character(desserte[["classe"]]),
                      geometry = sf::st_geometry(desserte))
    if (inherits(lignes, "sf") && nrow(lignes) > 0L) {
      # Les routes creees n'ont pas de `classe` : elles sont des routes
      # forestieres neuves. Le libelle compte - `reseau_public` a un sens
      # particulier pour le controle de connectivite.
      rbind(base, geom_only(sf::st_transform(lignes, sf::st_crs(base)), "route"))
    } else base
  }, error = function(e) NULL)
  if (is.null(reseau)) return(NULL)

  r <- tryCatch(foretaccess::verifier_integrite_desserte(reseau, aoi = aoi),
                error = function(e) NULL)
  res <- tryCatch(as.list(r$resume), error = function(e) NULL)
  if (is.null(res) || is.null(res$n_infractions)) return(NULL)
  # `NA` = le coeur a degrade (dessertR injoignable malgre le garde). Ne pas le
  # presenter comme " 0 infraction ".
  if (!is.finite(suppressWarnings(as.numeric(res$n_infractions)))) return(NULL)
  list(
    n_infractions = as.integer(res$n_infractions),
    longueur_infraction_m = suppressWarnings(as.numeric(res$longueur_infraction_m)),
    n_composants = suppressWarnings(as.integer(res$n_composants)),
    n_composants_orphelins = suppressWarnings(as.integer(res$n_composants_orphelins)))
}

#' Read the persisted network-integrity summary of a project
#' @noRd
.load_cached_integrite <- function(cache_dir) {
  f <- file.path(cache_dir, "integrite.rds")
  if (!file.exists(f)) return(NULL)
  tryCatch(readRDS(f), error = function(e) NULL)
}

#' Run the network-integrity check on a project's designed network (worker-side)
#'
#' Deliberately a SEPARATE action, not a step of `run_desserte()`. Measured on
#' Dabo (3 122 segments over the 1 km emprise): **376,8 s**, against 39,7 s for
#' the whole creation run. Folding it in would have made "Generer la desserte"
#' ten times slower - reintroducing the very wait that v0.121.10 removed.
#'
#' Reads the network back from the run's GeoPackage (`desserte_existante` +
#' `reseau_cree`), so it needs no state from the creation worker.
#'
#' @param cache_dir Desserte cache directory of the project.
#' @param aoi_path Path to the parcels GeoPackage written by the module.
#' @return `list(status = "success", integrite = <scalars>)`, or a structured error.
#' @noRd
run_desserte_integrite <- function(cache_dir, aoi_path) {
  if (!requireNamespace("foretaccess", quietly = TRUE)) {
    return(list(status = "error", reason = "desserte_no_foretaccess"))
  }
  if (!.dessertR_dispo()) {
    return(list(status = "error", reason = "desserte_integrite_no_dessertr"))
  }
  gpkg <- file.path(cache_dir, "desserte.gpkg")
  if (!file.exists(gpkg) || is.null(aoi_path) || !file.exists(aoi_path)) {
    return(list(status = "error", reason = "desserte_integrite_no_reseau"))
  }
  lyr <- tryCatch(sf::st_layers(gpkg)$name, error = function(e) character(0))
  if (!("desserte_existante" %in% lyr)) {
    return(list(status = "error", reason = "desserte_integrite_no_reseau"))
  }
  existante <- tryCatch(sf::st_read(gpkg, layer = "desserte_existante", quiet = TRUE),
                        error = function(e) NULL)
  creees <- if ("reseau_cree" %in% lyr) {
    tryCatch(sf::st_read(gpkg, layer = "reseau_cree", quiet = TRUE),
             error = function(e) NULL)
  } else NULL
  aoi <- tryCatch(sf::st_read(aoi_path, quiet = TRUE), error = function(e) NULL)
  if (is.null(existante) || is.null(aoi)) {
    return(list(status = "error", reason = "desserte_integrite_no_reseau"))
  }
  integrite <- .desserte_integrite(existante, creees, aoi)
  if (is.null(integrite)) {
    return(list(status = "error", reason = "desserte_integrite_failed"))
  }
  tryCatch(saveRDS(integrite, file.path(cache_dir, "integrite.rds")),
           error = function(e) invisible(NULL))
  list(status = "success", integrite = integrite)
}

#' Default trials/iterations for the network optimiser
#'
#' The core recommends 8-32 trials and 100-300 iterations and states no hard cap
#' is needed below those (`optimiser_reseau()` `@section Performance`): trials
#' reuse a **single** neighbourhood table and run in parallel, so `n_start = 16`
#' costs about one greedy build.
#'
#' Measured on Dabo (emprise 1 km, `skidding_m = 100`) : greedy 82,2 s / 36 roads
#' / cost 16 673, against **100,2 s / 35 roads / cost 15 002** for multistart at
#' `n_start = 8` - 1,2x the wall clock for **-10 % cost**. Worth exposing.
#' @noRd
DESSERTE_OPTIM_N_START <- 8L
DESSERTE_OPTIM_N_ITER <- 100L
DESSERTE_OPTIM_STRATEGIES <- c("multistart", "recuit", "riprute")

#' Run the network optimiser on a project's designed network (worker-side)
#'
#' Separate action, same reasoning as the integrity check: it re-runs full greedy
#' builds, so it costs at least as much as the creation itself.
#'
#' @param cache_dir Desserte cache directory.
#' @param aoi_path Parcels GeoPackage written by the module.
#' @param strategie One of `DESSERTE_OPTIM_STRATEGIES`.
#' @param n_start,n_iter Trials / iterations.
#' @param buffer_m,skidding_m Same emprise and skidding distance as the creation run.
#' @return `list(status, cout, cout_initial, n_routes, strategie)` or an error list.
#' @noRd
run_desserte_optimiser <- function(cache_dir, aoi_path, strategie,
                                   n_start = DESSERTE_OPTIM_N_START,
                                   n_iter = DESSERTE_OPTIM_N_ITER,
                                   buffer_m = 0,
                                   skidding_m = DESSERTE_SKIDDING_DEFAULT_M) {
  if (!requireNamespace("foretaccess", quietly = TRUE)) {
    return(list(status = "error", reason = "desserte_no_foretaccess"))
  }
  strategie <- intersect(strategie, DESSERTE_OPTIM_STRATEGIES)[1]
  if (is.na(strategie)) {
    return(list(status = "error", reason = "desserte_optim_bad_strategie"))
  }
  if (is.null(aoi_path) || !file.exists(aoi_path)) {
    return(list(status = "error", reason = "desserte_need_project"))
  }
  parcelles <- tryCatch(sf::st_transform(sf::st_read(aoi_path, quiet = TRUE), 2154),
                        error = function(e) NULL)
  if (is.null(parcelles)) return(list(status = "error", reason = "desserte_need_project"))

  # Memes entrees que la creation : elles sont deja en cache sous l'emprise.
  acq_dir <- file.path(cache_dir, sprintf("emprise_%gm", buffer_m))
  aoi_ext <- if (buffer_m > 0) {
    tryCatch(sf::st_buffer(parcelles, buffer_m), error = function(e) parcelles)
  } else parcelles
  mnt_path <- .acquire_mnt_highres(aoi_ext, res_m = 5, crs = 2154, cache_dir = acq_dir)
  mnt <- tryCatch(terra::rast(mnt_path), error = function(e) NULL)
  desserte <- tryCatch(
    foretaccess::acquire_desserte(aoi_ext, crs = 2154, cache_dir = acq_dir),
    error = function(e) NULL)
  foret <- tryCatch(
    foretaccess::acquire_foret(aoi_ext, crs = 2154, cache_dir = acq_dir),
    error = function(e) NULL)
  if (is.null(mnt) || is.null(desserte)) {
    return(list(status = "error", reason = "desserte_optim_no_entrees"))
  }
  foret_mask <- if (inherits(foret, "sf") && nrow(foret) > 0L) foret else parcelles

  res <- tryCatch({
    pre <- foretaccess::preprocess(mnt = mnt, desserte = desserte, foret = foret_mask)
    cout <- foretaccess::surface_cout_construction(pre)
    foretaccess::optimiser_reseau(
      pre, cout, parcelles = parcelles, desserte_existante = desserte,
      strategie = strategie, n_start = as.integer(n_start),
      n_iter = as.integer(n_iter), skidding_m = skidding_m,
      # MEME ponderation que `run_desserte()` : sans elle l'optimiseur
      # minimiserait des METRES pendant que la creation minimise des EUROS, et
      # le panneau comparerait deux grandeurs differentes. Mesure sur
      # ForetAccess : 1 034 sans ponderation contre 65 983 pour la creation
      # ponderee - un " gain " de 98 % qui n'existe pas.
      pondere_cout = TRUE)
  }, error = function(e) structure(list(msg = conditionMessage(e)), class = "acc_err"))
  if (inherits(res, "acc_err")) {
    return(list(status = "error", reason = "desserte_optim_failed", detail = res$msg))
  }

  out <- list(
    strategie = strategie,
    cout = suppressWarnings(as.numeric(res$cout)),
    n_routes = if (inherits(res$lignes, "sf")) nrow(res$lignes) else 0L,
    n_start = as.integer(n_start), n_iter = as.integer(n_iter),
    skidding_m = skidding_m)
  tryCatch(saveRDS(out, file.path(cache_dir, "optimisation.rds")),
           error = function(e) invisible(NULL))
  c(list(status = "success"), out)
}

#' Read the persisted optimiser result
#' @noRd
.load_cached_optim <- function(cache_dir) {
  f <- file.path(cache_dir, "optimisation.rds")
  if (!file.exists(f)) return(NULL)
  tryCatch(readRDS(f), error = function(e) NULL)
}

#' Compare the BD TOPO network against OSM `track` ways (spec 028, worker-side)
#'
#' `acquire_desserte_osm()` is cheap (5,9 s, 544 segments on Dabo) but
#' `comparer_desserte_osm()` is not (104,2 s) - hence a separate action again.
#'
#' @return `list(status, n_osm, gpkg_path, resume, ...)` or an error list.
#' @noRd
run_desserte_osm <- function(cache_dir, aoi_path, buffer_m = 0) {
  if (!requireNamespace("foretaccess", quietly = TRUE)) {
    return(list(status = "error", reason = "desserte_no_foretaccess"))
  }
  if (is.null(aoi_path) || !file.exists(aoi_path)) {
    return(list(status = "error", reason = "desserte_need_project"))
  }
  parcelles <- tryCatch(sf::st_transform(sf::st_read(aoi_path, quiet = TRUE), 2154),
                        error = function(e) NULL)
  if (is.null(parcelles)) return(list(status = "error", reason = "desserte_need_project"))
  acq_dir <- file.path(cache_dir, sprintf("emprise_%gm", buffer_m))
  aoi_ext <- if (buffer_m > 0) {
    tryCatch(sf::st_buffer(parcelles, buffer_m), error = function(e) parcelles)
  } else parcelles

  osm <- tryCatch(foretaccess::acquire_desserte_osm(aoi_ext, crs = 2154,
                                                    cache_dir = acq_dir),
                  error = function(e) structure(list(msg = conditionMessage(e)),
                                                class = "acc_err"))
  if (inherits(osm, "acc_err")) {
    return(list(status = "error", reason = "desserte_osm_failed", detail = osm$msg))
  }
  if (!inherits(osm, "sf") || nrow(osm) == 0L) {
    return(list(status = "error", reason = "desserte_osm_empty"))
  }
  bdtopo <- tryCatch(foretaccess::acquire_desserte(aoi_ext, crs = 2154,
                                                   cache_dir = acq_dir),
                     error = function(e) NULL)
  if (is.null(bdtopo)) return(list(status = "error", reason = "desserte_osm_failed"))

  cmp <- tryCatch(foretaccess::comparer_desserte_osm(bdtopo, osm),
                  error = function(e) structure(list(msg = conditionMessage(e)),
                                                class = "acc_err"))
  if (inherits(cmp, "acc_err")) {
    return(list(status = "error", reason = "desserte_osm_failed", detail = cmp$msg))
  }
  # Le GeoPackage porte la couche OSM pour la carte et pour l'inspection SIG.
  # ATTENTION AU LIBELLE : c'est la couche OSM BRUTE telle qu'acquise, PAS le
  # resultat de la comparaison. `comparer_desserte_osm()` calcule bien un
  # " hors corridor " par troncon mais ne renvoie que des kilometres agreges ;
  # la geometrie du gisement est jetee cote coeur (brief sect.4, option (b)
  # deposee la-bas). Nommer ce calque " pistes absentes de la BD TOPO " serait
  # donc faux : il contient aussi tout ce qui doublonne la BD TOPO.
  gp <- file.path(cache_dir, "desserte_osm.gpkg")
  tryCatch({
    unlink(gp)
    sf::st_write(osm, gp, layer = "osm_track", quiet = TRUE, delete_dsn = TRUE)
  }, error = function(e) invisible(NULL))

  # PROVENANCE DU TRANSPORT (brief unification OSM, sect.4.2 et sect.5.3). Deux
  # executions a un mois d'ecart donnent des resultats differents sans aucune
  # trace ; et le transport Overpass lui-meme est en cours de refonte cote
  # `foretaccess` (requete unique + bissection au lieu du tuilage). Un cache
  # produit par l'ancien transport n'a donc pas la meme couverture qu'un cache
  # produit par le nouveau : on horodate et on versionne pour pouvoir le
  # refuser, plutot que de comparer sans le savoir des couvertures differentes.
  out <- list(n_osm = nrow(osm),
              # Le chemin VOYAGE avec le resultat, comme pour la detection : sans
              # lui le module devrait reconstruire la convention de nommage a la
              # main, et le calque disparaissait au rechargement du projet alors
              # que le fichier etait la (`osm.rds` ne le portait pas).
              gpkg_path = if (file.exists(gp)) gp else NULL,
              resume = tryCatch(as.list(cmp$resume), error = function(e) NULL),
              corridor_m = tryCatch(cmp$corridor_m, error = function(e) NA_real_),
              date_requete = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
              foretaccess_version = tryCatch(
                as.character(utils::packageVersion("foretaccess")),
                error = function(e) NA_character_))
  tryCatch(saveRDS(out, file.path(cache_dir, "osm.rds")),
           error = function(e) invisible(NULL))
  c(list(status = "success"), out)
}

#' Read the persisted OSM comparison
#'
#' Refuses a cache produced by another `foretaccess` version. The Overpass
#' transport is being reworked (single request + bisection instead of 1 km
#' tiling), which changes the COVERAGE and not merely the speed: silently
#' reusing an older cache would compare two different extractions without
#' saying so. A cache with no recorded version predates this guard and is
#' refused for the same reason.
#'
#' @param cache_dir Desserte cache directory.
#' @return The cached list, or `NULL` when absent/stale.
#' @noRd
.load_cached_osm <- function(cache_dir) {
  f <- file.path(cache_dir, "osm.rds")
  if (!file.exists(f)) return(NULL)
  out <- tryCatch(readRDS(f), error = function(e) NULL)
  if (!is.list(out)) return(NULL)
  vu <- out$foretaccess_version %||% NA_character_
  cour <- tryCatch(as.character(utils::packageVersion("foretaccess")),
                   error = function(e) NA_character_)
  if (is.na(vu) || is.na(cour) || !identical(vu, cour)) return(NULL)
  out
}

#' Detect roads absent from the reference network (dessertR, spec 026)
#'
#' Separate action, and the heaviest of the panel. Measured on the Dabo
#' accessibility emprise (4454 x 4162 @ 1 m, 1 855 ha, reference 1 032 segments):
#'
#' | `las_source` | duration | peak RSS | detections |
#' |---|---:|---:|---:|
#' | `NULL` (geomorphology only) | 189,4 s | **7,91 Go** | 0 |
#' | LiDAR point cloud | **> 10 min** (not completed under a 16 Go cap) | - | - |
#'
#' Two consequences the UI must carry:
#'
#' - the geomorphology-only path is cheap*er* but the core itself warns it is
#'   "nettement moins sure" - and it found nothing here. Offering it as a fast
#'   alternative would be offering a result one cannot trust;
#' - 7,91 Go on a 31 Go workstation shared with RStudio is already in the zone
#'   where `systemd-oomd` intervenes. The **memory guard is not optional**; it
#'   reuses `.desserte_memory_check()`, whose estimate is grid-driven and
#'   therefore applies here too.
#'
#' Depends on `dessertR` (via `.dsr("dsr_detecter")`), which `foretaccess` does
#' not declare - hence the explicit guard, as for the integrity check.
#'
#' @param cache_dir Desserte cache directory.
#' @param aoi_path Parcels GeoPackage written by the module.
#' @param buffer_m Emprise buffer (m), same as the creation run.
#' @param avec_lidar Use the LiDAR surface channel when the project has a cloud.
#' @param project_path Project root, to locate the LiDAR cache.
#' @return `list(status, n_detecte, gpkg_path)` or a structured error.
#' @noRd
run_desserte_detection <- function(cache_dir, aoi_path, buffer_m = 0,
                                   avec_lidar = TRUE, project_path = NULL) {
  if (!requireNamespace("foretaccess", quietly = TRUE)) {
    return(list(status = "error", reason = "desserte_no_foretaccess"))
  }
  if (!.dessertR_dispo()) {
    return(list(status = "error", reason = "desserte_detect_no_dessertr"))
  }
  if (is.null(aoi_path) || !file.exists(aoi_path)) {
    return(list(status = "error", reason = "desserte_need_project"))
  }
  parcelles <- tryCatch(sf::st_transform(sf::st_read(aoi_path, quiet = TRUE), 2154),
                        error = function(e) NULL)
  if (is.null(parcelles)) return(list(status = "error", reason = "desserte_need_project"))
  aoi_ext <- if (buffer_m > 0) {
    tryCatch(sf::st_buffer(parcelles, buffer_m), error = function(e) parcelles)
  } else parcelles

  # Garde-fou memoire AVANT toute acquisition : mesure 7,91 Go sur 1 855 ha meme
  # sans nuage. Sans ce refus, un depassement se paie par un OOM qui emporte la
  # session - le mode d'echec que toute cette serie de correctifs elimine.
  mem <- .desserte_memory_check(aoi_ext, res_m = 5)
  if (!isTRUE(mem$ok)) {
    return(list(status = "error", reason = "desserte_memory_guard",
                detail = sprintf(
                  "grille %.1f M cellules, pic estime %.1f Go, RAM disponible %.1f Go",
                  mem$cells / 1e6, mem$bytes / 1024^3, mem$available / 1024^3)))
  }

  acq_dir <- file.path(cache_dir, sprintf("emprise_%gm", buffer_m))

  # MNT : le LiDAR HD du projet EN PRIORITE, et pas le RGE ALTI 5 m des autres
  # etapes. `detecter_desserte()` cherche une signature de MICRO-RELIEF (SLRM,
  # openness, vesselness) et defaute a `dtm_res = 1` : a 5 m cette signature est
  # lissee et il ne trouve rien. Mesure sur ForetAccess avec le MNT 5 m,
  # `dsr_calibrer_specs()` ne retient AUCUN canal - AUC ~= 0,50 contre un seuil
  # de 0,55, c'est-a-dire pas mieux que le hasard. Le projet dispose pourtant
  # d'une mosaique LiDAR a 0,5 m.
  lidar_mnt <- if (!is.null(project_path)) {
    file.path(project_path, "cache", "layers", "lidar_mnt_mosaic.tif")
  } else NULL
  a_lidar_mnt <- !is.null(lidar_mnt) && file.exists(lidar_mnt)
  mnt_path <- if (a_lidar_mnt) lidar_mnt else {
    .acquire_mnt_highres(aoi_ext, res_m = 5, crs = 2154, cache_dir = acq_dir)
  }
  mnt <- tryCatch(terra::rast(mnt_path), error = function(e) NULL)
  reference <- tryCatch(
    foretaccess::acquire_desserte(aoi_ext, crs = 2154, cache_dir = acq_dir),
    error = function(e) NULL)
  if (is.null(mnt)) return(list(status = "error", reason = "desserte_detect_no_entrees"))

  # Nuage LiDAR du projet, s'il existe ET si l'utilisateur le demande. Sans lui
  # le coeur avertit que la detection est " nettement moins sure " : on ne
  # bascule donc JAMAIS silencieusement sur ce repli, on le remonte.
  laz_dir <- if (!is.null(project_path)) {
    file.path(project_path, "cache", "layers", "lidar_nuage")
  } else NULL
  a_du_laz <- !is.null(laz_dir) && dir.exists(laz_dir) &&
    length(list.files(laz_dir, pattern = "\\.(copc\\.)?laz$")) > 0L
  las_source <- if (isTRUE(avec_lidar) && a_du_laz) laz_dir else NULL

  det <- tryCatch(
    foretaccess::detecter_desserte(mnt, reference = reference,
                                   las_source = las_source),
    error = function(e) structure(list(msg = conditionMessage(e)), class = "acc_err"))
  if (inherits(det, "acc_err")) {
    return(list(status = "error", reason = "desserte_detect_failed", detail = det$msg))
  }
  n <- if (inherits(det, "sf")) nrow(det) else 0L

  # Classement (dessertR >= 1.3.0, brief sect.2). " Qu'est-ce qui a ete detecte ? "
  # est la question qui suit immediatement : en foret geree, ce qui remonte hors
  # reference est majoritairement du cloisonnement d'exploitation et du layon,
  # pas de la desserte.
  #
  # On ne passe QUE ce dont on dispose reellement. `stations` (fosses) et `ndvi`
  # (route/piste, et condition du pare-feu) demanderaient `dsr_measure()` et une
  # ortho IRC - non cables. Les criteres non renseignes sont declares INCONNUS
  # par dessertR, pas supposes : c'est pourquoi `CLASSE_CONF` doit accompagner
  # `CLASSE` dans l'affichage.
  #
  # `parcellaire` = contours d'UGF (brief sect.3) : ce sont des limites de GESTION,
  # d'ou `sous_type_parcelle = "section"`, passe EXPLICITEMENT - sans lui,
  # dessertR emet une notice, une valeur qui ne se lit pas dans la geometrie ne
  # se supposant pas en silence.
  classes <- NULL
  if (n > 0L) {
    # `foretaccess::classer_desserte()` et NON `dessertR::dsr_classer()` en
    # direct : l'app depend de `foretaccess`, jamais de son moteur (regle du
    # sens des dependances). L'appel direct etait de surcroit NON DECLARE dans
    # DESCRIPTION et enveloppe d'un `tryCatch(NULL)` - sur un poste sans
    # `dessertR`, le classement disparaissait EN SILENCE. Le wrapper fait le
    # recast LINESTRING lui-meme (la BD TOPO est multi) et rend l'indisponibilite
    # visible (avertissement + attribut `disponible`).
    det_cl <- tryCatch(
      foretaccess::classer_desserte(det, reference = reference,
                                    parcellaire = parcelles,
                                    sous_type_parcelle = "section"),
      error = function(e) NULL)
    if (inherits(det_cl, "sf") && "CLASSE" %in% names(det_cl)) {
      det <- det_cl
      conf <- suppressWarnings(as.numeric(det_cl[["CLASSE_CONF"]]))
      classes <- list(
        table = as.list(table(as.character(det_cl$CLASSE))),
        conf_moy = if (any(is.finite(conf))) mean(conf, na.rm = TRUE) else NA_real_,
        # Proposition de balisage OSM transportee au GeoPackage, JAMAIS
        # televersee : un import releve des regles de la communaute (brief sect.2).
        n_osm_tags = sum(!is.na(det_cl[["OSM_TAGS"]])))
    }
  }

  gp <- file.path(cache_dir, "desserte_detectee.gpkg")
  if (n > 0L) {
    tryCatch({
      unlink(gp)
      sf::st_write(det, gp, layer = "desserte_detectee", quiet = TRUE, delete_dsn = TRUE)
    }, error = function(e) invisible(NULL))
  }
  out <- list(n_detecte = n, avec_lidar = !is.null(las_source),
              mnt_lidar = a_lidar_mnt,
              mnt_res_m = tryCatch(terra::res(mnt)[1], error = function(e) NA_real_),
              classes = classes,
              gpkg_path = if (file.exists(gp)) gp else NULL)
  tryCatch(saveRDS(out, file.path(cache_dir, "detection.rds")),
           error = function(e) invisible(NULL))
  c(list(status = "success"), out)
}

#' Read the persisted detection result
#' @noRd
.load_cached_detection <- function(cache_dir) {
  f <- file.path(cache_dir, "detection.rds")
  if (!file.exists(f)) return(NULL)
  tryCatch(readRDS(f), error = function(e) NULL)
}

# --- Garde-fou memoire du glouton ------------------------------------------
#
# `foretaccess::reseau_desserte()` materialise une table de voisinage
# (`NeibTable.neighbors`, un `Vec<Vec<Neighbor>>` Rust) : UNE allocation tas par
# cellule franchissable, contenant tout le disque de rayon `d_neighborhood_m`
# (42 m par defaut). Le pic memoire est donc PROPORTIONNEL a la grille et
# QUADRATIQUE en `d_neighborhood / resolution` - pas une fuite (la memoire est
# rendue), un cout structurel.
#
# Mesures (grille synthetique, foretaccess 1.21.0, 2026-07-24) :
#   600x600 @ 5 m, d = 42 m (220 voisins) -> 1 537 Mo  soit 4,37 Ko/cellule
#   600x600 @ 5 m, d = 30 m (112 voisins) ->   841 Mo  soit 2,39 Ko/cellule
#   600x600 @ 5 m, d = 21 m ( 56 voisins) ->   493 Mo  soit 1,40 Ko/cellule
#   800x800 @ 5 m, d = 42 m               -> 2 520 Mo  soit 4,03 Ko/cellule
# Sans garde-fou, une emprise de ~10 km x 10 km a 5 m (~= 3,9 M cellules) demande
# ~17 Go et emporte la machine par OOM apres ~15 min de calcul (observe).

#' Number of disc offsets in the solver's extended neighbourhood
#'
#' Mirrors `build_offsets()` (foretaccess `src/rust/src/desserte/neighborhood.rs`):
#' every integer offset of the square `[-nb, nb]^2` except the centre, kept when
#' its planimetric distance is within `d_neighborhood`.
#'
#' @param d_neighborhood_m Neighbourhood radius in metres.
#' @param csize Cell size in metres.
#' @return Integer count of offsets.
#' @noRd
.desserte_n_offsets <- function(d_neighborhood_m = 42, csize = 5) {
  d <- suppressWarnings(as.numeric(d_neighborhood_m))
  cs <- suppressWarnings(as.numeric(csize))
  if (!is.finite(d) || !is.finite(cs) || d <= 0 || cs <= 0) return(0L)
  nb <- as.integer(d / cs + 0.5)
  if (nb < 1L) return(0L)
  g <- expand.grid(dr = -nb:nb, dc = -nb:nb)
  g <- g[!(g$dr == 0L & g$dc == 0L), , drop = FALSE]
  sum(sqrt(g$dr^2 + g$dc^2) * cs <= d)
}

#' Estimate the greedy engine's peak memory for a grid
#'
#' Per cell the solver holds: the neighbour list (`n_offsets * 16` bytes), the
#' `Vec` header (24), the `NodeState` search array (80), the heuristic grid (8)
#' and the id/coord tables (20). The 1.25 factor is the measured allocator
#' overhead (jemalloc), calibrated on the runs quoted above.
#'
#' @param n_cells Number of grid cells.
#' @param d_neighborhood_m Neighbourhood radius (m).
#' @param csize Cell size (m).
#' @return Estimated peak in bytes (numeric), or `NA_real_` on bad input.
#' @noRd
.desserte_memory_estimate <- function(n_cells, d_neighborhood_m = 42, csize = 5) {
  n <- suppressWarnings(as.numeric(n_cells))
  if (!is.finite(n) || n <= 0) return(NA_real_)
  n_off <- .desserte_n_offsets(d_neighborhood_m, csize)
  per_cell <- n_off * 16 + 24 + 80 + 8 + 20
  n * per_cell * 1.25
}

#' Grid cell count of an extent at a given resolution
#'
#' `buffer_m` widens the **bounding box**, which is all the grid depends on:
#' buffering the geometries (per feature or after a union) yields the exact same
#' box, so the caller never has to pay for `st_buffer()` just to size the grid.
#'
#' @param aoi An `sf`/`sfc` object; its bounding box drives the grid.
#' @param res_m Resolution in metres.
#' @param buffer_m Buffer in metres grown on every side of the box.
#' @return Number of cells (numeric), or `NA_real_`.
#' @noRd
.desserte_grid_cells <- function(aoi, res_m = 5, buffer_m = 0) {
  bb <- tryCatch(sf::st_bbox(aoi), error = function(e) NULL)
  r <- suppressWarnings(as.numeric(res_m))
  b <- suppressWarnings(as.numeric(buffer_m))
  if (!is.finite(b) || b < 0) b <- 0
  if (is.null(bb) || !is.finite(r) || r <= 0) return(NA_real_)
  w <- as.numeric(bb$xmax - bb$xmin) + 2 * b
  h <- as.numeric(bb$ymax - bb$ymin) + 2 * b
  if (!is.finite(w) || !is.finite(h) || w <= 0 || h <= 0) return(NA_real_)
  ceiling(w / r) * ceiling(h / r)
}

#' Memory currently available on the host, in bytes
#'
#' Reads `MemAvailable` from `/proc/meminfo` (Linux). Best-effort: returns
#' `NA_real_` elsewhere, which disables the guard rather than blocking a run.
#'
#' @return Available bytes (numeric) or `NA_real_`.
#' @noRd
.available_memory_bytes <- function() {
  if (!file.exists("/proc/meminfo")) return(NA_real_)
  lines <- tryCatch(readLines("/proc/meminfo", n = 60L), error = function(e) NULL)
  if (is.null(lines)) return(NA_real_)
  hit <- grep("^MemAvailable:", lines, value = TRUE)
  if (length(hit) == 0L) return(NA_real_)
  kb <- suppressWarnings(as.numeric(gsub("[^0-9]", "", hit[1])))
  if (!is.finite(kb)) return(NA_real_)
  kb * 1024
}

#' Pre-flight memory check for a desserte run
#'
#' Compares the estimated peak against a fraction of the available RAM. Set
#' `NEMETON_DESSERTE_SKIP_GUARD=1` to bypass (documented escape hatch: the
#' estimate is a model, the user may know better).
#'
#' @param aoi Extent to be analysed.
#' @param res_m Grid resolution (m).
#' @param d_neighborhood_m Neighbourhood radius (m).
#' @param frac Fraction of available RAM the run may claim.
#' @param buffer_m Buffer (m) to grow around `aoi` before sizing the grid; pass
#'   `0` when `aoi` is already the buffered extent.
#' @return A list with `ok`, `cells`, `bytes`, `available`.
#' @noRd
.desserte_memory_check <- function(aoi, res_m = 5, d_neighborhood_m = 42,
                                   frac = 0.8, buffer_m = 0) {
  cells <- .desserte_grid_cells(aoi, res_m, buffer_m)
  bytes <- .desserte_memory_estimate(cells, d_neighborhood_m, res_m)
  avail <- .available_memory_bytes()
  skip <- tolower(Sys.getenv("NEMETON_DESSERTE_SKIP_GUARD", "")) %in%
    c("1", "true", "yes", "oui")
  ok <- skip || !is.finite(bytes) || !is.finite(avail) || bytes <= avail * frac
  list(ok = isTRUE(ok), cells = cells, bytes = bytes, available = avail)
}

#' Reconstruct a run result from a project's cached desserte network
#'
#' Lets the tab show a **previously computed** network without recomputing (a
#' run is ~11.5 min): scans `cache/desserte/` for the network raster
#' (`reseau_<engine>.tif`) and its sidecar metadata (`reseau_<engine>.rds`,
#' holding the scalars - cost, connectedness, served parcels - that a raster
#' cannot carry) and rebuilds a minimal `run_desserte()` result marked
#' `from_cache = TRUE`. Returns `NULL` when the project has no cached network.
#'
#' @param project_path Project directory, or `NULL`.
#' @return A result list compatible with the map/badge UI, or `NULL`.
#' @noRd
#' Current values of the inputs that change the traced network
#'
#' Single source of truth for "what does the user ask for right now", used both
#' to launch a run and to decide whether a cached one still answers it. Falls
#' back to the documented defaults so a missing input never invalidates a cache
#' by accident.
#' @noRd
.desserte_params_courants <- function(input, skidding_m = NULL) {
  num <- function(x, defaut) {
    v <- suppressWarnings(as.numeric(x))
    if (!isTRUE(is.finite(v)) || v < 0) defaut else v
  }
  mp <- input$dess_methode_pente %||% "bareme"
  list(
    skidding_m = if (is.null(skidding_m)) {
      num(input$skidding_m, DESSERTE_SKIDDING_DEFAULT_M)
    } else skidding_m,
    methode_pente = if (mp %in% c("bareme", "terrassement")) mp else "bareme",
    largeur_m = num(input$dess_largeur, DESSERTE_LARGEUR_DEFAULT_M),
    pente_max_pct = num(input$dess_pente_max, DESSERTE_PENTE_MAX_DEFAULT_PCT))
}

#' Current desserte parameters, from the project settings
#'
#' @description
#' Since the five calibrations moved to the settings modal, the parameters no
#' longer come from module inputs but from `project_desserte_params()`. This
#' maps that list onto the input names [.desserte_params_courants()] expects, so
#' the sanitising (defaults on absent / negative / non-numeric) stays in one
#' place and the cache key keeps the same shape.
#'
#' @param params List from `project_desserte_params()`.
#' @return Same shape as [.desserte_params_courants()].
#' @noRd
.desserte_params_projet <- function(params) {
  .desserte_params_courants(list(
    skidding_m         = params$skidding_m,
    dess_methode_pente = params$methode_pente,
    dess_largeur       = params$largeur_m,
    dess_pente_max     = params$pente_max_pct))
}


.desserte_params_identiques <- function(meta, params) {
  for (nm in names(params)) {
    a <- meta[[nm]]
    b <- params[[nm]]
    # Un cache SANS le champ est anterieur a son introduction : on ne peut pas
    # affirmer qu'il a ete calcule avec la valeur demandee, donc il diverge.
    if (is.null(a) || is.null(b)) return(FALSE)
    ok <- if (is.character(b) || is.character(a)) {
      identical(as.character(a), as.character(b))
    } else {
      isTRUE(all.equal(as.numeric(a), as.numeric(b)))
    }
    if (!ok) return(FALSE)
  }
  TRUE
}

.load_cached_desserte <- function(project_path, params = NULL) {
  if (is.null(project_path) || !nzchar(project_path)) return(NULL)
  cache_dir <- .desserte_cache_dir(project_path)
  if (!dir.exists(cache_dir)) return(NULL)
  for (eng in DESSERTE_ENGINES) {
    rp <- file.path(cache_dir, paste0("reseau_", eng, ".tif"))
    if (!file.exists(rp)) next
    meta <- tryCatch(readRDS(file.path(cache_dir, paste0("reseau_", eng, ".rds"))),
                     error = function(e) list())
    # Un reseau trace AVANT `pondere_cout = TRUE` minimisait des metres, pas des
    # euros : ses traces ne sont pas comparables a ceux d'aujourd'hui. On le
    # traite comme absent plutot que de l'afficher comme un resultat courant -
    # l'utilisateur relance, ce qui est le seul moyen d'obtenir le bon trace.
    if (!isTRUE(meta$pondere_cout)) next
    # ...et tout parametre qui change le RESULTAT invalide de la meme facon.
    # Sans cette comparaison, changer `skidding_m` puis rouvrir l'onglet servait
    # le reseau precedent, calcule a l'ancienne distance - et le badge affichait
    # l'ancienne valeur, donc rien ne trahissait l'ecart.
    if (!is.null(params) && !.desserte_params_identiques(meta, params)) next
    gpkg <- file.path(cache_dir, "desserte.gpkg")
    return(list(
      status = "success",
      engine = eng,
      reseau_path = rp,
      gpkg_path = if (file.exists(gpkg)) gpkg else NULL,
      cout = meta$cout %||% NA_real_,
      connexe = meta$connexe %||% NA,
      raccorde = meta$raccorde %||% NA,
      n_desservies = meta$n_desservies %||% NA_integer_,
      n_parcelles = meta$n_parcelles %||% NA_integer_,
      n_routes = meta$n_routes %||% NA_integer_,
      skidding_m = meta$skidding_m %||% NA_real_,
      methode_pente = meta$methode_pente %||% NA_character_,
      largeur_m = meta$largeur_m %||% NA_real_,
      pente_max_pct = meta$pente_max_pct %||% NA_real_,
      integrite = .load_cached_integrite(cache_dir),
      from_cache = TRUE))
  }
  NULL
}

#' Run a road-network creation engine for a project (worker-side)
#'
#' Heavy, self-contained function meant to run in a `future` worker. Acquires
#' the **IGN RGE ALTI 5 m** DEM (HIGHRES, `.acquire_mnt_highres`), the existing
#' road network (**IGN BD TOPO V3**) and the forest mask (**IGN BD Foret V2**)
#' for the buffered AOI, runs `foretaccess::preprocess()` +
#' `surface_cout_construction()`, then the requested creation engine
#' (`reseau_desserte()`), writes the network class raster to
#' `cache/desserte/reseau_<engine>.tif` (+ a `.rds` sidecar with the scalars)
#' and an exportable GeoPackage (`parcelles` + `desserte_existante` +
#' `reseau_cree` layers). Returns only serialisable data.
#'
#' Best-effort and structured: every failure path returns
#' `list(status = "error", reason = ...)` instead of throwing.
#'
#' @param aoi_path GeoPackage path holding the parcels to serve (AOI). Passed as
#'   a FILE, never a live `sf` (external-pointer serialisation across the
#'   `future` boundary).
#' @param engine Character, one of `DESSERTE_ENGINES`.
#' @param cache_dir Destination directory for the artefacts (and DEM/road cache).
#' @param buffer_m Numeric buffer (m) grown around the AOI for the DEM and road
#'   acquisition: access to a stand comes from roads OUTSIDE it, and the trace
#'   solver needs the surrounding terrain. The parcels served stay the original
#'   AOI - only the analysed emprise widens.
#' @param skidding_m Skidding/forwarding distance (m) handed to
#'   `foretaccess::reseau_desserte()`. **Not a performance knob** - a business
#'   parameter that changes the result: a parcel cell within `skidding_m` of a
#'   road is already served and spawns no trace. Left at the core default `0`,
#'   *every* parcel cell off a road spawns its own A* trace, which the core
#'   documents as "both slow and over-connected": measured on Dabo
#'   (309 726 source cells) the run never finished in 22 min, against 70 s at
#'   300 m. See `DESSERTE_SKIDDING_DEFAULT_M`.
#' @return A named list describing the run.
#' @noRd
run_desserte <- function(aoi_path, engine, cache_dir, buffer_m = 0,
                         skidding_m = DESSERTE_SKIDDING_DEFAULT_M,
                         methode_pente = "bareme",
                         largeur_m = DESSERTE_LARGEUR_DEFAULT_M,
                         pente_max_pct = DESSERTE_PENTE_MAX_DEFAULT_PCT) {
  if (!requireNamespace("foretaccess", quietly = TRUE)) {
    return(list(status = "error", reason = "desserte_no_foretaccess"))
  }
  # Le coeur n'expose la tarification par terrassement ni le plafond de pente
  # que depuis la spec 029. Contre un coeur anterieur, on ne peut pas les
  # honorer -- et surtout on ne doit PAS retomber en silence sur le bareme :
  # l'utilisateur croirait chiffrer un volume de terre alors qu'il applique une
  # grille par classe de pente. On echoue donc explicitement, et seulement si la
  # demande sort du comportement historique.
  #
  # Le controle est ici, AVANT toute acquisition : refuser une demande qu'on ne
  # peut pas honorer apres plusieurs minutes de telechargement serait une
  # deuxieme faute.
  supporte <- "methode_pente" %in%
    names(formals(foretaccess::surface_cout_construction))
  if (!supporte &&
      (!identical(methode_pente, "bareme") ||
         !isTRUE(all.equal(as.numeric(pente_max_pct),
                           DESSERTE_PENTE_MAX_DEFAULT_PCT)))) {
    return(list(status = "error", reason = "desserte_core_trop_ancien"))
  }
  if (is.null(aoi_path) || !file.exists(aoi_path)) {
    return(list(status = "error", reason = "desserte_need_project"))
  }
  parcelles <- tryCatch(sf::st_read(aoi_path, quiet = TRUE), error = function(e) NULL)
  if (is.null(parcelles) || !inherits(parcelles, "sf") || nrow(parcelles) == 0L) {
    return(list(status = "error", reason = "desserte_need_project"))
  }
  engine <- intersect(engine, DESSERTE_ENGINES)[1]
  if (is.na(engine) || length(engine) == 0L) {
    return(list(status = "error", reason = "desserte_need_engine"))
  }

  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)

  # Tout le pipeline travaille en Lambert-93 (EPSG:2154) : preprocess() valide
  # l'egalite stricte des CRS (MNT vs vecteurs).
  epsg <- 2154L
  parcelles <- tryCatch(sf::st_transform(parcelles, epsg), error = function(e) parcelles)

  buffer_m <- suppressWarnings(as.numeric(buffer_m %||% 0))
  if (!is.finite(buffer_m) || buffer_m < 0) buffer_m <- 0
  aoi_ext <- parcelles
  if (buffer_m > 0) {
    aoi_ext <- tryCatch(sf::st_buffer(parcelles, buffer_m), error = function(e) parcelles)
  }
  acq_dir <- file.path(cache_dir, sprintf("emprise_%gm", buffer_m))
  dir.create(acq_dir, recursive = TRUE, showWarnings = FALSE)

  # Garde-fou memoire AVANT toute acquisition : le pic du glouton est connu a
  # partir de la seule emprise (cf. .desserte_memory_check). Sans ca, l'echec
  # arrive apres ~15 min de calcul, sous forme d'OOM qui emporte la machine -
  # pas d'une condition R rattrapable.
  mem <- .desserte_memory_check(aoi_ext, res_m = 5)
  if (!isTRUE(mem$ok)) {
    return(list(status = "error", reason = "desserte_memory_guard",
                detail = sprintf(
                  "grille %.1f M cellules, pic estime %.1f Go, RAM disponible %.1f Go",
                  mem$cells / 1e6, mem$bytes / 1024^3, mem$available / 1024^3),
                cells = mem$cells, bytes = mem$bytes, available = mem$available))
  }

  # Jalons de phase : le worker publie son avancement sur le canal disque, que
  # le module poll toutes les secondes (cf. DESSERTE_PHASES).
  .dess_write_phase(cache_dir, "mnt")
  # 1. MNT 5 m HIGHRES (repli acquire_mnt), partage avec l'accessibilite.
  mnt_path <- .acquire_mnt_highres(aoi_ext, res_m = 5, crs = epsg, cache_dir = acq_dir)
  if (is.null(mnt_path)) {
    mnt_path <- tryCatch(
      foretaccess::acquire_mnt(aoi_ext, res_m = 5, crs = epsg, cache_dir = acq_dir),
      error = function(e) structure(list(msg = conditionMessage(e)), class = "acc_err"))
  }
  if (inherits(mnt_path, "acc_err")) {
    return(list(status = "error", reason = "desserte_mnt_failed", detail = mnt_path$msg))
  }
  mnt <- tryCatch(terra::rast(mnt_path), error = function(e) NULL)
  if (is.null(mnt)) return(list(status = "error", reason = "desserte_mnt_failed"))

  .dess_write_phase(cache_dir, "desserte")
  # 2. Desserte EXISTANTE (reseau a raccorder) via IGN BD TOPO V3.
  desserte <- tryCatch(
    foretaccess::acquire_desserte(aoi_ext, crs = epsg, cache_dir = acq_dir),
    error = function(e) structure(list(msg = conditionMessage(e)), class = "acc_err"))
  if (inherits(desserte, "acc_err")) {
    return(list(status = "error", reason = "desserte_desserte_failed", detail = desserte$msg))
  }
  if (!inherits(desserte, "sf") || nrow(desserte) == 0L) {
    return(list(status = "error", reason = "desserte_desserte_empty"))
  }

  .dess_write_phase(cache_dir, "foret")
  # 3. Masque foret (IGN BD Foret V2 inter emprise), repli geometrie projet.
  foret_bd <- tryCatch(
    foretaccess::acquire_foret(aoi_ext, crs = epsg, cache_dir = acq_dir),
    error = function(e) NULL)
  foret_mask <- if (inherits(foret_bd, "sf") && nrow(foret_bd) > 0L) foret_bd else parcelles

  .dess_write_phase(cache_dir, "preprocess")
  # 4. Pretraitement commun (pente, franchissabilite, rasterisation).
  pre <- tryCatch(
    foretaccess::preprocess(mnt = mnt, desserte = desserte, foret = foret_mask),
    error = function(e) structure(list(msg = conditionMessage(e)), class = "acc_err"))
  if (inherits(pre, "acc_err")) {
    return(list(status = "error", reason = "desserte_preprocess_failed", detail = pre$msg))
  }

  .dess_write_phase(cache_dir, "cout")
  # 5. Surface de cout de construction (base + surcharge de pente ; couches eau/
  # sol optionnelles laissees a NULL en v1 - cf. plan de dev).
  cout <- tryCatch(
    if (supporte) {
      foretaccess::surface_cout_construction(pre, methode_pente = methode_pente,
                                             largeur_m = largeur_m,
                                             pente_max_pct = pente_max_pct)
    } else {
      foretaccess::surface_cout_construction(pre)
    },
    error = function(e) structure(list(msg = conditionMessage(e)), class = "acc_err"))
  if (inherits(cout, "acc_err")) {
    return(list(status = "error", reason = "desserte_cout_failed", detail = cout$msg))
  }

  .dess_write_phase(cache_dir, "moteur")
  # 6. Moteur de creation : GLOUTON (parcelles = AOI d'origine, reseau a
  # raccorder = desserte existante).
  # `skidding_m` : voir la doc du parametre. Sans lui (defaut coeur 0) le moteur
  # trace depuis CHAQUE cellule de parcelle hors route - 309 726 traces sur Dabo
  # au lieu de 4 parcelles - d'ou un calcul interminable ET un reseau
  # sur-connecte. Ce n'est pas un reglage de performance.
  skidding_m <- suppressWarnings(as.numeric(skidding_m)[1])
  if (!is.finite(skidding_m) || skidding_m < 0) {
    skidding_m <- DESSERTE_SKIDDING_DEFAULT_M
  }
  # `pondere_cout = TRUE` : le trace minimise des EUROS, pas des metres. Sans
  # lui, la surface de cout du Lot 14 - calculee juste au-dessus, phase " cout "
  # comprise - ne servait que par son masque `franchissable` : le solveur
  # tournait sur une grille neutre a 1,0 et rendait un trace purement
  # geometrique. On payait le calcul du cout sans jamais s'en servir.
  res <- tryCatch(
    foretaccess::reseau_desserte(pre, cout, parcelles = parcelles,
                                 desserte_existante = desserte, mode = engine,
                                 skidding_m = skidding_m, pondere_cout = TRUE),
    error = function(e) structure(list(msg = conditionMessage(e)), class = "acc_err"))
  if (inherits(res, "acc_err")) {
    return(list(status = "error", reason = "desserte_engine_failed", detail = res$msg))
  }

  # 7. Raster reseau -> disque. Normalisation defensive en masque 1 = route /
  # NA = reste : `is.na()` et `<=` sont des PRIMITIVES (dispatch S4 correct sans
  # import terra), contrairement a `%in%` (cf. fix hors_foret) - sur cote worker.
  reseau_path <- file.path(cache_dir, paste0("reseau_", engine, ".tif"))
  net <- tryCatch(
    terra::ifel(is.na(res$reseau) | res$reseau <= 0, NA, 1L),
    error = function(e) res$reseau)
  ok <- tryCatch({ terra::writeRaster(net, reseau_path, overwrite = TRUE); TRUE },
                 error = function(e) FALSE)
  if (!isTRUE(ok)) {
    return(list(status = "error", reason = "desserte_engine_failed",
                detail = "raster write failed"))
  }

  # Persiste l'OBJET `foretaccess_reseau` complet (pas seulement le raster) pour le
  # typage : `vectoriser_reseau()` l'exige, et le glouton est trop long pour etre
  # relance. Le `$reseau` (SpatRaster) porte un pointeur externe non serialisable :
  # on le `terra::wrap()` avant `saveRDS` (le typage l'`unwrap()`era).
  tryCatch({
    res_save <- res
    res_save$reseau <- terra::wrap(res$reseau)
    saveRDS(res_save, file.path(cache_dir, paste0("reseau_obj_", engine, ".rds")))
  }, error = function(e) cli::cli_warn(
    "desserte reseau object persist failed: {conditionMessage(e)}"))

  # Scalaires de badge (non portes par le raster) -> sidecar RDS pour le cache.
  # `raccorde` (foretaccess >= 1.11) est le VRAI indicateur qualite : " toutes les
  # routes creees sont-elles rattachees ? ". `connexe` (une seule composante pour
  # existant union cree) vaut presque toujours FALSE - domine par la fragmentation du
  # reseau EXISTANT a la resolution de la grille, pas par un defaut du reseau cree.
  n_parcelles <- nrow(parcelles)
  n_desservies <- suppressWarnings(sum(as.logical(res$desservies), na.rm = TRUE))
  # Nombre de routes CREEES. Zero est un resultat legitime - " le reseau
  # existant dessert deja tout " - et non un echec : a `skidding_m` realiste
  # c'est meme le cas nominal sur une foret bien desservie (mesure sur Dabo :
  # 39 routes a 100 m, aucune a 300 m). L'app doit pouvoir le DIRE, d'ou ce
  # compteur explicite plutot qu'un cout nul ambigu.
  n_routes <- if (inherits(res$lignes, "sf")) nrow(res$lignes) else 0L

  connexe <- isTRUE(res$connexe)
  raccorde <- if ("raccorde" %in% names(res)) isTRUE(res$raccorde) else NA
  cout_total <- suppressWarnings(as.numeric(res$cout))
  saveRDS(list(cout = cout_total, connexe = connexe, raccorde = raccorde,
               n_desservies = n_desservies, n_parcelles = n_parcelles,
               n_routes = n_routes, skidding_m = skidding_m,
               methode_pente = methode_pente, largeur_m = largeur_m,
               pente_max_pct = pente_max_pct,
               pondere_cout = TRUE),
          file.path(cache_dir, paste0("reseau_", engine, ".rds")))

  # 8. GeoPackage exportable : parcelles + desserte existante + reseau cree.
  gpkg_path <- file.path(cache_dir, "desserte.gpkg")
  unlink(gpkg_path)
  tryCatch({
    sf::st_write(sf::st_transform(parcelles, epsg), gpkg_path, layer = "parcelles",
                 quiet = TRUE, delete_dsn = TRUE)
    sf::st_write(sf::st_transform(desserte, epsg), gpkg_path, layer = "desserte_existante",
                 quiet = TRUE, append = TRUE)
    if (inherits(res$lignes, "sf") && nrow(res$lignes) > 0L) {
      sf::st_write(sf::st_transform(res$lignes, epsg), gpkg_path, layer = "reseau_cree",
                   quiet = TRUE, append = TRUE)
    }
  }, error = function(e) cli::cli_warn(
    "desserte GPKG write failed: {conditionMessage(e)}"))

  list(
    status = "success",
    engine = engine,
    reseau_path = reseau_path,
    gpkg_path = if (file.exists(gpkg_path)) gpkg_path else NULL,
    cout = cout_total,
    connexe = connexe,
    raccorde = raccorde,
    n_desservies = n_desservies,
    n_parcelles = n_parcelles,
    n_routes = n_routes,
    skidding_m = skidding_m)
}

#' Resolve the standing-volume (P1) column on a project's units
#'
#' The core and the app do NOT agree on the column name, which used to surface
#' as a false " volume P1 absent " error even on a fully computed project:
#'
#'   * `nemeton:::indicateur_p1_volume()` writes **`P1`** (its `column_name`
#'     default) - that is what a freshly computed `sf` carries in memory;
#'   * the project's `indicators.parquet` persists it as
#'     **`indicateur_p1_volume`**, aligned with the 30 other `indicateur_*`
#'     columns - and `.resolve_project_aoi_2154()` returns exactly that.
#'
#' Candidates are tried in order, then a case-insensitive match, and a column is
#' only accepted when it holds at least one finite value.
#'
#' @param parcelles An `sf`/data.frame of units.
#' @return The resolved column name, or `NULL` when no usable column exists.
#' @noRd
.resolve_volume_col <- function(parcelles) {
  if (!is.data.frame(parcelles) || ncol(parcelles) == 0L) return(NULL)
  nms <- names(parcelles)
  usable <- function(col) {
    v <- suppressWarnings(as.numeric(parcelles[[col]]))
    any(is.finite(v))
  }
  for (cand in c("P1", "indicateur_p1_volume")) {
    if (cand %in% nms && usable(cand)) return(cand)
  }
  hit <- nms[tolower(nms) %in% c("p1", "indicateur_p1_volume")]
  for (cand in hit) if (usable(cand)) return(cand)
  NULL
}

#' Default wood-flux thresholds for road typing (m3 total)
#'
#' Named ascending numeric vector of class lower bounds consumed by
#' `foretaccess::typer_desserte()`. Each troncon gets the highest class whose
#' accumulated flux bound it reaches.
#' @noRd
DESSERTE_TYPAGE_SEUILS <- c(tertiaire = 0, secondaire = 100, primaire = 500)

#' Type a project's created road network by mobilisable wood flux (worker-side)
#'
#' Chains `nemeton::volume_mobilisable()` (P1 -> mobilised volume) into the
#' `foretaccess` typing pipeline, per the spec-040 integration brief:
#'
#'   parcelles (+P1) -> volume_mobilisable(unite = "m3_total")   [nemeton]
#'                   -> calculer_flux(volume_champ = "volume_mobilisable")
#'                   -> typer_desserte(seuils_flux)                [foretaccess]
#'
#' **Unit trap (brief sect.3): `unite = "m3_total"` for typing** - `calculer_flux()`
#' distributes then accumulates a TOTAL m3 per parcel; an `m3_ha` density would
#' underestimate flux by a factor equal to the parcel area. (The `m3_ha` unit is
#' for weighting the glouton, a different consumer - not here.)
#'
#' Reuses the `foretaccess_reseau` object persisted by `run_desserte()` (the
#' glouton is too slow to re-run just to vectorise), so typing runs in seconds.
#' No app business logic (rules 1-3): two package calls.
#'
#' @param cache_dir The project's `cache/desserte` directory.
#' @param parcelles An `sf` of the parcels to serve, carrying a volume column.
#' @param taux_prelevement Numeric annual removal rate (voie " saisi ").
#' @param horizon_ans Numeric horizon in years.
#' @param engine Engine whose persisted network to type (default `"glouton"`).
#' @param seuils_flux Named ascending numeric vector of flux class bounds.
#' @param volume_col Name of the standing-volume column on `parcelles`, or
#'   `NULL` (default) to resolve it with `.resolve_volume_col()` - the core and
#'   the persisted project do NOT use the same name.
#' @return A named list: `status`, `recap` (length per type), `gpkg_path`,
#'   `seuils`, or an error list.
#' @noRd
run_desserte_typage <- function(cache_dir, parcelles, taux_prelevement,
                                horizon_ans, engine = "glouton",
                                seuils_flux = DESSERTE_TYPAGE_SEUILS,
                                volume_col = NULL) {
  if (!requireNamespace("foretaccess", quietly = TRUE) ||
      !requireNamespace("nemeton", quietly = TRUE)) {
    return(list(status = "error", reason = "desserte_typage_no_pkg"))
  }
  obj_path <- file.path(cache_dir, paste0("reseau_obj_", engine, ".rds"))
  if (!file.exists(obj_path)) {
    return(list(status = "error", reason = "desserte_typage_no_reseau"))
  }
  if (!inherits(parcelles, "sf") || nrow(parcelles) == 0L) {
    return(list(status = "error", reason = "desserte_typage_no_parcelles"))
  }
  # Resolution du nom de colonne : `P1` (sortie coeur en memoire) OU
  # `indicateur_p1_volume` (nom persiste dans indicators.parquet, et donc ce que
  # renvoie .resolve_project_aoi_2154). Chercher " P1 " en dur produisait un
  # " volume P1 absent " sur un projet pourtant entierement calcule.
  volume_col <- volume_col %||% .resolve_volume_col(parcelles)
  if (is.null(volume_col) || !volume_col %in% names(parcelles) ||
      !any(is.finite(suppressWarnings(as.numeric(parcelles[[volume_col]]))))) {
    return(list(status = "error", reason = "desserte_typage_no_volume"))
  }
  taux_prelevement <- suppressWarnings(as.numeric(taux_prelevement))
  horizon_ans <- suppressWarnings(as.numeric(horizon_ans))
  if (!is.finite(taux_prelevement) || !is.finite(horizon_ans) ||
      taux_prelevement <= 0 || horizon_ans <= 0) {
    return(list(status = "error", reason = "desserte_typage_bad_params"))
  }

  # Reseau persiste par run_desserte : unwrap du SpatRaster puis vectorisation.
  reseau <- tryCatch({
    r <- readRDS(obj_path); r$reseau <- terra::unwrap(r$reseau); r
  }, error = function(e) NULL)
  if (is.null(reseau)) {
    return(list(status = "error", reason = "desserte_typage_no_reseau"))
  }
  # Zero route NOUVELLE a creer n'est pas une panne : c'est le meilleur resultat
  # possible - le reseau existant dessert deja toutes les parcelles (constate
  # sur Couchey : `lignes` vide, 17 056 troncons existants, 76 UGF desservies
  # sur 76, cout 0). `vectoriser_reseau()` travaille sur `reseau$lignes` et
  # abandonne alors sur « Le reseau ne contient aucune route a vectoriser », que
  # l'onglet affichait en rouge - un bon resultat rapporte comme un echec.
  # On le distingue ici, AVANT l'appel, avec un statut propre.
  if (is.null(reseau$lignes) || nrow(reseau$lignes) == 0L) {
    return(list(status = "empty", reason = "desserte_typage_rien_a_typer"))
  }

  graphe <- tryCatch(foretaccess::vectoriser_reseau(reseau),
                     error = function(e) structure(list(msg = conditionMessage(e)),
                                                   class = "acc_err"))
  if (inherits(graphe, "acc_err")) {
    return(list(status = "error", reason = "desserte_typage_failed", detail = graphe$msg))
  }

  # Volume MOBILISE, unite m3_total (piege sect.3), voie " saisi " (taux + horizon).
  parc_vol <- tryCatch(
    nemeton::volume_mobilisable(parcelles, volume_col = volume_col,
                                unite = "m3_total",
                                taux_prelevement = taux_prelevement,
                                horizon_ans = horizon_ans),
    error = function(e) structure(list(msg = conditionMessage(e)), class = "acc_err"))
  if (inherits(parc_vol, "acc_err")) {
    return(list(status = "error", reason = "desserte_typage_volume_failed",
                detail = parc_vol$msg))
  }

  # Flux accumule puis typage par seuils.
  typee <- tryCatch({
    g <- foretaccess::calculer_flux(graphe, parc_vol,
                                    volume_champ = "volume_mobilisable")
    foretaccess::typer_desserte(g, seuils_flux = seuils_flux)
  }, error = function(e) structure(list(msg = conditionMessage(e)), class = "acc_err"))
  if (inherits(typee, "acc_err")) {
    return(list(status = "error", reason = "desserte_typage_failed", detail = typee$msg))
  }

  # Reseau type -> GeoPackage (cache), pour l'affichage carte et l'export.
  gpkg_path <- file.path(cache_dir, paste0("typage_", engine, ".gpkg"))
  ok <- tryCatch({
    unlink(gpkg_path)
    sf::st_write(sf::st_transform(typee$troncons, 2154), gpkg_path,
                 layer = "reseau_type", quiet = TRUE, delete_dsn = TRUE)
    TRUE
  }, error = function(e) FALSE)

  out <- list(
    status = "success",
    engine = engine,
    recap = typee$recap,
    gpkg_path = if (isTRUE(ok) && file.exists(gpkg_path)) gpkg_path else NULL,
    seuils = seuils_flux)
  # Sidecar, comme les quatre autres actions du panneau. Le typage etait le seul
  # a n'en avoir aucun : on rouvrait le projet, `typage_<moteur>.gpkg` etait bien
  # sur le disque, et l'onglet redemandait de typer le reseau.
  tryCatch(saveRDS(out, file.path(cache_dir, "typage.rds")),
           error = function(e) invisible(NULL))
  out
}

#' Read the persisted network-typing result
#'
#' The GeoPackage is checked again on read: a sidecar can outlive the cache it
#' points at. The recap table stays usable in that case, only the map layer goes.
#'
#' @param cache_dir Desserte cache directory.
#' @return The cached list, or `NULL` when absent.
#' @noRd
.load_cached_typage <- function(cache_dir) {
  f <- file.path(cache_dir, "typage.rds")
  if (!file.exists(f)) return(NULL)
  out <- tryCatch(readRDS(f), error = function(e) NULL)
  if (!is.list(out) || !identical(out$status, "success")) return(NULL)
  gp <- tryCatch(out$gpkg_path, error = function(e) NULL)
  if (is.null(gp) || !file.exists(gp)) out$gpkg_path <- NULL
  out
}

#' Export the desserte GeoPackage produced by a run
#'
#' Copies the cached `desserte.gpkg` (parcels, existing network, designed
#' network) then folds in whatever the other four actions of the panel left in
#' the cache: the typed network, the OSM acquisition and the detected roads.
#' Those three are the LONGEST work of the tab and used to never leave the
#' project cache - the download shipped only the creation run.
#'
#' Each extra layer is optional by construction: an action that never ran leaves
#' no file, and a missing file is skipped in silence. Written with
#' `append = FALSE`, which replaces the layer - **never** `delete_dsn`, which
#' would wipe the file just copied.
#'
#' @param result A `run_desserte()` result list.
#' @param file Destination path handed to the browser.
#' @param cache_dir Desserte cache directory. Defaults to the directory holding
#'   `result$gpkg_path`, which is that same cache.
#' @return Invisibly `TRUE`/`FALSE` - `TRUE` as soon as the base copy worked,
#'   the extras being best-effort.
#' @noRd
export_desserte_geopackage <- function(result, file, cache_dir = NULL) {
  src <- tryCatch(result$gpkg_path, error = function(e) NULL)
  if (is.null(src) || !file.exists(src)) return(invisible(FALSE))
  if (!isTRUE(tryCatch(file.copy(src, file, overwrite = TRUE),
                       error = function(e) FALSE))) {
    return(invisible(FALSE))
  }
  cache_dir <- cache_dir %||% dirname(src)

  # Le typage porte le moteur dans son nom : prendre CELUI DU RUN COURANT, pas
  # le premier venu - un projet peut porter `typage_glouton.gpkg` et
  # `typage_steiner.gpkg` cote a cote, et exporter le mauvais serait indetectable
  # a la lecture du fichier.
  eng <- tryCatch(as.character(result$engine), error = function(e) NULL)
  typage <- if (length(eng) == 1L && nzchar(eng)) {
    file.path(cache_dir, paste0("typage_", eng, ".gpkg"))
  } else NA_character_
  if (is.na(typage) || !file.exists(typage)) {
    typage <- Sys.glob(file.path(cache_dir, "typage_*.gpkg"))[1]
  }
  extras <- list(
    reseau_type       = typage,
    osm_track         = file.path(cache_dir, "desserte_osm.gpkg"),
    desserte_detectee = file.path(cache_dir, "desserte_detectee.gpkg"))

  for (lyr in names(extras)) {
    gp <- extras[[lyr]]
    if (length(gp) != 1L || is.na(gp) || !file.exists(gp)) next
    d <- tryCatch(sf::st_read(gp, layer = lyr, quiet = TRUE),
                  error = function(e) NULL)
    if (!inherits(d, "sf") || nrow(d) == 0L) next
    tryCatch(sf::st_write(d, file, layer = lyr, append = FALSE, quiet = TRUE),
             error = function(e) invisible(NULL))
  }
  invisible(TRUE)
}

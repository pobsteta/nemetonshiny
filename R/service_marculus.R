# service_marculus.R - export vers Marculus (martelage sur telephone Android)
#
# Marculus lit UN GeoPackage par contexte de martelage, et c'est le NOM DE LA
# TABLE qui dit le role de chaque couche, pas sa geometrie
# (`marculus/docs/specs/couches-gpkg.md`) :
#
#   `desserte`        -> routes, pistes et chemins, purement informatif
#   `houppier`        -> hauteur des tiges (h_max), non produit ici (cf. plus bas)
#   tout autre nom    -> PARCELLES, avec rattachement spatial des tiges
#
# La regle de repli est un piege autant qu'une commodite : une couche mal nommee
# devient une couche de parcelles, et chacune de ses entites devient candidate
# au rattachement des tiges. D'ou les noms figes en constantes ici plutot
# qu'ecrits au fil de l'eau.
#
# Les contextes eux-memes voyagent en JSON, via le meme format que le partage
# multi-operateurs (`.marsync`). Cote telephone c'est `fusionnerJson()` qui le
# lit : UNION PAR UUID, non destructive et atomique. Un fichier ne portant que
# des `contextes` cree donc les contextes sans toucher aux tiges deja saisies -
# c'est ce qui rend cet export sur : il n'ecrase rien.
#
# NON PRODUIT ICI : la couche `houppier`. Elle demande une segmentation de
# couronnes sur MNH (apex + delimitation), qui est de la LOGIQUE METIER et
# appartient donc a `nemeton` (regle 1). Brief a emettre ; le GPKG produit ici
# est valide sans elle, l'app telephone se contentant de ne pas estimer les
# hauteurs.


# ---- Constantes -------------------------------------------------------

#' Action types that designate stems, and so become a marking context
#'
#' @description
#' The criterion is the operator's gesture, not the silvicultural intent: an
#' action becomes a Marculus context when someone will walk the stand and
#' designate stems one by one. Thinning and clear-cut mark what leaves;
#' respacing marks what stays; an observation covers the inventory rounds,
#' which count stems the same way.
#'
#' Plantation, desserte or protection are deliberately absent: nothing is
#' designated stem by stem there, and an empty context on the phone is worse
#' than no context at all.
#'
#' @noRd
MARCULUS_CONTEXT_ACTION_TYPES <- c(
  "eclaircie", "coupe_rase", "depressage", "observation"
)

#' Table names Marculus recognises
#'
#' Anything not in this list is read as a parcel layer, tenement attachment
#' included. Never write a layer under a name that is not deliberate.
#'
#' @noRd
MARCULUS_LAYER_PARCELLES <- "parcelle"
MARCULUS_LAYER_DESSERTE  <- "desserte"
MARCULUS_LAYER_HOUPPIER  <- "houppier"

#' Field separators of the encoded `essences` column
#'
#' Marculus stores the marking sheet's columns in one string:
#' `nom US argb_fond US argb_texte`, records joined by RS. Same bytes as
#' `MartelageRepository.kt` (RS = 0x1E, US = 0x1F).
#'
#' @noRd
MARCULUS_RS <- "\u001E"
MARCULUS_US <- "\u001F"

#' Kanban states, app side to phone side
#'
#' The two vocabularies coincide exactly - five states, same order, same
#' meaning. Only the case differs (`EtatKanban.name` is upper case), which is
#' the whole of this mapping.
#'
#' @noRd
MARCULUS_STATUTS <- c(
  proposee    = "PROPOSEE",
  validee     = "VALIDEE",
  planifiee   = "PLANIFIEE",
  realisee    = "REALISEE",
  abandonnee  = "ABANDONNEE"
)


# ---- Contextes --------------------------------------------------------

#' Encode marking-sheet columns the way Marculus stores them
#'
#' @param essences Character vector of species labels. Empty is legitimate:
#'   the phone then shows an empty sheet, and the operator adds the columns.
#'   That is the honest default while the project carries no species layer.
#' @param fond,texte Integer ARGB colours applied to every column.
#' @return A length-1 character, `""` for no species.
#' @noRd
.marculus_encode_essences <- function(essences,
                                      fond = -1L, texte = -16777216L) {
  essences <- essences[!is.na(essences) & nzchar(essences)]
  if (length(essences) == 0L) return("")
  paste(
    vapply(essences, function(nom) {
      paste(nom, as.integer(fond), as.integer(texte), sep = MARCULUS_US)
    }, character(1)),
    collapse = MARCULUS_RS
  )
}

#' Human designation of a tenement group, for a context name
#'
#' @description
#' `ug_id` is an internal identifier - `ug_1`, or `ug_20260822203555_001` after
#' an ONF crossing. On a phone, a flat list of "Couchey - ug_20260822203555_001
#' - coupe_rase" cannot be navigated: the marker knows their **forest parcel**,
#' not the row it occupies in a table. The label is what the crossing already
#' wrote - "Forêt communale de Couchey — parcelle 1".
#'
#' The forest name is dropped when it repeats the project's, which it usually
#' does: "Couchey - Forêt communale de Couchey — parcelle 1 - coupe_rase" says
#' Couchey twice for no gain. What is kept is the part that distinguishes one
#' site from the next.
#'
#' Falls back to `ug_id` when the project carries no label - a legacy project
#' whose `ugs` is a bare character vector, or a group the crossing never named.
#' An identifier is poor, an empty middle is worse.
#'
#' @param project The loaded project.
#' @param ug_id Character. Tenement group identifier.
#' @param nom_projet Character. Project name, used to spot the repetition.
#' @return A length-1 character.
#' @noRd
.marculus_ug_label <- function(project, ug_id, nom_projet = NULL) {
  id <- as.character(ug_id %||% "?")
  ugs <- project$ugs
  if (!is.data.frame(ugs) || !all(c("ug_id", "label") %in% names(ugs))) return(id)

  lab <- as.character(ugs$label)[match(id, as.character(ugs$ug_id))]
  if (length(lab) != 1L || is.na(lab) || !nzchar(trimws(lab))) return(id)
  lab <- trimws(lab)

  # « Foret communale de Couchey - parcelle 1 » -> « parcelle 1 » quand le
  # projet s'appelle deja Couchey. Le separateur est le tiret cadratin que pose
  # le croisement ; sans lui, on garde le libelle entier.
  if (!is.null(nom_projet) && nzchar(nom_projet) &&
      grepl(nom_projet, lab, fixed = TRUE)) {
    morceaux <- strsplit(lab, "\u2014|\u2013| - ")[[1]]
    queue <- trimws(morceaux[length(morceaux)])
    if (length(morceaux) > 1L && nzchar(queue)) return(queue)
  }
  lab
}


#' Build the Marculus context of one action plan action
#'
#' @description
#' Every field `versContexte()` reads with a non-optional getter is emitted:
#' `id`, `nom`, `mode`, `classeMin`, `classeMax`, `classePas`, `increment`,
#' `exporte`, `dateCreation`. A missing one makes the phone-side import throw,
#' so they are written unconditionally rather than when convenient.
#'
#' `cheminGpkg` stays absent on purpose. It is a path in the phone's private
#' storage, which this side cannot know: the operator attaches the GeoPackage
#' to the context once, on the device.
#'
#' @param action One action of the plan.
#' @param project The loaded project (names the context, dates it).
#' @param essences Character vector of species for the marking sheet.
#' @param gpkg_nom File name of this context's GeoPackage inside the bundle,
#'   or `NULL`. Emitted as `gpkgNom`, an **unknown key** to every Marculus
#'   released so far - `JSONObject` ignores what it does not read, so the field
#'   is inert until the app learns to open a bundle
#'   (`specs/BRIEF-marculus-import-zip.md`). It is what would let the phone pair
#'   context and file by itself, instead of thirteen manual attachments.
#' @return A named list, ready for `jsonlite::toJSON()`.
#' @noRd
marculus_context_from_action <- function(action, project, essences = character(0),
                                         gpkg_nom = NULL) {
  nom_projet <- project$metadata$name %||% project$id
  ug <- .marculus_ug_label(project, action$ug_id, nom_projet)
  type <- action$type %||% "autre"
  libelle <- if (identical(type, "autre")) (action$type_libre %||% type) else type

  # Le nom porte l'UGF ET l'action : sur le telephone, la liste des contextes
  # est plate, et « Dabo » repete douze fois ne se navigue pas.
  nom <- sprintf("%s - %s - %s", nom_projet, ug, libelle)

  statut <- unname(MARCULUS_STATUTS[action$statut %||% "proposee"])
  if (is.na(statut) || is.null(statut)) statut <- "PROPOSEE"

  # Millisecondes depuis l'epoque : `getLong` cote Kotlin.
  now_ms <- round(as.numeric(Sys.time()) * 1000)

  # L'annee cible devient la date de martelage, faute de mieux : c'est la seule
  # date que porte une action. Un 1er janvier n'est pas une date de chantier -
  # l'operateur la corrigera - mais laisser le champ vide priverait la liste de
  # son tri, qui est par date de martelage decroissante.
  annee <- suppressWarnings(as.integer(action$annee_cible %||% NA))
  date_martelage <- if (!is.na(annee)) {
    round(as.numeric(as.POSIXct(sprintf("%d-01-01", annee), tz = "UTC")) * 1000)
  } else NULL

  ctx <- list(
    id           = action$id %||% paste0("act-", ug),
    nom          = nom,
    mode         = "CIRCONFERENCE",
    classeMin    = 20L,
    classeMax    = 200L,
    classePas    = 5L,
    essences     = .marculus_encode_essences(essences),
    increment    = 1L,
    exporte      = FALSE,
    dateCreation = now_ms,
    statut       = statut,
    modifie      = now_ms
  )
  cmt <- action$commentaire %||% ""
  if (nzchar(cmt)) ctx$commentaire <- cmt
  if (!is.null(date_martelage)) ctx$dateMartelage <- date_martelage
  # Nom de fichier NU, jamais un chemin : le lot est a plat, et un chemin
  # relatif ouvrirait la porte au zip-slip cote telephone.
  if (!is.null(gpkg_nom) && nzchar(gpkg_nom)) ctx$gpkgNom <- basename(gpkg_nom)
  ctx
}

#' Serialise contexts into the file the phone merges
#'
#' @description
#' Shaped like `exporterContexteJson()`: `version`, `contextes`, `tiges`,
#' `configs`. The last two are emitted **empty** and that is the point - the
#' phone merges by UUID, so an empty `tiges` adds nothing and removes nothing.
#'
#' `referentiels` is deliberately absent: `fusionnerJson()` ignores it (each
#' device keeps its own), and only `importerJson()` would read it - the
#' destructive entry point, which wipes contexts, stems and configs before
#' inserting. This file must never be handed to that one.
#'
#' @param contexts List of contexts from [marculus_context_from_action()].
#' @return A length-1 JSON character.
#' @noRd
marculus_sync_json <- function(contexts) {
  payload <- list(
    version   = 1L,
    contextes = contexts,
    tiges     = list(),
    configs   = list()
  )
  as.character(jsonlite::toJSON(payload, auto_unbox = TRUE, null = "null",
                                digits = NA, pretty = TRUE))
}


# ---- Couches vectorielles ---------------------------------------------

#' Commune name rather than its INSEE code
#'
#' The parcels carry `commune` filled with the **code** (`48042`), the cadastre
#' being keyed on it. Marculus displays that column as-is, and a five-digit
#' number tells a marker on the ground nothing. `commune_geometry` carries the
#' pair `code` / `nom`, cached at save time: it is the only place in the project
#' where the name lives.
#'
#' Falls back to the code when the name is missing - a legacy project has no
#' `commune_geometry`, and showing the code beats showing nothing.
#'
#' @param project The loaded project.
#' @param codes Character vector of commune codes, one per feature.
#' @return Character vector of the same length.
#' @noRd
.marculus_commune <- function(project, codes) {
  cg <- project$commune_geometry
  if (!inherits(cg, "sf") || !all(c("code", "nom") %in% names(cg))) return(codes)
  noms <- as.character(cg$nom)[match(as.character(codes), as.character(cg$code))]
  ifelse(is.na(noms) | !nzchar(noms), codes, noms)
}


#' Parcel layer of one action, in the shape Marculus reads
#'
#' @description
#' The perimeter is the action's **tenement**, not the whole project: a context
#' is one work site. Attributes follow the column names the phone looks for -
#' `proprietaire`, `foret`, `commune`, `section`, `numero` - taken from the
#' project's metadata and from the cadastral parcel each tenement comes from.
#'
#' Surface is not written: Marculus computes it from the geometry (geodesic
#' area) and reads no surface attribute.
#'
#' @param project The loaded project.
#' @param ug_id Character. The action's tenement group.
#' @return An `sf` of polygons, or `NULL` when the tenement is unknown.
#' @noRd
.marculus_parcelles <- function(project, ug_id) {
  ten <- project$tenements
  if (!inherits(ten, "sf") || nrow(ten) == 0L) return(NULL)
  if (!is.null(ug_id) && "ug_id" %in% names(ten)) {
    ten <- ten[!is.na(ten$ug_id) & ten$ug_id == ug_id, , drop = FALSE]
  }
  if (nrow(ten) == 0L) return(NULL)

  par <- project$parcels
  idx <- if (inherits(par, "sf") && "id" %in% names(par)) {
    match(ten$parent_parcelle_id, par$id)
  } else rep(NA_integer_, nrow(ten))

  pick <- function(col) {
    if (!inherits(par, "sf") || !(col %in% names(par))) return(NA_character_)
    as.character(par[[col]])[idx]
  }

  out <- sf::st_sf(
    proprietaire = project$metadata$owner %||% NA_character_,
    foret        = project$metadata$name  %||% NA_character_,
    commune      = .marculus_commune(project, pick("commune")),
    section      = pick("section"),
    numero       = pick("numero"),
    ug           = as.character(ten$ug_id %||% NA_character_),
    geometry     = sf::st_geometry(ten)
  )
  sf::st_make_valid(out)
}

#' Desserte layer of a project, folded into the single table Marculus reads
#'
#' @description
#' The tab produces four layers - `desserte_existante` (BD TOPO corrected),
#' `reseau_cree` (the tracks the engine designs), `osm_track` and
#' `desserte_detectee`. Marculus reads **one** table named `desserte`, so they
#' are concatenated rather than shipped side by side: on the phone the layer is
#' informative, it carries no calculation, and four ochre layers would say
#' nothing more than one.
#'
#' What survives the fold is the provenance, in `type`: an operator seeing a
#' track on screen needs to know whether it exists on the ground or is a
#' proposal on paper. That distinction is the only one worth a column here.
#'
#' When that tab never ran, the Accessibility run's own `desserte` serves as a
#' fallback - see `.marculus_desserte_accessibilite()`.
#'
#' @param project_id Character. Project identifier.
#' @return An `sf` of lines, or `NULL` when no desserte run left anything.
#' @noRd
.marculus_desserte <- function(project_id) {
  path <- get_project_path(project_id)
  if (is.null(path)) return(NULL)

  morceaux <- .marculus_desserte_onglet(path)
  # Repli sur l'onglet Accessibilite. Un projet peut n'avoir jamais ouvert
  # l'onglet Desserte et porter quand meme son reseau : `run_accessibility()`
  # acquiert la BD TOPO par le meme `foretaccess::acquire_desserte()` et la
  # range dans SON cache. C'est la meme desserte existante, a un repertoire
  # pres - la refuser ne protegeait rien, elle partait juste vide sur le
  # telephone. Repli et non union : quand les deux onglets ont tourne, la
  # couche de l'onglet Desserte redit la meme BD TOPO, corrigee en plus.
  if (length(morceaux) == 0L) morceaux <- .marculus_desserte_accessibilite(path)
  if (length(morceaux) == 0L) return(NULL)

  out <- do.call(rbind, morceaux)
  # Marculus accepte LINESTRING et MULTILINESTRING ; une desserte cartographiee
  # en surface serait contouree cote telephone. On ne lui envoie que des lignes.
  geom_ok <- as.character(sf::st_geometry_type(out)) %in%
    c("LINESTRING", "MULTILINESTRING")
  out <- out[geom_ok, , drop = FALSE]
  if (nrow(out) == 0L) NULL else out
}

#' The four layers the Desserte tab leaves in its own cache
#'
#' @param path Project root.
#' @return A list of `sf`, possibly empty.
#' @noRd
.marculus_desserte_onglet <- function(path) {
  cache <- file.path(path, "cache", "desserte")
  if (!dir.exists(cache)) return(list())

  sources <- list(
    list(gpkg = "desserte.gpkg",           layer = "desserte_existante", type = "existante"),
    list(gpkg = "desserte.gpkg",           layer = "reseau_cree",        type = "piste_creee"),
    list(gpkg = "desserte_osm.gpkg",       layer = "osm_track",          type = "osm"),
    list(gpkg = "desserte_detectee.gpkg",  layer = "desserte_detectee",  type = "detectee")
  )

  morceaux <- list()
  for (s in sources) {
    d <- .marculus_read_desserte(file.path(cache, s$gpkg), s$layer, s$type)
    if (!is.null(d)) morceaux[[length(morceaux) + 1L]] <- d
  }
  morceaux
}

#' The desserte the Accessibility run left behind, used as a fallback
#'
#' Same BD TOPO network, same acquisition, another cache directory. Typed
#' `existante` because that is what it is: acquired from the ground truth, not
#' designed by the engine.
#'
#' @param path Project root.
#' @return A list of at most one `sf`.
#' @noRd
.marculus_desserte_accessibilite <- function(path) {
  gp <- .accessibility_gpkg_path(path)
  d <- .marculus_read_desserte(gp, "desserte", "existante")
  if (is.null(d)) list() else list(d)
}

#' Read one desserte layer and reduce it to what Marculus reads
#'
#' @param gp Path to a GeoPackage, possibly absent or `NULL`.
#' @param layer Layer name.
#' @param type Provenance written into the `type` column.
#' @return An `sf` of `nom`/`type`/geometry in EPSG:4326, or `NULL`.
#' @noRd
.marculus_read_desserte <- function(gp, layer, type) {
  if (is.null(gp) || !file.exists(gp)) return(NULL)
  lyr <- tryCatch(sf::st_layers(gp)$name, error = function(e) character(0))
  if (!(layer %in% lyr)) return(NULL)
  d <- tryCatch(sf::st_read(gp, layer = layer, quiet = TRUE),
                error = function(e) NULL)
  if (!inherits(d, "sf") || nrow(d) == 0L) return(NULL)

  geom <- tryCatch(sf::st_geometry(sf::st_transform(d, 4326)),
                   error = function(e) NULL)
  if (is.null(geom)) return(NULL)

  nom <- if ("nom" %in% names(d)) as.character(d$nom) else NA_character_
  sf::st_sf(nom = nom, type = type, geometry = geom)
}


#' Where a project's segmented crowns live
#'
#' Beside the other cached layers, under their own directory: the crowns are a
#' derived product of the CHM, not a source, and they are rewritten whenever the
#' indicators are recomputed.
#'
#' @param project_id Character. Project identifier.
#' @return A path, or `NULL` when the project is unknown.
#' @noRd
.houppiers_cache_path <- function(project_id) {
  path <- get_project_path(project_id)
  if (is.null(path)) return(NULL)
  file.path(path, "cache", "layers", "houppiers", "houppiers.gpkg")
}

#' Segment the crowns once, at computation time, and cache them
#'
#' @description
#' Called at the end of `start_computation()`. The crowns serve no indicator -
#' they go into the Marculus bundle, where the phone reads `h_max` to pre-fill
#' the height of a marked stem.
#'
#' **Why here and not at download time.** Segmenting cost 173 s on Couchey, and
#' bounding the extent barely helps (162 s without) - the tile is read and
#' re-sampled either way. In a `downloadHandler` that is 173 s of frozen
#' session. Here, we are already inside the memory-capped child, after work that
#' counts in hours, and the CHM has just been produced.
#'
#' Best-effort throughout: a project with no CHM, a core without
#' `segment_houppiers()`, a segmentation that throws - none of these may fail a
#' computation that otherwise succeeded.
#'
#' @param project_id Character. Project identifier.
#' @return Invisibly the number of crowns written, `0` when nothing was.
#' @noRd
precompute_houppiers <- function(project_id) {
  if (!requireNamespace("nemeton", quietly = TRUE) ||
      !exists("segment_houppiers", envir = asNamespace("nemeton"),
              inherits = FALSE)) {
    return(invisible(0L))
  }
  out_path <- .houppiers_cache_path(project_id)
  chm <- .project_chm(project_id)
  if (is.null(out_path) || is.null(chm)) return(invisible(0L))

  projet <- load_project(project_id)
  aoi <- if (is.null(projet)) NULL else .marculus_aoi(projet)

  hp <- .marculus_segment_houppiers(chm, aoi)
  if (is.null(hp)) return(invisible(0L))

  dir.create(dirname(out_path), recursive = TRUE, showWarnings = FALSE)
  if (file.exists(out_path)) unlink(out_path)
  sf::st_write(hp, out_path, layer = "houppier", quiet = TRUE, driver = "GPKG")
  cli::cli_alert_success("Houppiers : {nrow(hp)} segment\u00e9s et mis en cache.")
  invisible(nrow(hp))
}

#' Crown layer of a project, when the core knows how to segment it
#'
#' @description
#' Pre-fills the **height** of a marked stem: on the phone, a point-in-polygon
#' on the GNSS position reads `h_max` and proposes it, modifiable. Without this
#' layer the GeoPackage stays valid - Marculus simply does not estimate.
#'
#' Computed **once per project**, not once per context. The core measured
#' 2 046 crowns in 11 s over 36.4 ha with a ~670 MB peak, so the whole extent is
#' cheaper than thirteen clipped runs, and every context then takes the crowns
#' its parcels cover.
#'
#' No `run_memory_capped()`: ~670 MB is 4 % of the ceiling. The capped child
#' costs an AOI to serialise and a progress channel to thread, and the memory
#' budget does not buy that here - unlike FORDEAD or reGénération.
#'
#' The export READS this cache, it never segments. A bundle must stay a matter
#' of seconds; the crowns are produced by [precompute_houppiers()] during the
#' indicator computation. A project computed before that existed simply has no
#' crown layer - the GeoPackage remains valid, the phone just does not pre-fill
#' heights.
#'
#' @param project_id Character. Project identifier.
#' @return An `sf` of polygons carrying `h_max`, or `NULL`.
#' @noRd
.marculus_houppiers <- function(project_id) {
  p <- .houppiers_cache_path(project_id)
  if (is.null(p) || !file.exists(p)) return(NULL)
  out <- tryCatch(sf::st_read(p, layer = "houppier", quiet = TRUE),
                  error = function(e) NULL)
  if (!inherits(out, "sf") || nrow(out) == 0L) return(NULL)
  if (!("h_max" %in% names(out))) return(NULL)
  out[, "h_max", drop = FALSE]
}

#' Segment crowns on a CHM, best-effort
#'
#' Bounded by the AOI: the cached CHM is a whole LiDAR HD tile, far larger than
#' the project. Measured on Couchey - 1 674 ha of tile for 536 ha of parcels -
#' segmenting everything yields 46 158 crowns against 22 435, most of the
#' surplus standing in forests belonging to somebody else. The time is much the
#' same either way, which is precisely why this runs at computation time.
#'
#' **Falls back to the whole tile when the bounded call fails.** On
#' `nemeton 0.184.0.9000` the `aoi` path aborts with
#' `st_crs(x) == st_crs(y) is not TRUE`, raised inside lidR once the CHM has
#' been cropped - reproduced twice, and not cured by handing the AOI in the
#' raster's own CRS (`specs/BRIEF-nemeton-houppiers-aoi-crs.md`). Segmenting
#' everything costs the same time and yields crowns standing in other people's
#' forests, but each context is clipped to its own parcels before writing, so
#' the surplus never reaches the phone. A feature that degrades beats a feature
#' that disappears.
#'
#' @param chm Path to the height model.
#' @param aoi Optional `sf` limiting the segmentation.
#' @return An `sf` in EPSG:4326 carrying `h_max`, or `NULL`.
#' @noRd
#' Outline of everything a project covers
#'
#' Union of the tenements, or of the parcels when no tenement exists yet.
#' Buffered by 10 m so a crown standing on the boundary is still segmented -
#' the marker walking the edge sees those trees too.
#'
#' @param project The loaded project.
#' @return An `sf` of one polygon, or `NULL`.
#' @noRd
.marculus_aoi <- function(project) {
  src <- if (inherits(project$tenements, "sf") && nrow(project$tenements) > 0L) {
    project$tenements
  } else if (inherits(project$parcels, "sf") && nrow(project$parcels) > 0L) {
    project$parcels
  } else NULL
  if (is.null(src)) return(NULL)
  tryCatch({
    u <- sf::st_union(sf::st_geometry(src))
    metrique <- sf::st_transform(u, 2154)
    sf::st_sf(geometry = sf::st_buffer(metrique, 10))
  }, error = function(e) NULL)
}


#' Segment crowns on a CHM, best-effort
#'
#' @description
#' Bounded by the AOI: the cached height model is a whole LiDAR HD tile, far
#' larger than the project - 1 169 ha of tiles for 637 ha of parcels on
#' "Fordead". The core keeps a boundary crown whole (`emprise = "intersecte"`),
#' so a tree straddling the edge is a tree, not a fraction of one.
#'
#' **A forced `max_cells` lived here from v0.140.0 to v0.141.0**, along with
#' `aoi = NULL`, because that was the only path measured as working: lidR
#' refused a raster left on disk, and shrinking the cell budget forced an
#' aggregation, which `terra` returns in memory. It cost resolution (0.50 m
#' worked at 2 m) and it cost the AOI. `nemeton 0.189.0` materialises the raster
#' itself, so both are given back - the call is a normal one again.
#'
#' @param chm A `SpatRaster`.
#' @param aoi The project outline, or `NULL`.
#' @return An `sf` of crowns carrying `h_max`, in WGS84, or `NULL`.
#' @noRd
.marculus_segment_houppiers <- function(chm, aoi = NULL) {
  out <- tryCatch(
    nemeton::segment_houppiers(chm, aoi = aoi),
    error = function(e) {
      cli::cli_warn("Segmentation des houppiers : {conditionMessage(e)}")
      NULL
    })
  if (!inherits(out, "sf") || nrow(out) == 0L) return(NULL)
  if (!("h_max" %in% names(out))) return(NULL)
  .marculus_to_4326(out[, "h_max", drop = FALSE])
}


#' Does this height model actually hold a canopy?
#'
#' @description
#' A model whose tallest pixel stands below `hmin` describes no tree, and
#' `segment_houppiers()` will rightly return nothing from it - after spending
#' minutes finding that out. The project "Fordead" is the case that forced this
#' check: its four Open-Canopy rasters are **flat**, every value between 0 and
#' 0.20 m, while the LiDAR HD height model sitting in the same cache has a
#' median of 20.7 m. Segmentation ran 142 s and returned 0 crowns, silently,
#' at the end of every indicator computation.
#'
#' Sampled rather than read whole: the rasters here run to hundreds of millions
#' of cells, and the question - "is there anything tall in there at all" - does
#' not need every one of them. A false negative costs what we already had (no
#' crowns); a false positive costs the two minutes this check exists to save.
#'
#' @param r A `SpatRaster`.
#' @param hmin Numeric. Minimum tree height, matching `segment_houppiers()`.
#' @return `TRUE` when at least one sampled cell reaches `hmin`.
#' @noRd
.chm_exploitable <- function(r, hmin = 5) {
  if (!inherits(r, "SpatRaster")) return(FALSE)
  v <- tryCatch(
    terra::spatSample(r, 1e5, method = "regular", na.rm = TRUE, warn = FALSE),
    error = function(e) NULL)
  if (is.null(v)) return(TRUE)   # doute : on laisse le coeur trancher
  v <- suppressWarnings(as.numeric(v[[1]]))
  v <- v[is.finite(v)]
  length(v) > 0L && max(v) >= hmin
}


#' Height model of a project, best source first
#'
#' @description
#' Delegates to `nemeton::resolve_project_chm()`, the canonical resolver - the
#' same one the sampling plan uses. It probes the project cache and **prefers
#' LiDAR HD over Open-Canopy**, which is both the better source and the higher
#' NDP.
#'
#' This function used to look **only** inside `cache/layers/opencanopy/`, taking
#' the first file that existed. On a project holding both, it therefore picked
#' the weaker of the two - and on "Fordead", where the Open-Canopy rasters came
#' out flat, it picked one with no canopy in it at all while twenty LiDAR HD
#' tiles sat unused next door.
#'
#' The Open-Canopy fallback is kept, and both candidates now have to pass
#' [.chm_exploitable()]: a height model with no height is not a
#' height model.
#'
#' Sert Marculus (segmentation des houppiers) ET le plan d'echantillonnage :
#' les deux ont besoin du meilleur modele de hauteur disponible. D'ou le nom
#' neutre : ce n'est pas un helper Marculus.
#'
#' **Depuis `nemeton` v0.192.2 le resolveur du cœur connait
#' `cache/layers/opencanopy/`** - le repertoire ou `download_chm_opencanopy()`
#' depose ses livrables - et le sonde fichier par fichier, apres le LiDAR HD
#' (ADR-007). La boucle ci-dessous ne comble donc plus un trou de *chemin* :
#' il ne lui reste que le repli **inter-sources** quand le candidat de tete est
#' plat, que le cœur ne sait pas encore faire (il rend le premier chemin qui
#' matche, sans regarder son contenu). Elle part des que
#' `resolve_project_chm(validate =)` est livre - annonce en v0.193.0 ;
#' `.chm_exploitable()` deviendra alors cet argument.
#'
#' @param project_id Character. Project identifier.
#' @return A `SpatRaster` - `segment_houppiers()` takes one directly - or
#'   `NULL` when no usable model exists.
#' @noRd
.project_chm <- function(project_id) {
  path <- get_project_path(project_id)
  if (is.null(path)) return(NULL)

  r <- tryCatch(nemeton::resolve_project_chm(path, verbose = FALSE),
                error = function(e) NULL)
  if (.chm_exploitable(r)) return(r)

  dir <- file.path(path, "cache", "layers", "opencanopy")
  for (f in c("chm_predicted_0_2m.tif", "chm_predicted_1_5m.tif", "chm.tif")) {
    p <- file.path(dir, f)
    if (!file.exists(p)) next
    rr <- tryCatch(terra::rast(p), error = function(e) NULL)
    if (.chm_exploitable(rr)) return(rr)
  }

  cli::cli_alert_info(
    "Houppiers : aucun mod\u00e8le de hauteur exploitable dans le cache du \
     projet (pas de LiDAR HD, et le mod\u00e8le Open-Canopy ne porte aucune \
     v\u00e9g\u00e9tation).")
  NULL
}

#' Reproject to WGS84, re-stamping a CRS that carries no authority block
#'
#' @description
#' The CHM of Couchey is named "EPSG:2154" without an authority block:
#' `sf::st_crs(x)$epsg` reads `NA` on anything derived from it. Written as-is,
#' the layer would leave with a CRS the phone cannot resolve - and Marculus
#' reprojects everything to WGS84 on read, so it would have nothing to reproject
#' *from*. The core warned about this: the defect is in the files, not in the
#' function that reads them.
#'
#' Lambert-93 is assumed only when the CRS is projected and metric, which is
#' what every French height model in this cache is.
#'
#' @param x An `sf`.
#' @return The same `sf` in EPSG:4326.
#' @noRd
.marculus_to_4326 <- function(x) {
  crs <- sf::st_crs(x)
  if (is.na(crs$epsg) && !is.na(crs$input) && grepl("2154", crs$input)) {
    sf::st_crs(x) <- 2154
  }
  if (is.na(sf::st_crs(x)$epsg) && is.na(sf::st_crs(x)$input)) return(x)
  tryCatch(sf::st_transform(x, 4326), error = function(e) x)
}


# ---- Ecriture ---------------------------------------------------------

#' Write the GeoPackage of one context
#'
#' @description
#' Vector only - no tile table. Marculus would take the first one as its
#' offline basemap, and the project's orthophotos weigh gigabytes: a basemap
#' that does not fit on the phone is not a basemap.
#'
#' @param project The loaded project.
#' @param action One action of the plan.
#' @param file Destination path.
#' @param desserte Optional `sf` of lines, shared across contexts of the same
#'   project (it is read once, not once per action).
#' @param houppiers Optional `sf` of crowns for the whole project; each context
#'   keeps those its parcels cover.
#' @return Invisibly `TRUE` when the parcel layer was written.
#' @noRd
marculus_write_action_gpkg <- function(project, action, file, desserte = NULL,
                                       houppiers = NULL) {
  par <- .marculus_parcelles(project, action$ug_id)
  if (is.null(par)) return(invisible(FALSE))

  if (file.exists(file)) unlink(file)
  sf::st_write(par, file, layer = MARCULUS_LAYER_PARCELLES, quiet = TRUE,
               driver = "GPKG")

  if (inherits(desserte, "sf") && nrow(desserte) > 0L) {
    tryCatch(
      sf::st_write(desserte, file, layer = MARCULUS_LAYER_DESSERTE,
                   append = FALSE, quiet = TRUE),
      error = function(e) {
        cli::cli_warn("Couche desserte non ecrite : {conditionMessage(e)}")
      }
    )
  }

  # Les houppiers du CHANTIER seulement : la couche du projet entier pese pour
  # rien dans les douze autres GeoPackages, et le telephone n'a que faire des
  # arbres d'une parcelle qu'il n'ouvrira pas.
  hp <- .marculus_clip_houppiers(houppiers, par)
  if (inherits(hp, "sf") && nrow(hp) > 0L) {
    tryCatch(
      sf::st_write(hp, file, layer = MARCULUS_LAYER_HOUPPIER,
                   append = FALSE, quiet = TRUE),
      error = function(e) {
        cli::cli_warn("Couche houppier non ecrite : {conditionMessage(e)}")
      }
    )
  }
  invisible(TRUE)
}

#' Keep the crowns a context's parcels cover
#'
#' Intersection, not clipping: a crown straddling the boundary keeps its whole
#' outline. Cutting it would move its centroid and shrink the polygon a stem
#' must fall into - the estimate would then miss the very trees at the edge.
#'
#' @param houppiers An `sf` of crowns, or `NULL`.
#' @param parcelles An `sf` of the context's parcels.
#' @return An `sf`, or `NULL`.
#' @noRd
.marculus_clip_houppiers <- function(houppiers, parcelles) {
  if (!inherits(houppiers, "sf") || nrow(houppiers) == 0L) return(NULL)
  if (!inherits(parcelles, "sf") || nrow(parcelles) == 0L) return(NULL)
  hit <- tryCatch(
    lengths(sf::st_intersects(houppiers, sf::st_union(parcelles))) > 0L,
    error = function(e) NULL)
  if (is.null(hit) || !any(hit)) return(NULL)
  houppiers[hit, , drop = FALSE]
}

#' Actions of a plan that become marking contexts
#'
#' @param plan The action plan.
#' @return A list of actions, possibly empty.
#' @noRd
marculus_eligible_actions <- function(plan) {
  actions <- plan$actions %||% list()
  Filter(function(a) {
    (a$type %||% "") %in% MARCULUS_CONTEXT_ACTION_TYPES
  }, actions)
}

#' Build the whole Marculus bundle of a project
#'
#' @description
#' One GeoPackage per eligible action plus one `.marsync` carrying every
#' context, zipped together. The stem-designating actions each become a work
#' site; the rest of the plan is not shipped.
#'
#' @param project_id Character. Project identifier.
#' @param file Destination `.zip` handed to the browser.
#' @param essences Character vector of species for the marking sheets. `NULL`
#'   (the default) reads them from the project's group profile.
#' @return Invisibly a list: `n_contexts`, `n_gpkg`, `n_essences`,
#'   `has_desserte`.
#' @noRd
marculus_export_bundle <- function(project_id, file, essences = NULL) {
  project <- load_project(project_id)
  plan    <- load_action_plan(project_id)
  vide    <- list(n_contexts = 0L, n_gpkg = 0L, has_desserte = FALSE)
  if (is.null(project) || is.null(plan)) return(invisible(vide))

  # La feuille de martelage part pre-remplie des essences du PROFIL DE GROUPE
  # du projet - ONF, CRPF, OFB ou generique ne martelent pas les memes. Un
  # appelant qui passe sa propre liste garde la main ; `character(0)` reste
  # possible et donne une feuille vide, que l'operateur remplit.
  if (is.null(essences)) {
    essences <- get_groupes_essences(project$metadata$groupes_profile)
  }

  actions <- marculus_eligible_actions(plan)
  if (length(actions) == 0L) return(invisible(vide))

  # Lues UNE fois : desserte et houppiers sont ceux du PROJET, pas de l'action.
  # Les houppiers sont LUS d'un cache produit au calcul des indicateurs - les
  # segmenter ici gelerait la session le temps du telechargement.
  desserte  <- .marculus_desserte(project_id)
  houppiers <- .marculus_houppiers(project_id)

  tmp <- file.path(tempdir(), paste0("marculus_", project_id))
  unlink(tmp, recursive = TRUE)
  dir.create(tmp, recursive = TRUE, showWarnings = FALSE)

  contexts <- list()
  n_gpkg <- 0L
  for (a in actions) {
    # Le nom du fichier se decide AVANT le contexte, puisque le contexte le
    # porte. Lisible - un operateur qui rattache a la main lit le nom du
    # chantier - et sans accent ni espace, pour traverser un ZIP et un systeme
    # de fichiers Android sans surprise.
    provisoire <- marculus_context_from_action(a, project, essences = essences)
    nom_gpkg <- paste0(gsub("[^A-Za-z0-9_-]+", "_", provisoire$nom), ".gpkg")

    ctx <- marculus_context_from_action(a, project, essences = essences,
                                        gpkg_nom = nom_gpkg)
    contexts[[length(contexts) + 1L]] <- ctx
    ok <- marculus_write_action_gpkg(project, a, file.path(tmp, nom_gpkg),
                                     desserte = desserte, houppiers = houppiers)
    if (isTRUE(ok)) n_gpkg <- n_gpkg + 1L
  }

  writeLines(marculus_sync_json(contexts),
             file.path(tmp, paste0(project$metadata$name %||% project_id, ".marsync")))

  utils::zip(zipfile = file, files = list.files(tmp, full.names = TRUE),
             flags = "-j9Xq")
  unlink(tmp, recursive = TRUE)

  invisible(list(n_contexts = length(contexts), n_gpkg = n_gpkg,
                 n_essences = length(essences),
                 has_desserte = !is.null(desserte),
                 n_houppiers = if (is.null(houppiers)) 0L else nrow(houppiers)))
}

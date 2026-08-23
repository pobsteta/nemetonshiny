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
#' @param project_id Character. Project identifier.
#' @return An `sf` of lines, or `NULL` when no desserte run left anything.
#' @noRd
.marculus_desserte <- function(project_id) {
  path <- get_project_path(project_id)
  if (is.null(path)) return(NULL)
  cache <- file.path(path, "cache", "desserte")
  if (!dir.exists(cache)) return(NULL)

  sources <- list(
    list(gpkg = "desserte.gpkg",           layer = "desserte_existante", type = "existante"),
    list(gpkg = "desserte.gpkg",           layer = "reseau_cree",        type = "piste_creee"),
    list(gpkg = "desserte_osm.gpkg",       layer = "osm_track",          type = "osm"),
    list(gpkg = "desserte_detectee.gpkg",  layer = "desserte_detectee",  type = "detectee")
  )

  morceaux <- list()
  for (s in sources) {
    gp <- file.path(cache, s$gpkg)
    if (!file.exists(gp)) next
    lyr <- tryCatch(sf::st_layers(gp)$name, error = function(e) character(0))
    if (!(s$layer %in% lyr)) next
    d <- tryCatch(sf::st_read(gp, layer = s$layer, quiet = TRUE),
                  error = function(e) NULL)
    if (!inherits(d, "sf") || nrow(d) == 0L) next

    nom <- if ("nom" %in% names(d)) as.character(d$nom) else NA_character_
    morceaux[[length(morceaux) + 1L]] <- sf::st_sf(
      nom      = nom,
      type     = s$type,
      geometry = sf::st_geometry(sf::st_transform(d, 4326))
    )
  }
  if (length(morceaux) == 0L) return(NULL)

  out <- do.call(rbind, morceaux)
  # Marculus accepte LINESTRING et MULTILINESTRING ; une desserte cartographiee
  # en surface serait contouree cote telephone. On ne lui envoie que des lignes.
  geom_ok <- as.character(sf::st_geometry_type(out)) %in%
    c("LINESTRING", "MULTILINESTRING")
  out <- out[geom_ok, , drop = FALSE]
  if (nrow(out) == 0L) NULL else out
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
#' Degrades in silence on a core that predates `segment_houppiers()`
#' (`nemeton 0.184.0`): the floor is not raised while that version is unreleased,
#' so the app must run on both.
#'
#' @param project_id Character. Project identifier.
#' @param aoi Optional `sf` limiting the segmentation. **Pass it**: the cached
#'   CHM is a whole LiDAR HD tile, far larger than the project. Measured on
#'   Couchey - 1 674 ha of tile for 536 ha of parcels - segmenting everything
#'   costs 162 s and 46 158 crowns, most of them in forests belonging to
#'   somebody else.
#' @return An `sf` of polygons carrying `h_max`, or `NULL`.
#' @noRd
.marculus_houppiers <- function(project_id, aoi = NULL) {
  if (!requireNamespace("nemeton", quietly = TRUE)) return(NULL)
  if (!exists("segment_houppiers", envir = asNamespace("nemeton"),
              inherits = FALSE)) {
    return(NULL)
  }
  chm <- .marculus_chm_path(project_id)
  if (is.null(chm)) return(NULL)

  out <- tryCatch(
    nemeton::segment_houppiers(chm, aoi = aoi),
    error = function(e) {
      cli::cli_warn("Segmentation des houppiers : {conditionMessage(e)}")
      NULL
    })
  if (!inherits(out, "sf") || nrow(out) == 0L) return(NULL)
  if (!("h_max" %in% names(out))) return(NULL)

  out <- out[, "h_max", drop = FALSE]
  .marculus_to_4326(out)
}

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

#' Height model of a project, highest resolution first
#'
#' The 0.20 m CHM is the one the pipeline writes last; the 1.5 m is its
#' predecessor and a fine enough input, the core re-sampling to 0.5 m anyway.
#'
#' @param project_id Character. Project identifier.
#' @return A path, or `NULL`.
#' @noRd
.marculus_chm_path <- function(project_id) {
  path <- get_project_path(project_id)
  if (is.null(path)) return(NULL)
  dir <- file.path(path, "cache", "layers", "opencanopy")
  for (f in c("chm_predicted_0_2m.tif", "chm_predicted_1_5m.tif", "chm.tif")) {
    p <- file.path(dir, f)
    if (file.exists(p)) return(p)
  }
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
  # L'emprise limite la segmentation aux parcelles : le CHM en cache est une
  # dalle LiDAR HD entiere, sans rapport avec le perimetre du projet.
  desserte  <- .marculus_desserte(project_id)
  houppiers <- .marculus_houppiers(project_id, aoi = .marculus_aoi(project))

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

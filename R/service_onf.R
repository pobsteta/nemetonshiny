#' ONF forest-parcel service (spec 046, spec 058 core)
#'
#' @description
#' Application-side wiring for the **ONF forest parcels** (the "parcellaire
#' forestier"). In public forests the *cadastral* parcel is not the management
#' unit: the *forest* parcel is, and it is the one materialised on the ground.
#' The core owns the whole acquisition (`nemeton::load_onf_parcelles_source()`)
#' and the whole crossing arithmetic (`nemeton::construire_ugf_onf()`, nemeton
#' >= 1.2.0); this file only turns their output into a project, so `mod_ug`
#' stays free of business logic (rules #1 and #2).
#'
#' One path: [onf_projet_croise()]. The cadastral parcels are never warped: the
#' ONF layer is rubber-sheeted onto them, then each cadastral parcel is cut
#' along the forest parcels and the pieces are grouped into UGF, each carrying
#' its ONF forest parcel in columns (`UG_ONF_COLS`).
#'
#' The former chain - the core's first crossing function, with the "whole parcel above
#' 90 %" snapping, `rattacher_reste`, and the purge on the forest share - was
#' removed on 2026-10-08 (brief `onf-nouveau-chemin-seul`), with no option to
#' go back to it.
#'
#' The WFS is reachable over **HTTP only**; every call therefore happens
#' server-side, never from the browser (mixed content would be blocked).
#'
#' @name service_onf
#' @keywords internal
NULL


#' Turn the ownership tick-boxes into the core's `domanialite` argument
#'
#' @description
#' The UI offers two tick-boxes - *domaniales* and *communales et autres* -
#' because "toutes" was only their conjunction, and a third way of saying the
#' same thing invites the user to wonder how it differs. The core still takes a
#' single string, so both ticked collapses back to `"toutes"`.
#'
#' Ticking neither is not "everything": it is a question with no object, and it
#' returns `NULL` so the caller can say so rather than fetch a parcel set nobody
#' asked for.
#'
#' @param x Character vector from the tick-boxes, or an already-resolved
#'   `"toutes"` / `"domaniale"` / `"autre"`.
#'
#' @return `"toutes"`, `"domaniale"`, `"autre"`, or `NULL`.
#'
#' @noRd
.onf_domanialite <- function(x) {
  x <- as.character(x %||% character(0))
  x <- x[!is.na(x) & nzchar(x)]
  # Valeur deja resolue (appel direct au service, tests).
  if (length(x) == 1L && x %in% c("toutes", "domaniale", "autre")) return(x)
  x <- intersect(x, c("domaniale", "autre"))
  if (length(x) == 0L) return(NULL)
  if (length(x) == 2L) return("toutes")
  x
}


#' Fetch the ONF forest parcels covering an area
#'
#' @description
#' Thin wrapper over `nemeton::load_onf_parcelles_source()` that turns the
#' core's two failure modes into one tagged result, so the module can branch on
#' `status` instead of re-deriving the distinction:
#'
#' * `"unavailable"` - the core returned `NULL`: network, service firewall, or
#'   unknown territory. The cadastral path stays available.
#' * `"empty"` - an `sf` with 0 row: the area simply holds no public forest.
#'   That is an answer, not an error.
#' * `"no_domanialite"` - neither tick-box is set: the question has no object.
#' * `"ok"` - parcels found.
#'
#' @param aoi `sf`/`sfc` with a defined CRS.
#' @param domanialite `"toutes"` (default), `"domaniale"` or `"autre"`. The
#'   filter is applied by the core.
#' @param max_parcelles Integer. Upper bound passed to the core.
#'
#' @return List with `status` (chr) and `parcelles` (`sf` or `NULL`).
#'
#' @noRd
onf_load_parcelles <- function(aoi,
                               domanialite = "toutes",
                               max_parcelles = 5000L,
                               clip_cadastre = FALSE) {
  if (is.null(aoi) || !inherits(aoi, c("sf", "sfc"))) {
    return(list(status = "no_aoi", parcelles = NULL))
  }
  domanialite <- .onf_domanialite(domanialite)
  if (is.null(domanialite)) {
    return(list(status = "no_domanialite", parcelles = NULL))
  }

  parcelles <- tryCatch(
    nemeton::load_onf_parcelles_source(
      aoi,
      domanialite   = domanialite,
      max_parcelles = as.integer(max_parcelles)
    ),
    error = function(e) {
      cli::cli_alert_warning("load_onf_parcelles_source: {conditionMessage(e)}")
      NULL
    }
  )

  # NULL = le service n'a pas repondu. Un sf a 0 ligne = il a repondu, et la
  # reponse est " pas de foret publique ici ". Deux messages differents.
  if (is.null(parcelles)) return(list(status = "unavailable", parcelles = NULL))
  if (nrow(parcelles) == 0L) return(list(status = "empty", parcelles = parcelles))

  if (isTRUE(clip_cadastre)) parcelles <- .onf_clip_cadastre(parcelles, aoi)
  if (nrow(parcelles) == 0L) return(list(status = "empty", parcelles = parcelles))

  list(status = "ok", parcelles = parcelles)
}

#' Cut the ONF parcellaire back to the project's cadastral parcels
#'
#' @description
#' The WFS answers on a bounding extent, so it returns forest that runs well
#' past the parcels one actually owns. The crossing already tiles on the
#' cadastre and is unaffected - what carried those fragments was the orange
#' preview layer and any export of the raw parcellaire, which showed forest
#' belonging to nobody's parcel and invited the question every time.
#'
#' A real intersection, not a filter: a forest parcel straddling the boundary
#' is **cut**, not dropped. Dropping it would hide the forest actually standing
#' on the parcel; keeping it whole would put back what this removes.
#'
#' @param onf An `sf` of forest parcels.
#' @param aoi The project's cadastral parcels.
#' @return The `sf`, cut. Unchanged when the intersection cannot be computed -
#'   a preview slightly too wide beats an empty map.
#' @noRd
.onf_clip_cadastre <- function(onf, aoi) {
  if (!inherits(onf, "sf") || nrow(onf) == 0L) return(onf)
  tryCatch({
    cad <- sf::st_union(sf::st_geometry(sf::st_transform(aoi, sf::st_crs(onf))))
    hit <- lengths(sf::st_intersects(onf, cad)) > 0L
    if (!any(hit)) return(onf[0L, , drop = FALSE])
    out <- suppressWarnings(sf::st_intersection(onf[hit, , drop = FALSE], cad))
    out <- out[!sf::st_is_empty(sf::st_geometry(out)), , drop = FALSE]
    if (nrow(out) == 0L) onf[0L, , drop = FALSE] else out
  }, error = function(e) {
    cli::cli_warn("Decoupe du parcellaire ONF : {conditionMessage(e)}")
    onf
  })
}


#' Label of a cadastral parcel that meets no forest parcel
#'
#' @description
#' Pascal's rule, 2026-08-26: a parcel listed in the CSV **is** the forest,
#' whether or not the ONF layer knows about it. It therefore keeps its own UGF,
#' named after the cadastral reference it does have - never merged into a
#' catch-all, which would put unrelated parcels in one unit of management.
#'
#' @param ref Character. Cadastral reference.
#' @param i18n Translator, or `NULL` for the raw fallback.
#' @return Character scalar.
#' @noRd
.onf_label_cadastrale <- function(ref, i18n = NULL) {
  fmt <- if (is.null(i18n)) "Parcelle cadastrale %s" else
    i18n$t("onf_ugf_cadastrale_fmt")
  sprintf(fmt, as.character(ref))
}


#' Labels of the UGF from the core's tenements
#'
#' @description
#' The label drives the UGF assignment of [tenement_import_replace()]: one
#' label, one UGF. It must therefore be a function of `ugf_id`, not of the row.
#'
#' * An ONF UGF takes the core's `nom_ugf` ("Foret communale de Couchey -
#'   parcelle 12").
#' * A `cad~<idu>` block is named after the cadastral reference in its
#'   `ugf_id`, dressed as "Parcelle cadastrale ...". **Not** after `nom_ugf`:
#'   the core puts on each `cad~` tenement the IDU of its own parcel, so a
#'   parcel attached to its neighbour's block (A 291 into `cad~...A0286` at
#'   Couchey) would carry another label and make a UGF of its own.
#' * Two different `ugf_id` sharing a name are told apart by their id, so the
#'   label never merges two UGF.
#'
#' @param ten `sf` of tenements from `nemeton::construire_ugf_onf()`.
#' @param i18n Translator, or `NULL`.
#' @return Character vector of labels, one per row.
#' @noRd
.onf_labels_ugf <- function(ten, i18n = NULL) {
  id  <- as.character(ten$ugf_id)
  lab <- as.character(ten$nom_ugf)
  cad <- !is.na(id) & startsWith(id, "cad~")
  if (any(cad)) {
    lab[cad] <- .onf_label_cadastrale(sub("^cad~", "", id[cad]), i18n)
  }
  vide <- is.na(lab) | !nzchar(lab)
  if (any(vide)) {
    ref <- if (!is.null(ten$idu)) ten$idu else id
    lab[vide] <- .onf_label_cadastrale(ref[vide], i18n)
  }
  # Meme nom pour deux UGF distinctes : l'identifiant les departage.
  paires <- unique(data.frame(id = id, lab = lab, stringsAsFactors = FALSE))
  doublons <- unique(paires$lab[duplicated(paires$lab)])
  if (length(doublons)) {
    k <- lab %in% doublons
    lab[k] <- sprintf("%s (%s)", lab[k], id[k])
  }
  lab
}


#' Run the core crossing over the whole project
#'
#' @description
#' One call to `nemeton::construire_ugf_onf()` for the whole project (nemeton
#' >= 1.2.0): with `cadastre` given, the core takes it as is, deduces the
#' communes from `code_insee` and warps the commune limits on both sides.
#'
#' @param cad `sf` of the project's parcels with `idu` and `code_insee`.
#' @param onf Raw `sf` of ONF parcels.
#' @param selection `"foret"` or `"toutes"`.
#' @param cfg Output of [project_onf_params()].
#' @return The core's `sf` (attributes `parcelles` and `calage`), an empty `sf`
#'   when nothing is kept, `NULL` when a source could not be reached.
#' @noRd
.onf_construire <- function(cad, onf, selection, cfg) {
  x <- nemeton::construire_ugf_onf(
    parcelles_onf    = onf,
    cadastre         = cad,
    selection        = selection,
    seuil_couverture = cfg$seuil_couverture,
    tol              = cfg$tol,
    larg_hors        = cfg$larg_hors,
    seuil            = cfg$seuil,
    seuil_hors       = cfg$seuil_hors
  )
  if (is.null(x)) return(NULL)
  p <- attr(x, "parcelles")
  if (inherits(p, "sf")) attr(x, "parcelles") <- sf::st_drop_geometry(p)
  x
}


#' Cross the ONF forest parcels with the project's cadastral parcels
#'
#' @description
#' Re-tiles the project's cadastral parcels into UGF numbered after the ONF
#' forest parcels, through `nemeton::construire_ugf_onf()` (nemeton >= 1.2.0):
#' rubber-sheeting of the ONF layer onto the cadastre (the cadastre never
#' moves), snapping of ONF limits closer than `tol` to a cadastral limit,
#' attachment of the pieces under `seuil` ha, and a `cad~<idu>` UGF for a block
#' outside the ONF parcels at least `larg_hors` wide and `seuil_hors` ha.
#'
#' **Selection.** With `selection = "foret"` (setting `purger`), the core keeps
#' only the parcels under the *regime forestier* - owned by a public person in
#' the DGFiP file AND covered at `seuil_couverture` by the warped ONF. The
#' others leave the project, parcels AND tenements, and are returned in
#' `ecartees` with their reason (`"privee"`, `"couverture"` or `"hors_onf"`) so
#' the user can take them back from the map. With `"toutes"`, the whole selection
#' is kept. The CSV path always uses `"toutes"`: a CSV lists the forest.
#'
#' **No new parcel.** The core is given the project's parcels as `cadastre`; it
#' never adds one.
#'
#' The ONF layer passed here must be the **raw** one, never the one cut by
#' `clip_cadastre`: the warping needs the overflowing ONF outline to pull it
#' back onto the cadastral limits.
#'
#' The tenements go through [tenement_import_replace()], which validates the
#' tiling; their ONF columns land on the UGF (`onf_foret_id`, `onf_foret_nom`,
#' `onf_parcelle`, `onf_domaniale`, `onf_part`).
#'
#' @param projet List. Project holding `$parcels`, `$tenements`, `$ugs`.
#' @param onf Raw `sf` of ONF parcels from [onf_load_parcelles()].
#' @param params Output of [project_onf_params()].
#' @param selection `"foret"`, `"toutes"`, or `NULL` to follow
#'   `params$purger`.
#' @param i18n Translator for the `cad~` labels, or `NULL`.
#'
#' @return List with `status` (`"ok"`, `"no_overlap"` or `"unavailable"`),
#'   `projet` (untouched unless `"ok"`), `tenements` (the core's table),
#'   `ecartees` (data.frame `idu`, `raison`, `proprietaire`, `couverture_onf`),
#'   `n_retenues`, `n_total` and `calage`.
#'
#' @noRd
onf_projet_croise <- function(projet,
                              onf,
                              params = ONF_PARAMS_DEFAULT,
                              selection = NULL,
                              i18n = NULL) {
  if (!has_ug_data(projet)) {
    cli::cli_abort("Project must have UG data. Run ug_init_default() first.")
  }
  parcelles <- projet$parcels
  if (is.null(parcelles) || !inherits(parcelles, "sf") || nrow(parcelles) == 0L) {
    cli::cli_abort("Project must have non-empty parcels sf object")
  }
  cfg <- project_onf_params(list(onf_params = params))
  selection <- selection %||% if (isTRUE(cfg$purger)) "foret" else "toutes"
  selection <- match.arg(selection, c("foret", "toutes"))

  # Le coeur attend `idu` ; le projet range l'IDU dans `id` (ou `nemeton_id`).
  id_col <- intersect(c("id", "nemeton_id", "geo_parcelle"), names(parcelles))[1]
  if (is.na(id_col)) {
    cli::cli_abort("Parcels layer has no id / nemeton_id / geo_parcelle column.")
  }
  cad <- parcelles
  cad$idu <- as.character(cad[[id_col]])
  # Commune : colonne du cadastre, sinon les 5 premiers caracteres de l'IDU.
  insee <- if ("code_insee" %in% names(cad)) as.character(cad$code_insee) else
    rep(NA_character_, nrow(cad))
  manque <- is.na(insee) | !nzchar(insee)
  insee[manque] <- substr(cad$idu[manque], 1L, 5L)
  cad$code_insee <- insee
  cad <- cad[, c("idu", "code_insee"), drop = FALSE]
  n_total <- nrow(cad)

  vide <- list(projet = projet, tenements = NULL,
               ecartees = .onf_ecartees_vide(), n_retenues = NA_integer_,
               n_total = n_total, calage = NULL)

  ten <- .onf_construire(cad, onf, selection, cfg)
  if (is.null(ten)) return(c(list(status = "unavailable"), vide))

  # " Aucun recoupement " : aucune ligne rattachee a une parcelle FORESTIERE.
  # En mode " toutes ", une emprise sans foret publique rend quand meme une UGF
  # `cad~` par parcelle ; l'appliquer detruirait le decoupage de l'utilisateur
  # pour rien.
  id <- as.character(ten$ugf_id)
  if (nrow(ten) == 0L || !any(!is.na(id) & !startsWith(id, "cad~"))) {
    vide$tenements <- ten
    return(c(list(status = "no_overlap"), vide))
  }

  # Parcelles du projet que le coeur n'a pas retenues.
  retenues <- unique(as.character(ten$idu))
  absentes <- setdiff(cad$idu, retenues)
  ecartees <- .onf_ecartees_vide()
  if (identical(selection, "foret")) {
    ecartees <- .onf_ecartees(absentes, attr(ten, "parcelles"))
    if (length(absentes)) {
      projet$parcels <- parcelles[!as.character(parcelles[[id_col]]) %in% absentes,
                                  , drop = FALSE]
      projet$tenements <- projet$tenements[
        !as.character(projet$tenements$parent_parcelle_id) %in% absentes, ,
        drop = FALSE]
    }
  } else if (length(absentes)) {
    # Filet : en mode " toutes " le coeur garde chaque parcelle. S'il en manquait
    # une, elle resterait sans tenement et le pavage casserait : elle devient sa
    # propre UGF cadastrale, entiere.
    reste <- sf::st_transform(cad[cad$idu %in% absentes, ], sf::st_crs(ten))
    ajout <- ten[rep(1L, nrow(reste)), , drop = FALSE]
    ajout$idu <- reste$idu
    ajout$ugf_id <- paste0("cad~", reste$idu)
    ajout$nom_ugf <- reste$idu
    for (col in intersect(c("foret_id", "foret_nom", "parcelle", "domaniale",
                            "part_onf"), names(ajout))) ajout[[col]] <- NA
    sf::st_geometry(ajout) <- sf::st_geometry(reste)
    ten <- rbind(ten, ajout)
  }

  # Colonnes ONF de l'UGF (brief 2026-10-08, sect. 2) ; `onf_part` est la part
  # du TENEMENT, que tenement_import_replace() moyenne par surface sur l'UGF.
  imp <- ten
  cadastrale <- startsWith(as.character(imp$ugf_id), "cad~")
  .col <- function(x) { x <- if (is.null(x)) rep(NA, nrow(imp)) else x; x[cadastrale] <- NA; x }
  imp$label_ugf     <- .onf_labels_ugf(imp, i18n)
  imp$onf_foret_id  <- as.character(.col(imp$foret_id))
  imp$onf_foret_nom <- as.character(.col(imp$foret_nom))
  imp$onf_parcelle  <- as.character(.col(imp$parcelle))
  imp$onf_domaniale <- as.logical(.col(imp$domaniale))
  imp$onf_part      <- suppressWarnings(as.numeric(.col(imp$part_onf)))

  projet <- tenement_import_replace(projet, imp)

  list(status = "ok", projet = projet, tenements = ten, ecartees = ecartees,
       n_retenues = length(retenues), n_total = n_total,
       calage = attr(ten, "calage"))
}


#' Whole ONF crossing, as run by the asynchronous worker
#'
#' @description
#' One WFS call over the whole selection, then [onf_projet_croise()]. About
#' 15 s per commune, more on the first call, which downloads the national DGFiP
#' file (376 MB, once): it runs in an `ExtendedTask` worker, never in the Shiny
#' session.
#'
#' The ONF layer is fetched **raw** and passed raw to the core: the warping
#' needs its overflow. `clip_cadastre` only shapes the preview layer `apercu`,
#' returned when the crossing did not apply so the user sees what was found.
#'
#' @param projet Project list (`$parcels`, `$tenements`, `$ugs`).
#' @param cfg Output of [project_onf_params()].
#' @param selection `"foret"`, `"toutes"` or `NULL` (follow `cfg$purger`).
#' @param lang `"fr"` or `"en"`, for the `cad~` labels.
#' @return The [onf_projet_croise()] list, or `list(status = )` when the ONF
#'   layer could not be read; `apercu` added when the status is not `"ok"`.
#' @noRd
onf_croise_tache <- function(projet, cfg, selection = NULL, lang = "fr") {
  res <- onf_load_parcelles(projet$parcels, domanialite = cfg$domanialite,
                            clip_cadastre = FALSE)
  if (!identical(res$status, "ok")) return(list(status = res$status))
  out <- onf_projet_croise(projet, res$parcelles, params = cfg,
                           selection = selection, i18n = get_i18n(lang))
  if (!identical(out$status, "ok")) {
    out$apercu <- if (isTRUE(cfg$clip_cadastre)) {
      .onf_clip_cadastre(res$parcelles, projet$parcels)
    } else res$parcelles
  }
  out
}


#' Build a project's parcels and UGF from a commune's public forest
#'
#' @description
#' Path A of spec 058: the user names a commune, the application finds the
#' cadastral parcels of its public forest and their UGF numbered after the ONF
#' forest parcels. Same chain as [onf_projet_croise()] with `selection =
#' "foret"`: the candidates are the commune's cadastral parcels that touch the
#' ONF layer, and the core keeps those owned by a public person (DGFiP) and
#' covered at `seuil_couverture` by the warped ONF.
#'
#' The cadastre comes from [get_cadastral_parcels()], the source of every
#' other project: the parcels stored are the ones the tenements tile, with the
#' same identifiers.
#'
#' Runs in an `ExtendedTask` worker (cadastre, WFS, DGFiP: 15 to 40 s). Writes
#' nothing: [onf_creer_projet_commune()] does.
#'
#' @param code_insee Character scalar, INSEE code of the commune.
#' @param params Output of [project_onf_params()]; `purger` is ignored (this
#'   path always selects the forest).
#' @param i18n Translator for the `cad~` labels, or `NULL`.
#' @return List with `status` (`"ok"`, `"cadastre"`, `"no_domanialite"`,
#'   `"unavailable"`, `"empty"` or `"no_overlap"`), `code_insee`, `commune`,
#'   `geometry` (commune boundary or `NULL`), and when `"ok"`: `projet`
#'   (`parcels`, `tenements`, `ugs`), `tenements` (the core's table),
#'   `ecartees` (sorted by decreasing ONF cover), `n_candidates`, `calage` and
#'   `nom` (name proposed for the project).
#' @noRd
onf_projet_depuis_commune <- function(code_insee, params = ONF_PARAMS_DEFAULT,
                                      i18n = NULL) {
  code_insee <- as.character(code_insee)[1]
  cfg <- project_onf_params(list(onf_params = params))
  out <- list(status = "cadastre", code_insee = code_insee, commune = NULL,
              geometry = NULL)

  geom <- tryCatch(get_commune_geometry(code_insee), error = function(e) NULL)
  out$geometry <- geom
  cad <- tryCatch(get_cadastral_parcels(code_insee, commune_geometry = geom),
                  error = function(e) NULL)
  if (is.null(cad) || !inherits(cad, "sf") || nrow(cad) == 0L) return(out)
  # Le cadastre range le code INSEE dans `commune` : le nom vient du
  # referentiel des communes du departement (en cache).
  communes <- tryCatch(get_communes_in_department(substr(code_insee, 1L, 2L)),
                       error = function(e) NULL)
  if (!is.null(communes) && all(c("code_insee", "nom") %in% names(communes))) {
    out$commune <- as.character(communes$nom[communes$code_insee == code_insee])[1]
  }

  aoi <- if (!is.null(geom)) geom else cad
  onf <- onf_load_parcelles(aoi, domanialite = cfg$domanialite,
                            clip_cadastre = FALSE)
  if (!identical(onf$status, "ok")) {
    out$status <- onf$status
    return(out)
  }

  # Candidates : les parcelles de la commune que le parcellaire ONF touche. Le
  # reste de la commune (plus d'un millier de parcelles a Sombernon) n'a rien
  # a faire dans le calage ni dans la liste des ecartees.
  o <- sf::st_union(sf::st_make_valid(
    sf::st_transform(onf$parcelles, sf::st_crs(cad))))
  touche <- lengths(suppressMessages(sf::st_intersects(cad, o))) > 0L
  if (!any(touche)) {
    out$status <- "no_overlap"
    return(out)
  }
  projet <- ug_init_default(list(parcels = cad[touche, ]))

  res <- onf_projet_croise(projet, onf$parcelles, params = cfg,
                           selection = "foret", i18n = i18n)
  out$status <- res$status
  if (!identical(res$status, "ok")) return(out)

  ecartees <- res$ecartees
  if (nrow(ecartees)) {
    ecartees <- ecartees[order(-ecartees$couverture_onf, na.last = TRUE), ,
                         drop = FALSE]
    rownames(ecartees) <- NULL
  }
  noms <- as.character(stats::na.omit(res$tenements$foret_nom))
  nom <- if (length(noms)) names(sort(table(noms), decreasing = TRUE))[1] else
    out$commune %||% code_insee

  c(out[c("status", "code_insee", "commune", "geometry")],
    list(projet = res$projet[c("parcels", "tenements", "ugs")],
         tenements = res$tenements, ecartees = ecartees,
         n_candidates = sum(touche), calage = res$calage, nom = nom))
}


#' Create a project from [onf_projet_depuis_commune()]
#'
#' Writes the project (parcels, commune boundary) then its tenements and UGF.
#' The current project is not touched.
#'
#' @param res An `"ok"` result of [onf_projet_depuis_commune()].
#' @param nom Project name; defaults to `res$nom`.
#' @return The new project id.
#' @noRd
onf_creer_projet_commune <- function(res, nom = NULL) {
  if (!identical(res$status, "ok")) {
    cli::cli_abort("Nothing to create: status {.val {res$status}}.")
  }
  nom <- trimws(nom %||% res$nom)
  pr <- create_project(name = nom, parcels = res$projet$parcels,
                       commune_geometry = res$geometry)
  save_ug_data(pr$id, res$projet)
  pr$id
}


#' Discarded parcels, as a sentence fragment
#'
#' @param ecartees data.frame from [.onf_ecartees()].
#' @param i18n Translator.
#' @param seuil Minimum cover, a share (for the wording only).
#' @param max_n Integer. Parcels listed before an ellipsis.
#' @return Character scalar, e.g. "212000000A0283 (couverture ONF 2 %, COMMUNE
#'   DE COUCHEY), ...".
#' @noRd
onf_ecartees_texte <- function(ecartees, i18n, max_n = 10L) {
  if (is.null(ecartees) || nrow(ecartees) == 0L) return("")
  raison <- vapply(seq_len(nrow(ecartees)), function(i) {
    couv <- suppressWarnings(as.numeric(ecartees$couverture_onf[i]))
    txt_couv <- if (length(couv) == 1L && !is.na(couv)) {
      sprintf(i18n$t("onf_raison_couverture"), as.integer(round(100 * couv)))
    } else NULL
    switch(ecartees$raison[i],
      # Une parcelle privee que l'ONF couvre presque entierement est celle que
      # l'utilisateur doit voir : sa couverture est dite, pas seulement son
      # statut.
      privee     = paste(c(i18n$t("onf_raison_privee"),
                           if (!is.null(couv) && isTRUE(couv >= 0.01)) txt_couv),
                         collapse = ", "),
      couverture = txt_couv %||% sprintf(i18n$t("onf_raison_couverture"), 0L),
      i18n$t("onf_raison_hors_onf"))
  }, character(1))
  prop <- ecartees$proprietaire
  detail <- ifelse(is.na(prop) | !nzchar(prop), raison, paste0(raison, ", ", prop))
  items <- sprintf("%s (%s)", ecartees$idu, detail)
  if (length(items) > max_n) {
    items <- c(utils::head(items, max_n), "\u2026")
  }
  paste(items, collapse = " ; ")
}


#' Empty table of discarded parcels
#' @noRd
.onf_ecartees_vide <- function() {
  data.frame(idu = character(0), raison = character(0),
             proprietaire = character(0), couverture_onf = numeric(0),
             stringsAsFactors = FALSE)
}

#' Discarded parcels with their reason
#'
#' @param absentes Character. IDU of the project's parcels the core did not
#'   keep.
#' @param candidates The core's `parcelles` attribute (`idu`, `raison`,
#'   `proprietaire`, `couverture_onf`...), or `NULL`.
#' @return data.frame `idu`, `raison` (`"privee"`, `"couverture"` or
#'   `"hors_onf"`), `proprietaire`, `couverture_onf`.
#' @noRd
.onf_ecartees <- function(absentes, candidates) {
  if (!length(absentes)) return(.onf_ecartees_vide())
  out <- data.frame(idu = absentes, raison = "hors_onf",
                    proprietaire = NA_character_, couverture_onf = NA_real_,
                    stringsAsFactors = FALSE)
  if (!is.null(candidates) && "idu" %in% names(candidates)) {
    k <- match(absentes, as.character(candidates$idu))
    vu <- !is.na(k)
    if (any(vu)) {
      r <- as.character(candidates$raison %||% rep(NA, nrow(candidates)))[k[vu]]
      out$raison[vu] <- ifelse(!is.na(r) & r == "privee", "privee",
                        ifelse(!is.na(r) & r == "hors ONF", "hors_onf", "couverture"))
      if (!is.null(candidates$proprietaire)) {
        out$proprietaire[vu] <- as.character(candidates$proprietaire)[k[vu]]
      }
      if (!is.null(candidates$couverture_onf)) {
        out$couverture_onf[vu] <- suppressWarnings(
          as.numeric(candidates$couverture_onf))[k[vu]]
      }
    }
  }
  out
}


#' Summarise a crossing for the user
#'
#' @description
#' Everything below is read off the core's return; nothing is recomputed
#' (rule: the core already did the arithmetic).
#'
#' @param ten Tenement table from `nemeton::construire_ugf_onf()`.
#'
#' @return List with `n_ugf`, `n_parcelles`, `n_multi` (UGF spanning several
#'   cadastral parcels) and `n_cad` (`cad~` UGF, outside the ONF parcels).
#'
#' @noRd
onf_croise_resume <- function(ten) {
  vide <- list(n_ugf = 0L, n_parcelles = 0L, n_multi = 0L, n_cad = 0L)
  if (is.null(ten) || nrow(ten) == 0L) return(vide)
  ugf <- as.character(ten$ugf_id)
  idu <- as.character(ten$idu)
  list(
    n_ugf       = length(unique(ugf)),
    n_parcelles = length(unique(idu)),
    n_multi     = sum(tapply(idu, ugf, function(x) length(unique(x))) > 1L),
    n_cad       = length(unique(ugf[startsWith(ugf, "cad~")]))
  )
}


#' Vectorised isTRUE
#'
#' `isTRUE()` is scalar-only; a logical column with NA needs element-wise
#' handling, and `NA` must read as FALSE rather than propagate.
#' @noRd
.isTRUE_vec <- function(x) {
  out <- as.logical(x)
  !is.na(out) & out
}

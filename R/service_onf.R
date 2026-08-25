#' ONF forest-parcel service (spec 046)
#'
#' @description
#' Application-side wiring for the **ONF forest parcels** (the "parcellaire
#' forestier"). In public forests the *cadastral* parcel is not the management
#' unit: the *forest* parcel is, and it is the one materialised on the ground.
#' The core owns the whole acquisition (`nemeton::load_onf_parcelles_source()`)
#' and the whole crossing arithmetic (`nemeton::croiser_parcelles_onf()`); this
#' file only turns their output into a project, so `mod_ug` stays free of
#' business logic (rules #1 and #2).
#'
#' One path: [onf_projet_croise()]. The cadastral selection is **kept** (it is
#' the user's property) and each UGF is described as the pieces of cadastral
#' parcels it is made of.
#'
#' A second path existed until v0.130.0.9001 - `onf_projet_from_parcelles()`,
#' which had the forest parcels *replace* the cadastral ones. It was removed
#' rather than kept: fed the same area, it produced the same UGF while throwing
#' away the cadastral composition (hence `part_ugf`, the "you only hold 40 % of
#' that forest parcel"). A lossy special case of the crossing, and a destructive
#' one.
#'
#' The WFS is reachable over **HTTP only**; every call therefore happens
#' server-side, never from the browser (mixed content would be blocked).
#'
#' @name service_onf
#' @keywords internal
NULL


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


#' Label carried by the tenements no forest parcel covers
#'
#' @description
#' `croiser_parcelles_onf(inclure_reste = TRUE)` returns those rows with
#' `ugf_id` and `nom_ugf` at `NA`. They still need a label: they become real
#' tenements, and a tenement without a UGF violates invariant 2.
#'
#' @param i18n Translator, or `NULL` for the raw key fallback.
#' @return Character scalar.
#' @noRd
.onf_label_hors_ugf <- function(i18n = NULL) {
  if (is.null(i18n)) return("Hors for\u00eat publique")
  i18n$t("onf_hors_ugf_label")
}


#' Cross the ONF forest parcels with the selected cadastral parcels
#'
#' @description
#' Keeps the cadastral parcels and re-tiles them by UGF: one tenement per
#' (UGF x cadastral parcel).
#'
#' **Why `inclure_reste = TRUE` is not optional here.** The brief's own sketch
#' loops over `tenement_split_by_import()`, which recreates the uncovered
#' remainder itself. We do not take that path (see below), and
#' [tenement_import_replace()] replaces the whole tenement layer without
#' recreating anything: without the remainder rows, the parts of the selection
#' that hold no public forest would simply lose their tenements, and the parcels
#' would stop being exactly tiled. So we ask the core for them and label them.
#'
#' **Why `tenement_import_replace()` rather than a loop over
#' `tenement_split_by_import()`.** The sketched loop calls a
#' `tenement_ids_created()` helper that does not exist, and
#' `tenement_split_by_import()` mints its ids from `Sys.time()` at second
#' resolution - two parcels split inside the same second produce *colliding*
#' `tenement_id`s, which `projet_validate()` does not catch. A single
#' `tenement_import_replace()` call does the whole job instead: it derives each
#' parent parcel by largest overlap, drives the UGF assignment from a
#' `label_ugf` column (reusing an existing UGF of the same label, so its
#' `groupe` survives), mints ids once for the whole set, drops the UGF left
#' empty and validates the invariants.
#'
#' Nothing is filtered or recomputed here: `min_surface_ha` absorption, the
#' cadastral snapping AND the discarding of parcels that meet no forest parcel
#' all happen in the core (the last one since `nemeton 0.180.0`). The core's two guards stay
#' whole - a parcel genuinely shared between two UGF is NOT snapped, and the
#' "outside UGF" remainder can never take a parcel.
#'
#' @param projet List. Project holding `$parcels`, `$tenements`, `$ugs`.
#' @param onf `sf` of forest parcels from [onf_load_parcelles()].
#' @param caler_sur_cadastre Logical. Snap UGF boundaries onto cadastral ones.
#'   `TRUE` by default since v0.130.1.9001: the ONF boundaries are approximate
#'   at the edge, so a UGF whose border does not follow a parcel it covers at
#'   90 % or more is a digitising artefact, not a management decision. The
#'   parameter stays, so the raw behaviour remains reachable and testable.
#' @param seuil_calage Numeric. Share above which a parcel is taken whole.
#' @param label_hors Character. Label for the non-forest remainder.
#'
#' @return List with `status` (`"ok"` or `"no_overlap"`), `projet` and the
#'   crossing table `tenements` (for the user-facing summary).
#'
#' @noRd
onf_projet_croise <- function(projet,
                              onf,
                              caler_sur_cadastre = TRUE,
                              seuil_calage = 0.9,
                              label_hors = .onf_label_hors_ugf()) {
  if (!has_ug_data(projet)) {
    cli::cli_abort("Project must have UG data. Run ug_init_default() first.")
  }
  parcelles <- projet$parcels
  if (is.null(parcelles) || !inherits(parcelles, "sf") || nrow(parcelles) == 0L) {
    cli::cli_abort("Project must have non-empty parcels sf object")
  }

  # Le COEUR ecarte lui-meme (>= 0.180.0) les parcelles cadastrales qu'aucune
  # parcelle forestiere ne rencontre, et emet directement leur ligne `hors_ugf`.
  # L'app faisait ce tri en v0.130.3, avec une reinjection qui imposait un
  # aller-retour de projection - d'ou 0,001231 % d'ecart de pavage. Fait dans le
  # coeur, ou les deux couches partagent deja un CRS, le pavage redevient exact.
  ten <- nemeton::croiser_parcelles_onf(
    onf,
    parcelles,
    caler_sur_cadastre = isTRUE(caler_sur_cadastre),
    seuil_calage       = seuil_calage,
    # Cf. la doc ci-dessus : le reliquat porte le pavage exact, il n'est pas
    # une option d'affichage.
    inclure_reste      = TRUE
  )

  # Compteur " N parcelles sur M " LU sur l'attribut plutot que recalcule : le
  # coeur l'expose precisement pour eviter un st_intersects() de plus.
  pc <- attr(ten, "parcelles_concernees")
  n_retenues <- suppressWarnings(as.integer(pc[["concernees"]]))
  n_total    <- suppressWarnings(as.integer(pc[["total"]]))
  if (length(n_total) != 1L || is.na(n_total)) n_total <- nrow(parcelles)
  if (length(n_retenues) != 1L) n_retenues <- NA_integer_

  # " Aucun recoupement " ne peut PAS se lire sur nrow(ten) : avec
  # `inclure_reste = TRUE`, une emprise sans la moindre foret publique rend
  # quand meme une ligne par parcelle cadastrale - le reste. Sans ce test, un
  # parcellaire hors sujet passerait pour un succes et reetiquetterait TOUS les
  # tenements en " hors foret publique ", detruisant le decoupage existant pour
  # rien. Le signal juste est : aucune ligne rattachee a une UGF.
  dans_ugf <- if (nrow(ten) && "hors_ugf" %in% names(ten)) {
    !.isTRUE_vec(ten$hors_ugf)
  } else {
    rep(TRUE, if (is.null(ten)) 0L else nrow(ten))
  }
  if (is.null(ten) || nrow(ten) == 0L || !any(dans_ugf)) {
    return(list(status = "no_overlap", projet = projet, tenements = ten,
                n_retenues = n_retenues, n_total = n_total))
  }

  # Le label pilote l'affectation UGF de tenement_import_replace(). Les lignes
  # " hors UGF " arrivent avec nom_ugf = NA : sans label elles seraient des
  # tenements sans UGF, ce que projet_validate() refuse (invariant 2).
  label <- as.character(ten$nom_ugf)
  label[is.na(label) | !nzchar(label)] <- label_hors
  ten$label_ugf <- label

  projet <- tenement_import_replace(projet, ten)

  list(status = "ok", projet = projet, tenements = ten,
       n_retenues = n_retenues, n_total = n_total)
}


#' Drop the parcels that hold little or no public forest
#'
#' @description
#' Optional last step of the crossing, off by default: remove from the project
#' the cadastral parcels the public forest barely touches.
#'
#' **The test is on the PARCEL, never on the tenement.** A parcel that is partly
#' forest also carries a `hors_ugf` fragment - the share the forest does not
#' cover - and that fragment must stay: it is what makes the parcel exactly
#' tiled. Dropping it alone would leave a hole inside a parcel the user still
#' owns. Either the whole parcel goes, or none of it does.
#'
#' **Which parcels go**: those whose forest share is below `seuil_foret`,
#' 10 % by default. That subsumes the parcels with no forest at all (share 0)
#' and adds those the forest only grazes - a parcel 3 % forested is a
#' digitising edge effect, not a stand to manage, and carrying it into the plan
#' dilutes every per-unit indicator computed on it.
#'
#' The share is read from `surface_m2`, the cadastral surface the split already
#' distributed across tenements. No geometry is touched: comparing shares inside
#' one parcel needs no area recomputation, and `st_area()` on a CRS-bearing
#' geometry is the expensive call this codebase learned to avoid.
#'
#' The parcels are removed from `$parcels` as well as from `$tenements`. Keeping
#' them in `$parcels` would leave parcels with no tenement at all - visible in
#' the Selection tab, absent from the UGF map, and belonging to no unit of
#' management. That state is not one the rest of the app expects.
#'
#' @param projet List. Project already re-tiled by the crossing.
#' @param label_hors Character. Label of the "outside public forest" UGF.
#' @param seuil_foret Numeric in 0..1. Forest share **at or below** which the
#'   parcel is dropped. At `0`, that is every parcel the forest does not touch
#'   at all - and only those. The comparison used to be strict, which made `0`
#'   mean "purge nothing": a setting that did nothing at its own default.
#'
#' @return List with `projet`, `n_supprimees` (parcels dropped) and
#'   `n_partielles` (parcels KEPT that still hold a non-forest share - they are
#'   why the "outside public forest" UGF survives a purge).
#'
#' @noRd
onf_purger_hors_foret <- function(projet, label_hors = .onf_label_hors_ugf(),
                                  seuil_foret = 0) {
  ugs <- projet$ugs
  ten <- projet$tenements
  vide <- list(projet = projet, n_supprimees = 0L, n_partielles = 0L)
  if (is.null(ugs) || is.null(ten) || nrow(ten) == 0L) return(vide)

  ug_hors <- ugs$ug_id[!is.na(ugs$label) & ugs$label == label_hors]
  if (length(ug_hors) == 0L) return(vide)

  seuil_foret <- suppressWarnings(as.numeric(seuil_foret))
  if (length(seuil_foret) != 1L || is.na(seuil_foret)) seuil_foret <- 0
  seuil_foret <- max(0, min(1, seuil_foret))

  # Part FORESTIERE de chaque parcelle, lue sur `surface_m2` (la surface
  # cadastrale que le decoupage a deja repartie entre tenements). Aucune
  # geometrie n'est touchee : comparer des parts a l'interieur d'une meme
  # parcelle ne demande aucun recalcul d'aire.
  est_hors <- ten$ug_id %in% ug_hors
  surf <- suppressWarnings(as.numeric(ten$surface_m2))
  surf[is.na(surf)] <- 0
  pid <- as.character(ten$parent_parcelle_id)
  tot  <- tapply(surf, pid, sum)
  foret <- tapply(surf * !est_hors, pid, sum)

  # Une parcelle de surface nulle n'a pas de part definie : on la laisse, la
  # supprimer sur une division par zero serait arbitraire.
  part <- ifelse(tot > 0, foret / tot, NA_real_)
  # `<=` et non `<` : sans quoi le seuil 0 - le defaut - ne supprimerait RIEN,
  # pas meme une parcelle sans un metre carre de foret. Au seuil exact la
  # parcelle part donc, ce qui est le sens qu'on attend de « moins de 10 % ».
  a_supprimer <- names(part)[!is.na(part) & part <= seuil_foret]
  # Ne rien supprimer n'est PAS ne rien avoir a dire. Si l'UGF " hors foret
  # publique " subsiste, c'est que des parcelles CONSERVEES gardent une part
  # non forestiere - et ce chemin sortait muet, si bien que l'utilisateur
  # lisait la ligne survivante comme une purge en panne. C'est le cas de
  # Couchey : les 21 parcelles touchent TOUTES la foret publique (la plus
  # faible a 5,05 %), donc le seuil 0 - " seulement ce que la foret ne touche
  # pas du tout " - n'a legitimement rien a prendre.
  if (length(a_supprimer) == 0L) {
    vide$n_partielles <- length(unique(pid[est_hors]))
    return(vide)
  }

  projet$tenements <- ten[!as.character(ten$parent_parcelle_id) %in% a_supprimer,
                          , drop = FALSE]

  parcels <- projet$parcels
  if (!is.null(parcels) && inherits(parcels, "sf")) {
    id_col <- intersect(c("id", "nemeton_id", "geo_parcelle"), names(parcels))
    if (length(id_col)) {
      garder <- !as.character(parcels[[id_col[1]]]) %in% a_supprimer
      projet$parcels <- parcels[garder, , drop = FALSE]
    }
  }

  # Une UGF que plus aucun tenement ne porte violerait l'invariant 3. C'est le
  # cas de " hors foret " des lors qu'aucune parcelle mi-forestiere ne subsiste.
  actives <- unique(projet$tenements$ug_id)
  projet$ugs <- ugs[ugs$ug_id %in% actives, , drop = FALSE]

  projet_validate(projet)

  # Parcelles CONSERVEES qui gardent une part hors foret : ce sont elles qui
  # font survivre l'UGF " hors foret publique " a une purge. Sans ce chiffre,
  # l'utilisateur voit une ligne " Hors foret publique " subsister apres avoir
  # demande la suppression, et croit la purge incomplete.
  reste_hors <- projet$tenements$ug_id %in% ug_hors
  n_partielles <- length(unique(
    as.character(projet$tenements$parent_parcelle_id)[reste_hors]))

  cli::cli_alert_success(
    "Parcellaire ONF : {length(a_supprimer)} parcelle{?s} sous {round(100 * seuil_foret)} % \\
     de for\u00eat publique retir\u00e9e{?s} du projet")

  list(projet = projet, n_supprimees = length(a_supprimer),
       n_partielles = n_partielles)
}


#' Summarise a crossing for the user
#'
#' @description
#' Everything below is read off the core's return; nothing is recomputed
#' (rule: the core already did the arithmetic).
#'
#' `part_ugf` is summed per UGF: it answers " how much of that forest parcel do
#' you actually hold ", which is the question a private owner asks first when a
#' UGF straddles land that is not theirs.
#'
#' @param ten Crossing table from [onf_projet_croise()].
#'
#' @return List with `n_ugf`, `n_parcelles`, `n_multi` (UGF spanning several
#'   cadastral parcels), `surface_hors_ha` and `partielles` (named numeric of
#'   the UGF held only in part, share in 0..1).
#'
#' @noRd
onf_croise_resume <- function(ten) {
  vide <- list(n_ugf = 0L, n_parcelles = 0L, n_multi = 0L,
               surface_hors_ha = 0, partielles = numeric(0))
  if (is.null(ten) || nrow(ten) == 0L) return(vide)

  hors <- if ("hors_ugf" %in% names(ten)) .isTRUE_vec(ten$hors_ugf) else rep(FALSE, nrow(ten))
  dans <- ten[!hors, , drop = FALSE]

  surface_hors <- if (any(hors)) {
    sum(as.numeric(ten$surface_ha[hors]), na.rm = TRUE)
  } else 0

  if (nrow(dans) == 0L) {
    vide$surface_hors_ha <- surface_hors
    return(vide)
  }

  part <- tapply(as.numeric(dans$part_ugf), as.character(dans$ugf_id), sum)
  # Tolerance : une somme de parts reconstruite geometriquement n'atteint pas
  # exactement 1. En dessous de 0,999 l'UGF est reellement detenue en partie.
  partielles <- part[!is.na(part) & part < 0.999]

  list(
    n_ugf           = length(unique(dans$ugf_id)),
    n_parcelles     = length(unique(dans$parcelle_cadastrale)),
    n_multi         = sum(tapply(dans$parcelle_cadastrale, dans$ugf_id,
                                 function(x) length(unique(x))) > 1L),
    surface_hors_ha = surface_hors,
    partielles      = partielles
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

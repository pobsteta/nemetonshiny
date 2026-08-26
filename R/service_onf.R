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


#' Split a layer into single-part polygons
#'
#' @description
#' The core returns the crossing remainder **fused into one row per cadastral
#' parcel** (`.croiser_reste()` is a single `st_difference()` per parcel), so a
#' 10.89 ha "remainder" of `212000000A0036` is one multipart geometry made of
#' eight scattered strips. At that granularity "the neighbouring UGF" has no
#' meaning: that parcel borders twenty-eight of them.
#'
#' Splitting first is therefore not a detail of shape - it is what makes the
#' attachment answerable at all.
#'
#' @param x An `sf`.
#' @return The same `sf`, one row per part. Unchanged if the cast fails.
#' @noRd
.onf_singleparts <- function(x) {
  if (!inherits(x, "sf") || nrow(x) == 0L) return(x)
  out <- tryCatch(
    suppressWarnings(sf::st_cast(sf::st_make_valid(x), "POLYGON")),
    error = function(e) NULL)
  if (!inherits(out, "sf") || nrow(out) < nrow(x)) return(x)
  out
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


#' Attach every un-numbered piece of a cadastral parcel to its neighbour
#'
#' @description
#' **The rule this implements** (Pascal, 2026-08-26): the ONF layer is not a
#' filter, it is a *source of labels*. It is crossed to recover the forest
#' parcel number, then discarded. What stands is the cadastre - so the UGF are
#' groups, splits or whole cadastral parcels, and **nothing of a cadastral
#' parcel is ever dropped or set aside**.
#'
#' The crossing leaves pieces the ONF layer does not cover: 50.34 ha over
#' Couchey's 529.73. They used to be gathered into one "outside public forest"
#' UGF, which read as *land outside the forest* when it is nothing of the sort -
#' it is forest the ONF layer simply did not number: rides, tracks, the gaps
#' between adjacent forest parcels, and digitising slack along the edges.
#'
#' **Each piece joins the forest parcel with which it shares the longest common
#' boundary** ("option A", chosen over "everything to the largest UGF" on
#' evidence: on `212000000A0036`, whose 28 forest parcels are cut from a single
#' cadastral parcel, the dominant one would swallow 10.89 ha of strips it does
#' not touch, growing 77 % on land that is nowhere near it).
#'
#' Boundary **length**, not area or distance: a ride running along parcel 3 for
#' 400 m and grazing parcel 4 at a corner belongs to 3, whatever their
#' respective sizes.
#'
#' A piece with no forest neighbour - a whole parcel the ONF layer misses, or a
#' sliver touching nothing - falls back on [.onf_label_cadastrale()]. All the
#' pieces of one parcel then share a label, so they land in **one** UGF rather
#' than one each.
#'
#' **Where this belongs.** The crossing arithmetic lives in the core, and so
#' should this; the app carries it because `croiser_parcelles_onf()` has no such
#' option yet and the core is another repository. Brief filed:
#' `briefs/vers-nemeton/2026-08-26-rattachement-reste-voisin.md`.
#'
#' @param ten Crossing table from `nemeton::croiser_parcelles_onf()`, with
#'   `hors_ugf` and `parcelle_cadastrale`.
#' @param i18n Translator, or `NULL`.
#' @return `ten`, remainder split into parts, each row carrying `label_ugf`.
#' @noRd
.onf_rattacher_reste <- function(ten, i18n = NULL) {
  if (!inherits(ten, "sf") || nrow(ten) == 0L) return(ten)
  if (!all(c("hors_ugf", "parcelle_cadastrale") %in% names(ten))) return(ten)

  etiquette <- function(x) {
    lab <- as.character(x$nom_ugf)
    vide <- is.na(lab) | !nzchar(lab)
    lab[vide] <- .onf_label_cadastrale(x$parcelle_cadastrale[vide], i18n)
    lab
  }

  est_hors <- .isTRUE_vec(ten$hors_ugf)
  if (!any(est_hors)) {
    ten$label_ugf <- etiquette(ten)
    return(ten)
  }

  fo <- ten[!est_hors, , drop = FALSE]
  re <- .onf_singleparts(ten[est_hors, , drop = FALSE])
  re$label_ugf <- NA_character_

  # Par parcelle cadastrale : « le voisin » ne veut rien dire au-dela. Deux
  # UGF qui se touchent dans DEUX parcelles differentes ne se disputent pas un
  # bout - chaque bout appartient a une parcelle, et il y reste.
  if (nrow(fo) > 0L) {
    fo_par <- split(seq_len(nrow(fo)), as.character(fo$parcelle_cadastrale))
    re_par <- split(seq_len(nrow(re)), as.character(re$parcelle_cadastrale))
    for (ref in names(re_par)) {
      ir <- re_par[[ref]]
      jf <- fo_par[[ref]]
      if (is.null(jf) || length(jf) == 0L) next
      br <- sf::st_boundary(sf::st_geometry(re)[ir])
      bf <- sf::st_boundary(sf::st_geometry(fo)[jf])
      # Une seule intersection croisee, vectorisee : `idx` rend les paires qui
      # se rencontrent, et rien d'autre n'est calcule. La boucle naive faisait
      # 8 x 29 appels sur A0036 pour la meme information.
      it <- tryCatch(suppressWarnings(sf::st_intersection(br, bf)),
                     error = function(e) NULL)
      if (is.null(it) || length(it) == 0L) next
      idx <- attr(it, "idx")
      if (is.null(idx)) next
      L <- suppressWarnings(as.numeric(sf::st_length(it)))
      L[is.na(L)] <- 0
      # Un contact PONCTUEL a une longueur nulle : ce n'est pas un voisinage.
      garde <- L > 0
      if (!any(garde)) next
      cle <- paste(idx[garde, 1], idx[garde, 2], sep = "/")
      somme <- tapply(L[garde], cle, sum)
      parts <- do.call(rbind, strsplit(names(somme), "/", fixed = TRUE))
      for (i in unique(parts[, 1])) {
        sel <- parts[, 1] == i
        best <- which(sel)[which.max(somme[sel])]
        re$label_ugf[ir[as.integer(i)]] <-
          as.character(fo$nom_ugf[jf[as.integer(parts[best, 2])]])
      }
    }
  }

  # Sans voisin forestier : la parcelle cadastrale devient son propre UGF.
  orphelin <- is.na(re$label_ugf) | !nzchar(re$label_ugf)
  re$label_ugf[orphelin] <-
    .onf_label_cadastrale(re$parcelle_cadastrale[orphelin], i18n)

  fo$label_ugf <- etiquette(fo)
  # `hors_ugf` devient faux pour tout le monde : plus rien n'est « hors ».
  re$hors_ugf <- FALSE
  rbind(fo, re[, names(fo)])
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
                              i18n = NULL) {
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

  # Part FORESTIERE de chaque parcelle, RELEVEE AVANT le rattachement. Apres,
  # l'information n'existe plus : tout appartient a une UGF forestiere, il n'y
  # a plus d'UGF « hors » a interroger. C'est elle, et non le label survivant,
  # que la purge consomme desormais.
  part_foret <- .onf_part_foret(ten)

  # Le label pilote l'affectation UGF de tenement_import_replace(). Chaque bout
  # sans numero rejoint son voisin ; plus aucune ligne ne reste « hors UGF »,
  # donc plus aucun tenement sans UGF (invariant 2).
  projet <- tenement_import_replace(projet, .onf_rattacher_reste(ten, i18n))

  # `tenements` reste la table AVANT rattachement : c'est elle que lit
  # [onf_croise_resume()], et le resume doit dire ce que le croisement a
  # TROUVE - dont la surface qu'aucune parcelle forestiere ne numerotait.
  # Apres rattachement, `hors_ugf` est faux partout et cette information a
  # disparu, ce qui rendrait le compte rendu muet sur le sujet meme qui
  # interesse l'utilisateur.
  list(status = "ok", projet = projet, tenements = ten,
       n_retenues = n_retenues, n_total = n_total,
       part_foret = part_foret)
}


#' Forest share of each cadastral parcel, read off the crossing
#'
#' @description
#' Taken from `surface_ha` and `hors_ugf` on the core's crossing table, **before
#' the remainder is attached**. Afterwards the question has no answer left to
#' read: every piece belongs to a forest UGF, and nothing says any more which
#' part of a parcel the ONF layer actually numbered.
#'
#' No geometry is touched - comparing shares inside one parcel needs no area
#' recomputation, and `st_area()` on a CRS-bearing geometry is the expensive
#' call this codebase learned to avoid.
#'
#' @param ten Crossing table from `nemeton::croiser_parcelles_onf()`.
#' @return Named numeric, one share in 0..1 per cadastral reference. A parcel of
#'   zero area has no defined share and gets `NA`.
#' @noRd
.onf_part_foret <- function(ten) {
  vide <- stats::setNames(numeric(0), character(0))
  if (!inherits(ten, "sf") || nrow(ten) == 0L) return(vide)
  if (!all(c("hors_ugf", "parcelle_cadastrale", "surface_ha") %in% names(ten))) {
    return(vide)
  }
  hors <- .isTRUE_vec(ten$hors_ugf)
  s <- suppressWarnings(as.numeric(ten$surface_ha))
  s[is.na(s)] <- 0
  ref <- as.character(ten$parcelle_cadastrale)
  tot <- tapply(s, ref, sum)
  fo  <- tapply(s * !hors, ref, sum)
  stats::setNames(as.numeric(ifelse(tot > 0, fo / tot, NA_real_)), names(tot))
}


#' Drop the parcels that hold little or no public forest
#'
#' @description
#' Optional last step of the crossing, off by default and **reserved to the
#' hand-made selection**: remove from the project the cadastral parcels the
#' public forest barely touches.
#'
#' **It has no place on the CSV path** (Pascal, 2026-08-26). A CSV lists the
#' forest: its parcels ARE the forest, all of them, and purging them would
#' delete what the file asserts. Picking parcels by hand on the cadastral map is
#' another matter - a selection can obviously overshoot, and this is the way
#' back.
#'
#' **The test is on the PARCEL, never on the piece.** Either the whole parcel
#' goes, or none of it does; dropping one piece would leave a hole inside a
#' parcel the user still owns.
#'
#' The share comes from [.onf_part_foret()], **measured before the remainder was
#' attached**. Until v0.139.0 it was re-derived from the surviving "outside
#' public forest" UGF - which no longer exists, every piece having joined its
#' neighbour ([.onf_rattacher_reste()]).
#'
#' @param projet List. Project already re-tiled by the crossing.
#' @param part_foret Named numeric from [.onf_part_foret()].
#' @param seuil_foret Numeric in 0..1. Forest share **at or below** which the
#'   parcel is dropped. At `0`, that is every parcel the forest does not touch
#'   at all - and only those. The comparison used to be strict, which made `0`
#'   mean "purge nothing": a setting that did nothing at its own default.
#'
#' @return List with `projet` and `n_supprimees`.
#'
#' @noRd
onf_purger_hors_foret <- function(projet, part_foret, seuil_foret = 0) {
  vide <- list(projet = projet, n_supprimees = 0L)
  if (is.null(part_foret) || length(part_foret) == 0L) return(vide)
  ten <- projet$tenements
  if (is.null(ten) || nrow(ten) == 0L) return(vide)

  seuil_foret <- suppressWarnings(as.numeric(seuil_foret))
  if (length(seuil_foret) != 1L || is.na(seuil_foret)) seuil_foret <- 0
  seuil_foret <- max(0, min(1, seuil_foret))

  # `<=` et non `<` : sans quoi le seuil 0 - le defaut - ne supprimerait RIEN,
  # pas meme une parcelle sans un metre carre de foret. Au seuil exact la
  # parcelle part donc, ce qui est le sens qu'on attend de « moins de 10 % ».
  a_supprimer <- names(part_foret)[!is.na(part_foret) & part_foret <= seuil_foret]
  if (length(a_supprimer) == 0L) return(vide)

  projet$tenements <- ten[!as.character(ten$parent_parcelle_id) %in% a_supprimer,
                          , drop = FALSE]

  # Les parcelles quittent AUSSI `$parcels`. Les y laisser donnerait des
  # parcelles sans aucun tenement - visibles dans l'onglet Selection, absentes
  # de la carte UGF, membres d'aucune unite de gestion. C'est le defaut paye en
  # v0.130.7.
  parcels <- projet$parcels
  if (!is.null(parcels) && inherits(parcels, "sf")) {
    id_col <- intersect(c("id", "nemeton_id", "geo_parcelle"), names(parcels))
    if (length(id_col)) {
      garder <- !as.character(parcels[[id_col[1]]]) %in% a_supprimer
      projet$parcels <- parcels[garder, , drop = FALSE]
    }
  }

  # Une UGF que plus aucun tenement ne porte violerait l'invariant 3.
  actives <- unique(projet$tenements$ug_id)
  projet$ugs <- projet$ugs[projet$ugs$ug_id %in% actives, , drop = FALSE]

  projet_validate(projet)

  cli::cli_alert_success(
    "Parcellaire ONF : {length(a_supprimer)} parcelle{?s} \u00e0 \\
     {round(100 * seuil_foret)} % ou moins de for\u00eat publique \\
     retir\u00e9e{?s} du projet")

  list(projet = projet, n_supprimees = length(a_supprimer))
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

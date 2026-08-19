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
#' Two paths, deliberately distinct:
#'
#' * [onf_projet_from_parcelles()] - the forest parcels **replace** the
#'   cadastral ones. One forest parcel = one UGF. Used when the cadastre is not
#'   the frame of reference at all.
#' * [onf_projet_croise()] - the cadastral selection is **kept** (it is the
#'   user's property) and each UGF is described as the pieces of cadastral
#'   parcels it is made of.
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
                               max_parcelles = 5000L) {
  if (is.null(aoi) || !inherits(aoi, c("sf", "sfc"))) {
    return(list(status = "no_aoi", parcelles = NULL))
  }
  if (!domanialite %in% c("toutes", "domaniale", "autre")) {
    domanialite <- "toutes"
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

  list(status = "ok", parcelles = parcelles)
}


#' Build a project whose UGF are the ONF forest parcels
#'
#' @description
#' One forest parcel = one tenement = one UGF, through the existing
#' `ug_init_default()` path. The forest parcels **replace** `projet$parcels`.
#'
#' `geo_parcelle` is set to `nom_ugf` because `ug_init_default()` takes the UG
#' label from `geo_parcelle` when present and from the id otherwise: without
#' this the UGF would be called `F06831S-400` instead of " Foret domaniale de
#' Chaux - parcelle 400 ". `contenance` is already carried by the core, so no
#' surface is recomputed here.
#'
#' @param projet List. Current project.
#' @param parcelles `sf` from [onf_load_parcelles()].
#'
#' @return The updated project.
#'
#' @noRd
onf_projet_from_parcelles <- function(projet, parcelles) {
  if (!inherits(parcelles, "sf") || nrow(parcelles) == 0L) {
    cli::cli_abort("onf_projet_from_parcelles: `parcelles` must be a non-empty sf")
  }
  parcelles$geo_parcelle <- as.character(parcelles$nom_ugf)
  # Affectation DIRECTE, surtout pas `utils::modifyList()` (que l'esquisse du
  # brief emploie) : modifyList RECURSE dans les listes, et un data.frame en
  # est une. Il fusionnerait donc les colonnes de l'ancien parcellaire avec
  # celles du nouveau au lieu de remplacer l'objet - erreur immediate des que
  # les deux n'ont pas le meme nombre de lignes (" replacement has 427 rows,
  # data has 1 " sur la foret domaniale de Chaux), et fusion SILENCIEUSE quand
  # ils l'ont, ce qui est pire.
  projet$parcels <- parcelles
  ug_init_default(projet)
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
#' Nothing is filtered or recomputed here: `min_surface_ha` absorption and the
#' optional cadastral snapping both happen in the core.
#'
#' @param projet List. Project holding `$parcels`, `$tenements`, `$ugs`.
#' @param onf `sf` of forest parcels from [onf_load_parcelles()].
#' @param caler_sur_cadastre Logical. Snap UGF boundaries onto cadastral ones.
#' @param seuil_calage Numeric. Share above which a parcel is taken whole.
#' @param label_hors Character. Label for the non-forest remainder.
#'
#' @return List with `status` (`"ok"` or `"no_overlap"`), `projet` and the
#'   crossing table `tenements` (for the user-facing summary).
#'
#' @noRd
onf_projet_croise <- function(projet,
                              onf,
                              caler_sur_cadastre = FALSE,
                              seuil_calage = 0.9,
                              label_hors = .onf_label_hors_ugf()) {
  if (!has_ug_data(projet)) {
    cli::cli_abort("Project must have UG data. Run ug_init_default() first.")
  }
  parcelles <- projet$parcels
  if (is.null(parcelles) || !inherits(parcelles, "sf") || nrow(parcelles) == 0L) {
    cli::cli_abort("Project must have non-empty parcels sf object")
  }

  ten <- nemeton::croiser_parcelles_onf(
    onf,
    parcelles,
    caler_sur_cadastre = isTRUE(caler_sur_cadastre),
    seuil_calage       = seuil_calage,
    # Cf. la doc ci-dessus : le reliquat porte le pavage exact, il n'est pas
    # une option d'affichage.
    inclure_reste      = TRUE
  )

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
    return(list(status = "no_overlap", projet = projet, tenements = ten))
  }

  # Le label pilote l'affectation UGF de tenement_import_replace(). Les lignes
  # " hors UGF " arrivent avec nom_ugf = NA : sans label elles seraient des
  # tenements sans UGF, ce que projet_validate() refuse (invariant 2).
  label <- as.character(ten$nom_ugf)
  label[is.na(label) | !nzchar(label)] <- label_hors
  ten$label_ugf <- label

  projet <- tenement_import_replace(projet, ten)

  list(status = "ok", projet = projet, tenements = ten)
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

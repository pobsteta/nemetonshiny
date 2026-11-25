#' Itineraires sylvicoles par UGF : admissibles, exclusions, classement
#'
#' @description
#' Couche applicative de l'onglet « Itineraires » du plan d'actions
#' (mission 066, specs 065 du coeur). Aucune logique metier ici : le coeur
#' dit ce qui est admissible (`nemeton::itineraires_admissibles()`), pourquoi
#' un itineraire est exclu (codes fermes), et classe sous un profil
#' (`nemeton::comparer_itineraires()` avec `nemeton::profils_poids()`). Ce
#' fichier assemble les UGF du projet, appelle le coeur, traduit les codes et
#' prepare les tables a afficher.
#'
#' Trois regles de presentation viennent de la spec :
#'
#'   * `score_global` est un **rang**, jamais une note : il n'est pas rendu,
#'     seul `rang` l'est ;
#'   * les poids de `profils_poids()` sont **provisoires** (decision de
#'     Pascal, 2026-11-25) et l'ecran le dit ;
#'   * `donnee_manquante` n'est jamais silencieux : la ligne reste, avec sa
#'     raison.
#'
#' @name service_itineraires
#' @keywords internal
NULL


#' Codes de raison d'exclusion connus (spec 065 section 4)
#' @noRd
ITIN_RAISONS <- c("station_insuffisante", "hors_ser", "essence_absente",
                  "pente", "desserte", "statut_protection",
                  "source_manquante", "donnee_manquante")


#' Le coeur expose-t-il les quatre fonctions des itineraires ?
#' @return Logical scalar.
#' @noRd
.itin_noyau_disponible <- function() {
  attendues <- c("itineraires_admissibles", "comparer_itineraires",
                 "profils_poids", "itineraires_catalogue")
  exports <- tryCatch(getNamespaceExports("nemeton"),
                      error = function(e) character())
  all(attendues %in% exports)
}


#' Cle i18n de la raison d'exclusion
#'
#' Prefixe court `itin_r_` : `itin_raison_station_insuffisante` depasserait les
#' 30 caracteres de la convention NMT.
#'
#' @param raison Character. Code du coeur (`NA` si admissible).
#' @return Character. Cle de `TRANSLATIONS`; `itin_r_inconnue` pour un code que
#'   l'app ne connait pas (jamais le code brut a l'ecran).
#' @noRd
.itin_cle_raison <- function(raison) {
  if (length(raison) != 1L || is.na(raison)) return(NA_character_)
  if (raison %in% ITIN_RAISONS) paste0("itin_r_", raison) else "itin_r_inconnue"
}


#' Profils d'acteur proposes a la comparaison
#'
#' Les quinze cles de `nemeton::profils_poids()` (`profil_<x>`), libelles pris
#' dans les profils experts de l'app (`inst/experts/<x>.yml`) quand ils
#' existent. Retombe sur la cle sans prefixe, jamais sur une erreur.
#'
#' @param lang `"fr"` ou `"en"`.
#' @return Named character vector (noms = libelles, valeurs = cles du coeur).
#' @noRd
.itin_profils_choix <- function(lang = "fr") {
  cles <- tryCatch(names(nemeton::profils_poids()), error = function(e) character())
  if (!length(cles)) return(stats::setNames(character(), character()))
  experts <- tryCatch(get_expert_profiles(), error = function(e) list())
  alias <- c(proprietaire_prive = "owner", chasseur = "hunter")
  libelle <- vapply(cles, function(k) {
    court <- sub("^profil_", "", k)
    cand <- c(court, unname(alias[court]))
    cand <- cand[!is.na(cand) & cand %in% names(experts)]
    lbl <- if (length(cand)) experts[[cand[1]]]$label[[lang]] %||%
      experts[[cand[1]]]$label[["en"]] else NULL
    if (is.null(lbl) || !nzchar(lbl)) gsub("_", " ", court) else lbl
  }, character(1))
  stats::setNames(cles, libelle)
}


#' Unites du projet au format attendu par le coeur
#'
#' Reprend le tableau des indicateurs du projet (une ligne par UGF) et nomme les
#' colonnes que `itineraires_admissibles()` lit : `unit_id`, `P2`, `ser`,
#' `essence`, `pente_pct`, `desserte_m`. Une colonne que le projet n'a pas est
#' **laissee absente** : le coeur rend alors `donnee_manquante` pour les
#' itineraires qui l'exigent, au lieu d'une valeur inventee. La distance de
#' desserte (spec 065 D-3) n'est aujourd'hui calculee nulle part dans l'app :
#' elle n'est lue que si le projet porte deja `desserte_m`.
#'
#' @param project Projet courant (liste avec `indicators_sf` ou `indicators`).
#' @return Une liste `unites` (data.frame sans geometrie, ou `NULL`) et
#'   `desserte` (logical : la colonne `desserte_m` est-elle renseignee ?).
#' @noRd
.itin_unites <- function(project) {
  src <- project$indicators_sf
  if (is.null(src) || !is.data.frame(src)) src <- project$indicators
  if (is.null(src) || !is.data.frame(src) || nrow(src) == 0L) {
    return(list(unites = NULL, desserte = FALSE))
  }
  if (inherits(src, "sf")) src <- sf::st_drop_geometry(src)
  src <- as.data.frame(src, stringsAsFactors = FALSE)
  col_id <- intersect(c("ug_id", "id", "nemeton_id"), names(src))[1]
  u <- data.frame(unit_id = if (is.na(col_id)) as.character(seq_len(nrow(src)))
                            else as.character(src[[col_id]]),
                  stringsAsFactors = FALSE)
  pris <- function(noms) {
    n <- intersect(noms, names(src))[1]
    if (is.na(n)) NULL else src[[n]]
  }
  p2 <- pris(c("P2", "indicateur_p2_station"))
  if (!is.null(p2)) u$P2 <- suppressWarnings(as.numeric(p2))
  ser <- pris(c("ser", ".p2_ser"))
  if (!is.null(ser)) u$ser <- as.character(ser)
  ess <- pris("essence")
  if (!is.null(ess)) u$essence <- as.character(ess)
  pente <- pris("pente_pct")
  if (!is.null(pente)) u$pente_pct <- suppressWarnings(as.numeric(pente))
  des <- pris("desserte_m")
  if (!is.null(des)) u$desserte_m <- suppressWarnings(as.numeric(des))
  actuel <- pris("itineraire_actuel")
  if (!is.null(actuel)) u$itineraire_actuel <- as.character(actuel)
  list(unites = u,
       desserte = !is.null(des) && any(is.finite(suppressWarnings(as.numeric(des)))))
}


#' Libelle d'un itineraire du catalogue
#' @noRd
.itin_libelle <- function(catalogue, code, lang = "fr") {
  vapply(code, function(cd) {
    it <- catalogue[[cd]]
    lbl <- if (is.null(it)) NULL else it$libelle[[lang]] %||% it$libelle[["en"]]
    if (is.null(lbl) || !nzchar(lbl)) cd else lbl
  }, character(1), USE.NAMES = FALSE)
}


#' Itineraires du catalogue pas encore sources
#'
#' @param catalogue Liste du coeur.
#' @return `c(a_sourcer = n, total = N)`.
#' @noRd
.itin_etat_catalogue <- function(catalogue) {
  src <- vapply(catalogue, function(it) identical(it$statut_source, "source"),
                logical(1))
  c(a_sourcer = sum(!src), total = length(src))
}


#' Calcule la vue Itineraires d'un projet
#'
#' Appelle le coeur une fois pour les admissibles et une fois pour la
#' comparaison sous le profil. Ne leve jamais : toute erreur du coeur devient un
#' `statut = "erreur"` que la vue dit.
#'
#' @param project Projet courant, ou `NULL`.
#' @param profil Cle d'un profil de `nemeton::profils_poids()`.
#' @param catalogue Catalogue (defaut : celui du coeur). Injectable pour les tests.
#' @return Liste : `statut` (`"ok"`, `"sans_projet"`, `"sans_ugf"`,
#'   `"noyau_absent"`, `"erreur"`), `admissibles`, `comparaison`, `catalogue`,
#'   `desserte`, `message` (erreur).
#' @noRd
itineraires_calculer <- function(project, profil, catalogue = NULL) {
  vide <- function(statut, ...) c(list(statut = statut, admissibles = NULL,
                                       comparaison = NULL, catalogue = NULL,
                                       desserte = FALSE), list(...))
  if (!.itin_noyau_disponible()) return(vide("noyau_absent"))
  if (is.null(project) || is.null(project$id)) return(vide("sans_projet"))
  u <- .itin_unites(project)
  if (is.null(u$unites)) return(vide("sans_ugf"))
  tryCatch({
    if (is.null(catalogue)) catalogue <- nemeton::itineraires_catalogue()
    adm <- suppressMessages(nemeton::itineraires_admissibles(
      u$unites, catalogue = catalogue,
      colonnes = list(id = "unit_id", p2 = "P2", ser = "ser", essence = "essence",
                      pente = "pente_pct", desserte = "desserte_m")))
    cmp <- suppressMessages(nemeton::comparer_itineraires(
      u$unites, poids = nemeton::profils_poids(profil), catalogue = catalogue,
      admissibles = adm))
    list(statut = "ok", admissibles = adm, comparaison = cmp,
         catalogue = catalogue, desserte = u$desserte, unites = u$unites)
  }, error = function(e) {
    vide("erreur", message = conditionMessage(e))
  })
}


#' Table des admissibles et des exclusions, traduite
#'
#' Une ligne par UGF x itineraire, **toutes** conservees : un itineraire exclu
#' ou a donnee manquante reste visible avec sa raison.
#'
#' @param adm Sortie de `nemeton::itineraires_admissibles()`.
#' @param catalogue Catalogue.
#' @param i18n Traducteur.
#' @return data.frame : `ugf`, `itineraire`, `etat`, `raison`, `statut`
#'   (`"admissible"`, `"exclu"`, `"manquant"`). Le `detail` du coeur (valeur
#'   observee contre seuil, en anglais, avec des codes bruts) n'est pas repris.
#' @noRd
itineraires_table_admissibles <- function(adm, catalogue, i18n) {
  if (is.null(adm) || nrow(adm) == 0L) return(NULL)
  statut <- ifelse(is.na(adm$admissible), "manquant",
                   ifelse(adm$admissible, "admissible", "exclu"))
  etat <- vapply(statut, function(s) i18n$t(switch(s,
    admissible = "itin_etat_adm", exclu = "itin_etat_exclu",
    manquant = "itin_etat_manque")), character(1), USE.NAMES = FALSE)
  raison <- vapply(adm$raison, function(r) {
    k <- .itin_cle_raison(r)
    if (is.na(k)) "" else i18n$t(k)
  }, character(1), USE.NAMES = FALSE)
  data.frame(ugf = as.character(adm$unit_id),
             itineraire = .itin_libelle(catalogue, adm$itineraire, i18n$language),
             etat = etat, raison = raison,
             statut = statut, stringsAsFactors = FALSE)
}


#' Table du classement sous un profil : un rang, jamais une note
#'
#' `score_global` n'est volontairement pas repris : c'est un ordre issu
#' d'effets qualitatifs, que le coeur demande d'afficher comme un rang.
#'
#' @param cmp Sortie de `nemeton::comparer_itineraires()`.
#' @param catalogue Catalogue.
#' @param i18n Traducteur.
#' @return data.frame : `ugf`, `rang`, `itineraire`, `evaluees`, `gagnantes`,
#'   `perdantes`, `pareto`, ordonnee par UGF puis rang; `NULL` si vide.
#' @noRd
itineraires_table_classement <- function(cmp, catalogue, i18n) {
  if (is.null(cmp) || !is.data.frame(cmp) || nrow(cmp) == 0L) return(NULL)
  cmp <- cmp[order(cmp$unit_id, cmp$rang), , drop = FALSE]
  pareto <- vapply(cmp$pareto, function(p) {
    if (is.na(p)) "—" else i18n$t(if (isTRUE(p)) "itin_oui" else "itin_non")
  }, character(1), USE.NAMES = FALSE)
  data.frame(ugf = as.character(cmp$unit_id),
             rang = as.integer(cmp$rang),
             itineraire = .itin_libelle(catalogue, cmp$itineraire, i18n$language),
             evaluees = cmp$familles_evaluees,
             gagnantes = cmp$familles_gagnantes,
             perdantes = cmp$familles_perdantes,
             pareto = pareto, stringsAsFactors = FALSE)
}

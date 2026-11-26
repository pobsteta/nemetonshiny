# Service "effet de la gestion" (mission 067, spec 068 du coeur).
#
# Ce que la gestion declaree a produit sur le massif du projet, calcule par
# `nemeton::effet_gestion()`. Aucune logique metier ici : l'effet, son intervalle
# a 90 % et le statut (`calcule`, `recouvrement_insuffisant`, `trop_peu_unites`)
# viennent du coeur ; ce fichier prepare les unites (type de gestion declare par
# UGF + covariables de station du projet), appelle le coeur et met le resultat
# en forme pour l'ecran. Le resultat est une ASSOCIATION ajustee sur la station,
# jamais une preuve causale : la mention est portee par le module.

#' Colonne du type de gestion passee au coeur
#' @noRd
EG_COLONNE_TRAITEMENT <- "type_gestion"

#' Types de gestion renseignes d'un projet, avec leur effectif
#'
#' `non_renseigne` ne compte pas : une UGF sans type declare n'est pas un
#' « type » a comparer.
#'
#' @param ugs data.frame des UGF du projet (`projet$ugs`), ou `NULL`.
#' @return Un vecteur entier nomme (noms = types, effectif en UGF), vide sans
#'   type renseigne.
#' @noRd
effet_gestion_types_renseignes <- function(ugs) {
  if (is.null(ugs) || !is.data.frame(ugs) || nrow(ugs) == 0L) return(integer(0))
  ugs <- .ug_gestion_normaliser(ugs)
  x <- ugs$type_gestion[ugs$type_gestion != UG_TYPE_GESTION_DEFAUT]
  tab <- table(factor(x, levels = setdiff(UG_TYPES_GESTION, UG_TYPE_GESTION_DEFAUT)))
  tab <- tab[tab > 0L]
  stats::setNames(as.integer(tab), names(tab))
}

#' Type de reference par defaut : le plus frequent parmi les renseignes
#'
#' Meme regle que le coeur (`reference = NULL`) : en cas d'egalite, le premier
#' dans l'ordre de `UG_TYPES_GESTION`.
#' @param ugs data.frame des UGF du projet.
#' @return Un type (character scalaire), ou `NA_character_` sans type renseigne.
#' @noRd
effet_gestion_reference_defaut <- function(ugs) {
  tp <- effet_gestion_types_renseignes(ugs)
  if (!length(tp)) return(NA_character_)
  names(tp)[which.max(tp)]
}

#' UGF sans type declare (`non_renseigne`), laissees hors du contraste
#' @param ugs data.frame des UGF du projet.
#' @return Un entier (0 sans UGF).
#' @noRd
effet_gestion_ugf_exclues <- function(ugs) {
  if (is.null(ugs) || !is.data.frame(ugs) || nrow(ugs) == 0L) return(0L)
  ugs <- .ug_gestion_normaliser(ugs)
  sum(ugs$type_gestion == UG_TYPE_GESTION_DEFAUT, na.rm = TRUE)
}

#' Le calcul est-il permis ? Au moins deux types renseignes
#' @param ugs data.frame des UGF du projet.
#' @return `TRUE` / `FALSE`.
#' @noRd
effet_gestion_calculable <- function(ugs) {
  length(effet_gestion_types_renseignes(ugs)) >= 2L
}

#' Unites au format attendu par `nemeton::effet_gestion()`
#'
#' Une ligne par UGF : `ug_id`, `type_gestion` (`NA` pour `non_renseigne`, que
#' le coeur laisse alors hors du contraste), les colonnes `famille_*` du projet
#' et les covariables de station que le projet porte : `P2`
#' (`indicateur_p2_station`), `ser` (`.p2_ser`), `altitude`, `pente`
#' (`pente_pct`). Une covariable absente est **laissee absente**, jamais
#' inventee ; le coeur en exige au moins une.
#'
#' @param family_sf Sortie de [project_family_scores()] (sf ou data.frame).
#' @param ugs data.frame des UGF du projet.
#' @return Un data.frame sans geometrie, ou `NULL` sans scores de famille, sans
#'   `ug_id` ou sans UGF.
#' @noRd
effet_gestion_unites <- function(family_sf, ugs) {
  if (is.null(family_sf) || !is.data.frame(family_sf) || nrow(family_sf) == 0L ||
      is.null(ugs) || !is.data.frame(ugs) || !"ug_id" %in% names(family_sf)) {
    return(NULL)
  }
  src <- if (inherits(family_sf, "sf")) sf::st_drop_geometry(family_sf) else family_sf
  src <- as.data.frame(src, stringsAsFactors = FALSE)
  fam <- grep("^famille_[a-z]", names(src), value = TRUE)
  if (length(fam) == 0L) return(NULL)

  ugs <- .ug_gestion_normaliser(ugs)
  type <- ugs$type_gestion[match(as.character(src$ug_id), as.character(ugs$ug_id))]
  type[is.na(type) | type == UG_TYPE_GESTION_DEFAUT] <- NA_character_

  u <- data.frame(ug_id = as.character(src$ug_id), stringsAsFactors = FALSE)
  u[[EG_COLONNE_TRAITEMENT]] <- type
  pris <- function(noms) {
    n <- intersect(noms, names(src))[1]
    if (is.na(n)) NULL else src[[n]]
  }
  p2 <- pris(c("P2", "indicateur_p2_station"))
  if (!is.null(p2)) u$P2 <- suppressWarnings(as.numeric(p2))
  ser <- pris(c("ser", ".p2_ser"))
  if (!is.null(ser)) u$ser <- as.character(ser)
  alt <- pris("altitude")
  if (!is.null(alt)) u$altitude <- suppressWarnings(as.numeric(alt))
  pente <- pris(c("pente", "pente_pct"))
  if (!is.null(pente)) u$pente <- suppressWarnings(as.numeric(pente))
  for (f in fam) u[[f]] <- suppressWarnings(as.numeric(src[[f]]))
  u
}

#' Covariables de station presentes dans des unites
#' @noRd
.eg_covariables <- function(units) intersect(c("P2", "altitude", "pente", "ser"), names(units))

#' Calcule l'effet de la gestion par le coeur
#'
#' Appelle `nemeton::effet_gestion(units, traitement = "type_gestion",
#' reference = reference)`. Les erreurs du coeur (colonne absente, moins de deux
#' niveaux, etc.) ne remontent pas : elles sont rendues dans `erreur`, pour que
#' l'ecran les dise au lieu de planter.
#'
#' @param units Sortie de [effet_gestion_unites()].
#' @param reference Niveau de reference de `type_gestion`, ou `NULL` (le coeur
#'   prend le plus frequent).
#' @param fn Fonction de calcul, le coeur par defaut (doublure en test).
#' @return Une liste : `resume` (data.frame du coeur, `NULL` si erreur),
#'   `reference`, `apprenant`, `covariables`, `erreur` (`NULL`, ou un code :
#'   `sans_unites`, `sans_covariable`, ou le message du coeur).
#' @noRd
effet_gestion_calculer <- function(units, reference = NULL,
                                   fn = nemeton::effet_gestion) {
  rien <- function(erreur) list(resume = NULL, reference = reference,
                                apprenant = NA_character_, covariables = character(0),
                                erreur = erreur)
  if (is.null(units) || !is.data.frame(units) || nrow(units) == 0L) {
    return(rien("sans_unites"))
  }
  if (length(.eg_covariables(units)) == 0L) return(rien("sans_covariable"))
  res <- tryCatch(
    fn(units, traitement = EG_COLONNE_TRAITEMENT, reference = reference),
    error = function(e) e)
  if (inherits(res, "error")) return(rien(conditionMessage(res)))
  list(resume = as.data.frame(res$resume, stringsAsFactors = FALSE),
       reference = res$reference %||% reference,
       apprenant = res$apprenant %||% NA_character_,
       covariables = res$covariables %||% character(0),
       erreur = NULL)
}

#' Libelle d'un type de gestion
#' @param type Vecteur de types (`UG_TYPES_GESTION`).
#' @param i18n Objet i18n.
#' @noRd
effet_gestion_libelle_type <- function(type, i18n) {
  vapply(as.character(type), function(t) {
    if (is.na(t) || !t %in% UG_TYPES_GESTION) return(if (is.na(t)) "" else t)
    i18n$t(paste0("ug_gestion_", t))
  }, character(1), USE.NAMES = FALSE)
}

#' Cle i18n du statut d'un contraste non calcule
#' @noRd
.eg_cle_statut <- function(statut) {
  switch(as.character(statut),
    recouvrement_insuffisant = "eg_st_recouvrement",
    trop_peu_unites = "eg_st_trop_peu",
    "eg_st_inconnu")
}

#' Nombre a une decimale, virgule en francais
#' @noRd
.eg_nombre <- function(x, language = "fr") {
  out <- formatC(x, format = "f", digits = 1)
  if (!identical(language, "en")) out <- sub(".", ",", out, fixed = TRUE)
  out[is.na(x)] <- ""
  out
}

#' Libelle de famille depuis la colonne `famille_*`
#' @noRd
.eg_libelle_famille <- function(col, language = "fr") {
  vapply(as.character(col), function(cl) {
    for (code in names(INDICATOR_FAMILIES)) {
      if (identical(get_famille_col(code), cl)) {
        fam <- INDICATOR_FAMILIES[[code]]
        return(if (identical(language, "en")) fam$name_en else fam$name_fr)
      }
    }
    cl
  }, character(1), USE.NAMES = FALSE)
}

#' Tableau a afficher : « ce que la gestion a produit sur ce massif »
#'
#' Une ligne par contraste et par famille calculee ; l'intervalle a 90 %
#' (`ic_bas` - `ic_haut`) est la colonne **avant** l'effet. Un contraste dont le
#' `statut` n'est pas `calcule` tient en UNE ligne : le statut traduit dans la
#' colonne de l'intervalle, rien d'autre (ni effet, ni effectifs).
#'
#' @param resume Le `resume` rendu par le coeur.
#' @param i18n Objet i18n (la langue en vient : `i18n$language`).
#' @return Un data.frame de chaines, colonnes deja titrees, ou `NULL` si `resume`
#'   est vide.
#' @noRd
effet_gestion_tableau <- function(resume, i18n) {
  if (is.null(resume) || !is.data.frame(resume) || nrow(resume) == 0L) return(NULL)
  lang <- i18n$language %||% "fr"
  tiret <- "\u2013"
  lignes <- list()
  vus <- character(0)
  for (i in seq_len(nrow(resume))) {
    r <- resume[i, , drop = FALSE]
    gestion <- effet_gestion_libelle_type(r$traitement, i18n)
    if (!identical(as.character(r$statut), "calcule")) {
      cle <- paste(r$traitement, r$statut)
      if (cle %in% vus) next
      vus <- c(vus, cle)
      lignes[[length(lignes) + 1L]] <- c(
        gestion, i18n$t("eg_toutes_familles"), i18n$t(.eg_cle_statut(r$statut)), "", "")
      next
    }
    lignes[[length(lignes) + 1L]] <- c(
      gestion,
      .eg_libelle_famille(r$resultat, lang),
      sprintf("%s %s %s", .eg_nombre(r$ic_bas, lang), tiret, .eg_nombre(r$ic_haut, lang)),
      .eg_nombre(r$effet_moyen, lang),
      sprintf("%d / %d", as.integer(r$n_traite), as.integer(r$n_reference)))
  }
  out <- as.data.frame(do.call(rbind, lignes), stringsAsFactors = FALSE)
  names(out) <- c(i18n$t("eg_col_gestion"), i18n$t("eg_col_famille"),
                  i18n$t("eg_col_ic"), i18n$t("eg_col_effet"), i18n$t("eg_col_n"))
  out
}

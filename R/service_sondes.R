# service_sondes.R - vue « Ce que les modeles voient » (mission 078, spec 066 du coeur)
#
# `nemeton::probe_indicator()` ajuste, pour chaque indicateur deja calcule, une
# regression de l'embedding moyen de chaque UGF vers l'indicateur et en lit le
# R2 en validation croisee spatiale. Aucune logique metier ici : le R2, la
# lecture (`voit`, `partiel`, `angle_mort`), le statut et le marqueur
# `source_commune` viennent du coeur. Ce fichier prepare les unites (colonnes
# nommees par code), retrouve l'embedding AlphaEarth DEJA en cache, appelle le
# coeur et met le resultat en forme. Une sonde est un diagnostic, pas un
# estimateur : la mention est portee par le module.

#' Unites au format attendu par `nemeton::probe_indicator()`
#'
#' Le coeur cherche les indicateurs par CODE (`C2`, `B4`...) et leur statut dans
#' `<code en minuscules>_status`. Le projet porte des colonnes `indicateur_*` et
#' des annexes `.xx_status` : on renomme, sans rien inventer. Les colonnes
#' vides (aucune valeur) sont laissees de cote.
#'
#' @param indicators_sf sf, `project$indicators_sf` (une ligne par UGF).
#' @return Un sf (UGF, codes en colonnes), ou `NULL` sans indicateur exploitable.
#' @noRd
sondes_unites <- function(indicators_sf) {
  if (is.null(indicators_sf) || !inherits(indicators_sf, "sf") ||
      nrow(indicators_sf) == 0L) {
    return(NULL)
  }
  lab <- tryCatch(nemeton::indicator_labels(lang = "en"), error = function(e) NULL)
  if (is.null(lab) || !all(c("code", "column_name") %in% names(lab))) return(NULL)

  out <- sf::st_sf(geometry = sf::st_geometry(indicators_sf))
  n_ind <- 0L
  for (i in seq_len(nrow(lab))) {
    col <- as.character(lab$column_name[i])
    code <- as.character(lab$code[i])
    if (!col %in% names(indicators_sf)) next
    v <- suppressWarnings(as.numeric(indicators_sf[[col]]))
    if (!any(is.finite(v))) next
    out[[code]] <- v
    st_col <- .indicator_status_col(col)
    if (!is.null(st_col) && st_col %in% names(indicators_sf)) {
      out[[paste0(tolower(code), "_status")]] <- as.character(indicators_sf[[st_col]])
    }
    n_ind <- n_ind + 1L
  }
  if (n_ind == 0L) return(NULL)
  out
}


#' Embedding AlphaEarth du projet, lu dans le cache seulement
#'
#' @description
#' La vue ne telecharge rien : elle relit ce que le dernier calcul a acquis
#' (`cache/layers/aef/courant.json`, [.aef_relire_cache()]). Le paquet
#' `alphaearthnemeton` n'a pas besoin d'etre installe pour cela.
#'
#' @param indicators_sf sf. UGF du projet.
#' @param project_path Character, chemin du projet.
#' @return `list(fichier, annee)` ou `NULL` sans embedding en cache pour cette
#'   emprise.
#' @noRd
sondes_embedding <- function(indicators_sf, project_path) {
  if (is.null(indicators_sf) || !inherits(indicators_sf, "sf") ||
      !is.character(project_path) || length(project_path) != 1L ||
      is.na(project_path) || !dir.exists(project_path)) {
    return(NULL)
  }
  rel <- .aef_relire_cache(indicators_sf, project_path)
  if (is.null(rel) || is.null(rel$fichier)) return(NULL)
  list(fichier = rel$fichier, annee = rel$annee)
}


#' Signature de ce qui entre dans les sondes (detecte un resultat perime)
#'
#' Valeurs des indicateurs, geometries, embedding (fichier, taille, date, annee) et version du coeur : si l'un
#' change, le resultat en cache n'est plus le bon.
#' @noRd
sondes_signature <- function(units, embedding) {
  if (is.null(units) || is.null(embedding)) return("")
  info <- file.info(embedding$fichier)
  rlang::hash(list(
    sf::st_drop_geometry(units),
    sf::st_as_text(sf::st_geometry(units)),
    basename(embedding$fichier), embedding$annee,
    # Le fichier lui-meme (taille et date) et la version du coeur : une autre
    # version de `probe_indicator()` peut rendre un autre R2.
    info$size, as.numeric(info$mtime),
    as.character(utils::packageVersion("nemeton"))))
}

#' Fichier du cache des sondes d'un projet
#' @noRd
.sondes_cache_fichier <- function(project_path) {
  file.path(project_path, "cache", "sondes", "courant.rds")
}

#' Resultat des sondes en cache, s'il correspond a la signature
#' @return La liste du resultat, ou `NULL`.
#' @noRd
sondes_cache_lire <- function(project_path, signature) {
  if (!is.character(project_path) || length(project_path) != 1L ||
      is.na(project_path) || !nzchar(signature)) return(NULL)
  f <- .sondes_cache_fichier(project_path)
  if (!file.exists(f)) return(NULL)
  res <- tryCatch(readRDS(f), error = function(e) NULL)
  if (is.list(res) && identical(res$signature, signature) && is.null(res$erreur)) res else NULL
}

#' Range un resultat reussi dans le cache du projet (echec silencieux)
#' @noRd
sondes_cache_ecrire <- function(project_path, signature, resultat) {
  if (!is.character(project_path) || length(project_path) != 1L ||
      is.na(project_path) || !dir.exists(project_path) || !nzchar(signature) ||
      !is.null(resultat$erreur)) {
    return(invisible(FALSE))
  }
  f <- .sondes_cache_fichier(project_path)
  dir.create(dirname(f), recursive = TRUE, showWarnings = FALSE)
  tryCatch({
    saveRDS(c(resultat, list(signature = signature)), f)
    invisible(TRUE)
  }, error = function(e) invisible(FALSE))
}


#' Calcule les sondes par le coeur
#'
#' Relit l'embedding du fichier (un raster ne traverse pas un processus : on
#' passe le chemin), appelle `nemeton::probe_indicator(units, embedding)`. Une
#' erreur du coeur ne remonte pas : elle est rendue dans `erreur`, pour que
#' l'ecran la dise au lieu de planter.
#'
#' @param units Sortie de [sondes_unites()].
#' @param fichier Chemin du GeoTIFF d'embedding.
#' @param annee Annee de l'embedding (ou `NA`).
#' @param fn Fonction de calcul, le coeur par defaut (doublure en test).
#' @return `list(table, source, annee, erreur)` ; `table` est `NULL` si erreur.
#' @noRd
sondes_calculer <- function(units, fichier, annee = NA_integer_,
                            fn = nemeton::probe_indicator) {
  rien <- function(erreur) list(table = NULL, source = "aef", annee = annee, erreur = erreur)
  if (is.null(units) || !inherits(units, "sf") || nrow(units) == 0L) {
    return(rien("sans_unites"))
  }
  if (!is.character(fichier) || length(fichier) != 1L || !file.exists(fichier)) {
    return(rien("sans_embedding"))
  }
  res <- tryCatch({
    emb <- terra::rast(fichier)
    attr(emb, "embedding_source") <- "aef"
    fn(units, emb)
  }, error = function(e) e)
  if (inherits(res, "error")) return(rien(conditionMessage(res)))
  if (!is.data.frame(res)) return(rien("reponse_inattendue"))
  list(table = as.data.frame(res, stringsAsFactors = FALSE),
       source = as.character(res$source_embedding[1] %||% "aef"),
       annee = annee, erreur = NULL)
}


#' Cle i18n de la lecture d'une sonde
#' @noRd
.sd_cle_lecture <- function(lecture) {
  switch(as.character(lecture),
    voit = "sd_lect_voit",
    partiel = "sd_lect_partiel",
    angle_mort = "sd_lect_angle",
    NULL)
}

#' Cle i18n du statut d'une sonde non calculee
#' @noRd
.sd_cle_statut <- function(statut) {
  switch(as.character(statut),
    trop_peu_unites = "sd_st_trop_peu",
    variance_nulle = "sd_st_variance",
    indicateur_absent = "sd_st_absent",
    "sd_st_inconnu")
}

#' Nombre a `digits` decimales, virgule en francais, vide si `NA`
#' @noRd
.sd_nombre <- function(x, digits = 2L, language = "fr") {
  out <- formatC(x, format = "f", digits = digits)
  if (!identical(language, "en")) out <- sub(".", ",", out, fixed = TRUE)
  out[is.na(x)] <- ""
  out
}

#' Tableau a afficher : « ce que les modeles voient »
#'
#' Trie par R2 decroissant (les sondes non calculees a la suite, dans l'ordre du
#' coeur). La lecture et le statut sont traduits, jamais le code brut ;
#' `source_commune` est dit en clair : l'indicateur et l'embedding partagent une
#' source, le R2 ne prouve rien.
#'
#' @param table data.frame de `probe_indicator()`.
#' @param i18n Objet i18n.
#' @return Un data.frame de chaines (colonnes traduites), 0 ligne si `table`
#'   est vide.
#' @noRd
sondes_tableau <- function(table, i18n) {
  lang <- i18n$language %||% "fr"
  cols <- c(i18n$t("sd_col_indicateur"), i18n$t("sd_col_famille"), i18n$t("sd_col_n"),
            i18n$t("sd_col_r2"), i18n$t("sd_col_rmse"), i18n$t("sd_col_lecture"),
            i18n$t("sd_col_commune"))
  vide <- stats::setNames(
    as.data.frame(replicate(length(cols), character(0), simplify = FALSE),
                  stringsAsFactors = FALSE), cols)
  if (is.null(table) || !is.data.frame(table) || nrow(table) == 0L) return(vide)

  lab <- tryCatch(nemeton::indicator_labels(lang = lang), error = function(e) NULL)
  libelle <- function(code) {
    if (is.null(lab)) return(code)
    l <- lab$label[match(code, lab$code)]
    ifelse(is.na(l), code, paste0(code, " - ", l))
  }
  r2 <- suppressWarnings(as.numeric(table$r2))
  ordre <- order(-ifelse(is.na(r2), -Inf, r2), seq_along(r2))
  t <- table[ordre, , drop = FALSE]
  r2 <- r2[ordre]

  lecture <- vapply(seq_len(nrow(t)), function(i) {
    if (!identical(as.character(t$statut[i]), "ok")) {
      return(i18n$t(.sd_cle_statut(t$statut[i])))
    }
    cle <- .sd_cle_lecture(t$lecture[i])
    if (is.null(cle)) "" else i18n$t(cle)
  }, character(1))

  out <- data.frame(
    libelle(as.character(t$code)),
    as.character(t$famille),
    as.character(t$n),
    .sd_nombre(r2, 2L, lang),
    .sd_nombre(suppressWarnings(as.numeric(t$rmse)), 1L, lang),
    lecture,
    ifelse(t$source_commune %in% TRUE, i18n$t("sd_commune_oui"), ""),
    stringsAsFactors = FALSE, check.names = FALSE)
  names(out) <- cols
  out
}

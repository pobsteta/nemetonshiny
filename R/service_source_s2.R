# service_source_s2.R - Source Sentinel-2 du suivi sanitaire (mission 099).
#
# Aucune logique metier ici (regles 1-3) : la resolution de la source par methode
# et son calage sont dans `nemeton::s2_source_resoudre()` (nemeton >= 2026.11.47).
# Ce fichier ne fait que : persister le choix du projet, le passer aux appels
# du coeur, retrouver le bon dossier de lecture, lire les metadonnees du dernier
# run, deduire "indicatif" (calee_sur != source effective) et rendre les
# avertissements et erreurs du coeur lisibles.
#
# Un choix de source par projet : `NULL` = "par methode" (defaut, aussi pour
# les projets existants : la cle est simplement absente), `"theia"` ou
# `"sen2cor"`. Sans choix, AUCUN `source_s2` n'est passe au coeur : les appels,
# les caches et les dossiers de resultats sont ceux d'avant.

# --- Choix du projet ----------------------------------------------------------

SOURCE_S2_VALEURS <- c("theia", "sen2cor")

# Valeur du select de l'ecran (`"methode"` = NULL).
.source_s2_vers_choix <- function(source_s2) {
  if (is.null(source_s2) || !length(source_s2)) "methode" else source_s2
}

# Normalise une saisie : NULL, "", "methode" ou toute valeur inconnue -> NULL.
.source_s2_normaliser <- function(x) {
  if (is.null(x) || length(x) != 1L || is.na(x)) return(NULL)
  x <- as.character(x)
  if (x %in% SOURCE_S2_VALEURS) x else NULL
}

# Choix enregistre dans les metadonnees du projet (`metadata$source_s2`).
project_source_s2 <- function(metadata) {
  .source_s2_normaliser(metadata$source_s2)
}

# Enregistre le choix. `NULL` (par methode) retire la cle : le projet redevient
# identique a un projet qui n'a jamais ouvert ce reglage.
set_project_source_s2 <- function(project_id, source_s2 = NULL) {
  project_path <- get_project_path(project_id)
  if (is.null(project_path) || !dir.exists(project_path)) {
    cli::cli_abort("Project not found: {project_id}")
  }
  update_project_metadata(
    project_id, list(source_s2 = .source_s2_normaliser(source_s2)),
    project_path = project_path)
  invisible(TRUE)
}

# Arguments a ajouter a un appel `nemeton::` : rien sans choix, sinon
# `source_s2`. C'est ce qui garantit des appels inchanges "par methode".
.source_s2_args <- function(source_s2) {
  s <- .source_s2_normaliser(source_s2)
  if (is.null(s)) list() else list(source_s2 = s)
}

# --- Dossiers de lecture ------------------------------------------------------

# Dossier frere d'une source : Sen2Cor = le dossier tel quel, THEIA =
# `<dossier>_theia` (meme regle que le coeur, idempotente).
.source_s2_dossier <- function(dossier, source_s2) {
  if (is.null(dossier) || length(dossier) != 1L || is.na(dossier) ||
      !nzchar(dossier)) return(dossier)
  if (!identical(.source_s2_normaliser(source_s2), "theia")) return(dossier)
  base <- sub("/+$", "", dossier)
  if (endsWith(base, "_theia")) base else paste0(base, "_theia")
}

# Dossier ou lire les resultats du dernier run. Sans choix, ou en Sen2Cor : le
# dossier d'avant. En THEIA : `<dossier>_theia` s'il porte un resultat (de la
# zone, quand elle est connue) ; sinon le dossier d'origine, ou le coeur a
# range un run qui a bascule sur Sen2Cor (repli).
.source_s2_dossier_lecture <- function(dossier, source_s2, zone_id = NULL) {
  d <- .source_s2_dossier(dossier, source_s2)
  if (identical(d, dossier)) return(dossier)
  cible <- if (is.null(zone_id)) d else file.path(d, sprintf("zone_%s", zone_id))
  if (dir.exists(cible)) d else dossier
}

# --- Metadonnees du dernier run -----------------------------------------------

.source_s2_lire_json <- function(f) {
  if (length(f) != 1L || is.na(f) || !file.exists(f)) return(NULL)
  tryCatch(jsonlite::read_json(f, simplifyVector = TRUE),
           error = function(e) NULL)
}

# Le plus recent des fichiers `nom` sous `racine` (recursif), NA sinon.
.source_s2_plus_recent <- function(racine, nom, motif = NULL) {
  if (is.null(racine) || !nzchar(racine) || !dir.exists(racine)) {
    return(NA_character_)
  }
  f <- list.files(racine, pattern = paste0("^", gsub("\\.", "\\\\.", nom), "$"),
                  recursive = TRUE, full.names = TRUE)
  if (!is.null(motif)) f <- f[grepl(motif, f)]
  if (!length(f)) return(NA_character_)
  f[which.max(file.mtime(f))]
}

# Metadonnees du dernier run d'une methode (liste, ou NULL sans run).
#   icd       : `<result_dir>/zone_<id>/meta.json`
#   fordead   : `run_meta.json` du bundle de la zone, sous `<dossier>/zone_<id>`
#   reconfort : `run_meta.json` du dernier run sous le cache RECONFORT
#   fast      : temoin ecrit par l'app a la fin de l'ingestion
.source_s2_meta_run <- function(methode, dossier, zone_id = NULL) {
  if (is.null(dossier) || !nzchar(dossier)) return(NULL)
  zid <- if (is.null(zone_id)) NULL else as.character(zone_id)
  switch(
    methode,
    icd = if (is.null(zid)) NULL else
      .source_s2_lire_json(file.path(dossier, sprintf("zone_%s", zid),
                                     "meta.json")),
    fordead = .source_s2_lire_json(.source_s2_plus_recent(
      if (is.null(zid)) dossier else file.path(dossier, sprintf("zone_%s", zid)),
      "run_meta.json")),
    reconfort = .source_s2_lire_json(.source_s2_plus_recent(
      dossier, "run_meta.json",
      motif = if (is.null(zid)) NULL else sprintf("(zone_?|z)%s([^0-9]|$)", zid))),
    fast = .source_s2_lire_json(file.path(dossier, "app_source_s2_fast.json")),
    NULL)
}

# Temoin FAST : l'ingestion rend `source_s2` et `repli_sen2cor` ; l'app les range
# a cote du cache pour que la carte les relise sans relancer le calcul.
.source_s2_ecrire_temoin_fast <- function(cache_dir, resume, choix = NULL) {
  if (is.null(cache_dir) || !nzchar(cache_dir)) return(invisible(FALSE))
  f <- file.path(cache_dir, "app_source_s2_fast.json")
  # Run "par methode" : le coeur ne dit rien, le temoin d'un run precedent
  # ne doit pas survivre.
  if (is.null(resume$source_s2)) {
    if (file.exists(f)) tryCatch(unlink(f), error = function(e) NULL)
    return(invisible(FALSE))
  }
  info <- tryCatch(nemeton::s2_source_resoudre("fast", resume$source_s2),
                   error = function(e) NULL)
  meta <- list(source_s2 = resume$source_s2,
               source_s2_demandee = choix %||% resume$source_s2,
               calee_sur = info$calee_sur %||% "sen2cor",
               repli_sen2cor = isTRUE(resume$repli_sen2cor))
  if (!is.null(info) && !is.na(info$mention)) meta$mention_source <- info$mention
  tryCatch({
    dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
    writeLines(jsonlite::toJSON(meta, auto_unbox = TRUE), f)
    invisible(TRUE)
  }, error = function(e) invisible(FALSE))
}

# Cause de repli, en une ligne lisible (jamais de pile d'appels).
.source_s2_cause_lisible <- function(cause, i18n = NULL) {
  cause <- paste(as.character(unlist(cause)), collapse = " ; ")
  if (!nzchar(cause) || is.na(cause)) return("")
  .message_erreur_lisible(cause, i18n, max_chars = 200L)
}

# --- Source effective, "indicatif", repli -----------------------------------

#' Source Sentinel-2 effective d'une methode et ce qu'il faut en dire
#'
#' `indicatif` vaut TRUE quand `calee_sur` differe de la source effective (il
#' n'existe pas de champ `validee` : decision de Pascal, 2026-11-27).
#'
#' @param methode `"fast"`, `"fordead"`, `"reconfort"` ou `"icd"`.
#' @param meta Metadonnees du dernier run (liste) ou `NULL`.
#' @param choix Choix du projet (`NULL`, `"theia"`, `"sen2cor"`).
#' @return Liste : `source`, `calee_sur`, `indicatif`, `mention`, `repli`,
#'   `cause`.
#' @noRd
source_s2_info <- function(methode, meta = NULL, choix = NULL) {
  choix <- .source_s2_normaliser(choix)
  defaut <- nemeton::s2_source_resoudre(methode)
  source <- .source_s2_normaliser(meta$source_s2)
  if (is.null(source) && identical(methode, "reconfort") &&
      identical(meta$source_reflectances, "sen2cor")) {
    source <- "sen2cor"
  }
  source <- source %||% choix %||% defaut$source_s2
  calee <- .source_s2_normaliser(meta$calee_sur) %||% defaut$calee_sur
  indicatif <- !identical(source, calee)
  mention <- NULL
  if (indicatif) {
    m <- meta$mention_source %||% meta$mention
    if (is.null(m) || !length(m) || is.na(m[[1L]]) || !nzchar(m[[1L]])) {
      m <- nemeton::s2_source_resoudre(methode, source)$mention
    }
    if (!is.na(m)) mention <- as.character(m[[1L]])
  }
  causes <- unlist(meta$causes_repli)
  repli <- isTRUE(meta$repli_sen2cor) || length(causes) > 0L
  cause <- if (!is.null(meta$cause_repli)) meta$cause_repli else causes
  list(source = source, calee_sur = calee, indicatif = indicatif,
       mention = mention, repli = repli,
       cause = if (repli) paste(as.character(unlist(cause)), collapse = " ; ")
               else NA_character_)
}

# Libelle d'une source.
.source_s2_etiquette <- function(source, i18n) {
  switch(source, theia = i18n$t("s2_src_theia"),
         sen2cor = i18n$t("s2_src_sen2cor"), source)
}

#' Ligne discrete "Source : ..." d'une carte du suivi sanitaire
#'
#' Le badge "indicatif" est un avertissement (`text-bg-warning`), pas du
#' contenu IA : jamais l'ambre `bg-ia`.
#' @param info Resultat de [source_s2_info()].
#' @param i18n Objet i18n.
#' @noRd
.source_s2_ligne_ui <- function(info, i18n) {
  if (is.null(info)) return(NULL)
  badge <- if (isTRUE(info$indicatif)) {
    htmltools::tags$span(
      class = "badge text-bg-warning ms-2",
      title = info$mention %||% i18n$t("s2_indicatif_aide"),
      i18n$t("s2_indicatif"))
  }
  repli <- if (isTRUE(info$repli)) {
    htmltools::div(
      class = "small text-warning-emphasis",
      sprintf(i18n$t("s2_repli_ligne"),
              .source_s2_cause_lisible(info$cause, i18n)))
  }
  htmltools::div(
    class = "small text-muted mb-1",
    sprintf(i18n$t("s2_source_ligne"), .source_s2_etiquette(info$source, i18n)),
    badge, repli)
}

# --- Avertissements et erreurs du coeur ---------------------------------------

S2_CLASSES_AVERTISSEMENT <- c(
  "nemeton_s2_repli_sen2cor", "nemeton_s2_source_non_calee",
  "nemeton_reconfort_repli_archive", "nemeton_reconfort_repli_sen2cor")

S2_CLASSES_CLES_THEIA <- c(
  "nemeton_s2_theia_refuse", "nemeton_s2_theia_sans_cles",
  "nemeton_reconfort_stac_refuse")

# Un message d'erreur ou une condition parle-t-il des cles THEIA ? La classe
# se perd a la frontiere du process enfant : on lit aussi le texte du coeur.
.source_s2_erreur_cles_theia <- function(x) {
  if (inherits(x, "condition")) {
    if (inherits(x, S2_CLASSES_CLES_THEIA)) return(TRUE)
    x <- conditionMessage(x)
  }
  txt <- paste(as.character(x %||% ""), collapse = "\n")
  grepl(paste0("needs THEIA credentials|THEIA STAC catalogue .{0,200}refused|",
               "THEIA refused the request|STAC catalogue .{0,200}refused"),
        cli::ansi_strip(txt), perl = TRUE)
}

# Message lisible d'une erreur de cles THEIA : renvoie vers Sources & parametres,
# sans pile d'appels. NULL quand l'erreur n'est pas de cette famille.
.source_s2_message_erreur <- function(x, i18n = NULL) {
  if (!.source_s2_erreur_cles_theia(x)) return(NULL)
  i18n <- i18n %||% get_i18n("fr")
  i18n$t("s2_err_cles_theia")
}

# Message lisible d'un avertissement du coeur (condition), NULL si la classe
# n'est pas l'une des quatre.
.source_s2_message_avertissement <- function(w, i18n) {
  if (!inherits(w, S2_CLASSES_AVERTISSEMENT)) return(NULL)
  cause <- .source_s2_cause_lisible(
    sub("^.*?\\(([^)]*)\\).*$", "\\1",
        cli::ansi_strip(conditionMessage(w)), perl = TRUE), i18n)
  if (inherits(w, "nemeton_s2_source_non_calee")) {
    return(i18n$t("s2_notif_hors_calage"))
  }
  if (inherits(w, "nemeton_reconfort_repli_archive")) {
    return(sprintf(i18n$t("s2_notif_repli_voie"), cause))
  }
  sprintf(i18n$t("s2_notif_repli"), cause)
}

# Notifications lisibles de fin de run, deduites des metadonnees (elles
# traversent la frontiere de process, contrairement aux avertissements) : repli
# et resultat indicatif. Rend la liste des textes ; `notifier` les affiche.
.source_s2_notifications <- function(methode, info, i18n) {
  if (is.null(info)) return(character())
  out <- character()
  if (isTRUE(info$repli)) {
    out <- c(out, sprintf(i18n$t("s2_notif_repli"),
                          .source_s2_cause_lisible(info$cause, i18n)))
  }
  if (isTRUE(info$indicatif)) {
    out <- c(out, sprintf(i18n$t("s2_notif_hors_calage"),
                          toupper(methode),
                          .source_s2_etiquette(info$calee_sur, i18n),
                          .source_s2_etiquette(info$source, i18n)))
  }
  out
}

# Affiche ces notifications (presentation seule).
.source_s2_notifier <- function(methode, dossier, zone_id, choix, i18n) {
  meta <- .source_s2_meta_run(methode, dossier, zone_id)
  if (is.null(meta)) return(invisible(character()))
  info <- source_s2_info(methode, meta, choix)
  msgs <- .source_s2_notifications(methode, info, i18n)
  for (m in msgs) shiny::showNotification(m, type = "warning", duration = 12)
  invisible(msgs)
}

# --- Raccourcis par projet (utilises par les modules du suivi sanitaire) -------

# Dossier ou lire les metadonnees et les resultats d'une methode, selon le choix
# du projet. FAST : le cache Sentinel-2 d'origine (le temoin y est ecrit).
.source_s2_dossier_methode <- function(methode, project_path, choix = NULL,
                                       zone_id = NULL) {
  if (is.null(project_path) || length(project_path) != 1L ||
      is.na(project_path) || !nzchar(project_path)) return(NULL)
  base <- file.path(project_path, "cache", "layers")
  switch(
    methode,
    fast = file.path(base, "sentinel2"),
    fordead = .source_s2_dossier_lecture(file.path(base, "fordead"), choix,
                                         zone_id),
    reconfort = file.path(base, "reconfort"),
    icd = .source_s2_dossier_lecture(.icd_result_dir(project_path), choix,
                                     zone_id),
    NULL)
}

# Info d'affichage d'une methode pour un projet (liste de `source_s2_info()`).
.source_s2_info_projet <- function(methode, project, zone_id = NULL) {
  choix <- project_source_s2(project$metadata)
  d <- .source_s2_dossier_methode(methode, project$path, choix, zone_id)
  source_s2_info(methode, .source_s2_meta_run(methode, d, zone_id), choix)
}

# Notifications de fin de run pour un projet.
.source_s2_notifier_projet <- function(methode, project, zone_id, i18n) {
  choix <- project_source_s2(project$metadata)
  .source_s2_notifier(methode,
                      .source_s2_dossier_methode(methode, project$path, choix,
                                                 zone_id),
                      zone_id, choix, i18n)
}

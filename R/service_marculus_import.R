# service_marculus_import.R - retour du martelage Marculus dans le Plan d'actions
#
# Le telephone partage ses donnees en JSON, sous deux formes au meme schema
# (`marculus/.../SauvegardeRepository.kt`) :
#   * un `.marsync` par contexte (partage de synchronisation) ;
#   * la sauvegarde complete (tous les contextes).
# Racine : `{version, contextes: [...], tiges: [...], configs: [...]}`.
#
# Un contexte exporte par Nemeton porte l'`id` de son ACTION
# (`marculus_context_from_action()`) : c'est la cle d'appariement. Un contexte
# cree a la main sur le telephone n'a pas d'action ici ; il est compte, pas
# applique.
#
# Les tiges forment un journal APPEND-ONLY : un `+` ajoute des tiges, une
# ANNULATION en retire (jamais d'effacement). Le total d'une cellule
# essence x classe est donc sum(PLUS) - sum(ANNULATION)
# (`TotauxMartelage.kt`), et c'est ce total net qui remonte dans l'action.
#
# Le terrain fait foi : statut et date du telephone remplacent ceux de l'app,
# l'audit du plan garde l'ancienne valeur.
#
# Troisieme forme (Marculus apres v0.47.0, commit 8848a67) : le CSV de contexte en
# `FormatCsv;2` (`ExportCsv.kt`), qui porte les memes cles (ContexteId, Uuid,
# Statut, DateMartelage, Modifie) et se lit donc comme un `.marsync`. Un CSV
# sans `FormatCsv` (format 1) n'a ni id ni uuid : il est refuse.


# ---- Lecture ----------------------------------------------------------

#' Statuses of a Marculus context, back to the action plan's
#' @noRd
MARCULUS_STATUTS_RETOUR <- stats::setNames(names(MARCULUS_STATUTS),
                                           unname(MARCULUS_STATUTS))

#' Empty stems table, with every column the import relies on
#' @noRd
.marculus_tiges_vides <- function() {
  data.frame(uuid = character(0), contexteId = character(0),
             essence = character(0), classe = integer(0),
             action = character(0), horodatage = numeric(0),
             quantite = integer(0), hauteurTexte = character(0),
             qualiteArbre = character(0), latitude = numeric(0),
             longitude = numeric(0), operateur = character(0),
             parcelle = character(0), qualiteFix = character(0),
             precisionM = numeric(0), modifie = numeric(0),
             stringsAsFactors = FALSE)
}

# Aligne un data.frame de tiges sur les colonnes attendues (champs optionnels
# absents -> NA), dans l'ordre de `.marculus_tiges_vides()`.
.marculus_tiges_normaliser <- function(df) {
  modele <- .marculus_tiges_vides()
  if (is.null(df) || !is.data.frame(df) || nrow(df) == 0L) return(modele)
  for (col in names(modele)) {
    if (!col %in% names(df)) df[[col]] <- rep(NA, nrow(df))
    df[[col]] <- methods::as(df[[col]], class(modele[[col]]))
  }
  df$quantite[is.na(df$quantite)] <- 1L
  df$modifie[is.na(df$modifie)] <- 0
  df[, names(modele), drop = FALSE]
}

#' Read one or several Marculus JSON exports
#'
#' @param chemins Character. Paths to `.marsync` files or full backups.
#' @return A list: `contextes` (data.frame: id, nom, statut, dateMartelage,
#'   modifie), `tiges` (see `.marculus_tiges_vides()`), `n_fichiers`,
#'   `illisibles` (basenames that are not Marculus exports).
#' @noRd
marculus_lire_exports <- function(chemins) {
  ctx <- list(); tig <- list(); illisibles <- character()
  csv_anciens <- character()
  for (p in chemins) {
    if (.marculus_est_csv(p)) {
      lu <- .marculus_lire_csv(p)
      if (identical(lu, "format1")) { csv_anciens <- c(csv_anciens, basename(p)); next }
      if (is.null(lu)) { illisibles <- c(illisibles, basename(p)); next }
      ctx[[length(ctx) + 1L]] <- lu$contexte
      if (nrow(lu$tiges)) tig[[length(tig) + 1L]] <- lu$tiges
      next
    }
    j <- tryCatch(jsonlite::fromJSON(p, simplifyDataFrame = TRUE),
                  error = function(e) NULL)
    if (!is.list(j) || is.null(j$contextes) || !is.data.frame(j$contextes) ||
        !"id" %in% names(j$contextes)) {
      illisibles <- c(illisibles, basename(p))
      next
    }
    c0 <- j$contextes
    ctx[[length(ctx) + 1L]] <- data.frame(
      id            = as.character(c0$id),
      nom           = as.character(c0$nom %||% NA_character_),
      statut        = as.character(c0$statut %||% NA_character_),
      dateMartelage = as.numeric(c0$dateMartelage %||% NA_real_),
      modifie       = as.numeric(c0$modifie %||% 0),
      stringsAsFactors = FALSE)
    if (is.data.frame(j$tiges) && nrow(j$tiges) > 0L) {
      tig[[length(tig) + 1L]] <- .marculus_tiges_normaliser(j$tiges)
    }
  }
  contextes <- if (length(ctx)) do.call(rbind, ctx) else
    data.frame(id = character(0), nom = character(0), statut = character(0),
               dateMartelage = numeric(0), modifie = numeric(0))
  # Un meme contexte partage deux fois : garder sa version la plus recente.
  if (nrow(contextes) > 1L) {
    contextes <- contextes[order(-contextes$modifie), , drop = FALSE]
    contextes <- contextes[!duplicated(contextes$id), , drop = FALSE]
  }
  tiges <- if (length(tig)) do.call(rbind, tig) else .marculus_tiges_vides()
  list(contextes = contextes, tiges = .marculus_tiges_union(tiges),
       n_fichiers = length(chemins) - length(illisibles) - length(csv_anciens),
       illisibles = illisibles, csv_anciens = csv_anciens)
}

# Un CSV Marculus commence par `Contexte;` ; le JSON par `{`.
.marculus_est_csv <- function(chemin) {
  l1 <- tryCatch(readLines(chemin, n = 1L, warn = FALSE, encoding = "UTF-8"),
                 error = function(e) character(0))
  length(l1) == 1L && startsWith(sub("^\ufeff", "", l1), "Contexte;")
}

# Millisecondes depuis l'epoque d'un horodatage ISO-8601 (`Instant.toString()`,
# fraction de seconde facultative, suffixe Z).
.marculus_iso_ms <- function(x) {
  t <- as.POSIXct(sub("Z$", "", x), format = "%Y-%m-%dT%H:%M:%OS", tz = "UTC")
  round(as.numeric(t) * 1000)
}

#' Read a Marculus context CSV in `FormatCsv;2`
#'
#' Layout (`ExportCsv.kt`): `key;value` header lines up to the first blank
#' line, then a `TOTAUX` section (ignored - it is derived) and a `JOURNAL`
#' section whose header names its columns. Fields are escaped the CSV way
#' (double quotes); decimals use a dot.
#'
#' @param chemin Path to the CSV.
#' @return `list(contexte, tiges)` shaped like [marculus_lire_exports()]'s,
#'   `"format1"` for a CSV without `FormatCsv` (no ids: not importable), or
#'   `NULL` when unreadable.
#' @noRd
.marculus_lire_csv <- function(chemin) {
  tryCatch({
    lignes <- readLines(chemin, warn = FALSE, encoding = "UTF-8")
    lignes[1] <- sub("^\ufeff", "", lignes[1])
    fin_entete <- which(!nzchar(trimws(lignes)))[1]
    if (is.na(fin_entete)) fin_entete <- length(lignes) + 1L
    kv <- utils::read.table(text = lignes[seq_len(fin_entete - 1L)], sep = ";",
                            quote = "\"", header = FALSE, fill = TRUE,
                            colClasses = "character", comment.char = "",
                            na.strings = character(0))
    entete <- as.list(stats::setNames(kv[[2]], kv[[1]]))
    # `$` sur une liste rend NULL pour une cle absente (un `[[` sur un vecteur
    # nomme leverait une erreur).
    fmt <- suppressWarnings(as.integer(entete$FormatCsv %||% NA))
    if (is.na(fmt) || fmt < 2L) return("format1")
    if (is.null(entete$ContexteId) || !nzchar(entete$ContexteId)) return(NULL)
    dm <- entete$DateMartelage %||% ""
    contexte <- data.frame(
      id            = entete$ContexteId,
      nom           = entete$Contexte %||% NA_character_,
      statut        = entete$Statut %||% NA_character_,
      dateMartelage = if (nzchar(dm))
        as.numeric(as.POSIXct(dm, tz = "UTC")) * 1000 else NA_real_,
      modifie       = suppressWarnings(as.numeric(entete$Modifie %||% 0)),
      stringsAsFactors = FALSE)

    j <- which(trimws(lignes) == "JOURNAL")[1]
    tiges <- .marculus_tiges_vides()
    if (!is.na(j) && j < length(lignes)) {
      corps <- lignes[(j + 1L):length(lignes)]
      corps <- corps[nzchar(trimws(corps))]
      if (length(corps) > 1L) {
        jr <- utils::read.table(text = corps, sep = ";", quote = "\"",
                                header = TRUE, colClasses = "character",
                                comment.char = "", na.strings = "",
                                check.names = FALSE)
        num <- function(x) suppressWarnings(as.numeric(x))
        tiges <- .marculus_tiges_normaliser(data.frame(
          uuid = jr$Uuid, contexteId = contexte$id, essence = jr$Essence,
          classe = as.integer(jr$Classe), action = jr$Action,
          horodatage = .marculus_iso_ms(jr$Horodatage),
          quantite = as.integer(jr$Quantite), hauteurTexte = jr$Hauteur,
          qualiteArbre = jr$QualiteArbre, latitude = num(jr$Latitude),
          longitude = num(jr$Longitude), operateur = jr$Operateur,
          parcelle = jr$Parcelle, qualiteFix = jr$QualiteFix,
          precisionM = num(jr$Precision_m), modifie = num(jr$Modifie),
          stringsAsFactors = FALSE))
        tiges <- tiges[!is.na(tiges$uuid) & nzchar(tiges$uuid), , drop = FALSE]
      }
    }
    list(contexte = contexte, tiges = tiges)
  }, error = function(e) NULL)
}

#' Union of stems by uuid, keeping each stem's latest version
#'
#' The phone's own merge (`fusionnerJson()`) is a union by uuid; importing the
#' same share twice must not double the counts.
#'
#' @param tiges A stems data.frame.
#' @return The same, one row per uuid.
#' @noRd
.marculus_tiges_union <- function(tiges) {
  tiges <- .marculus_tiges_normaliser(tiges)
  if (nrow(tiges) < 2L) return(tiges)
  tiges <- tiges[order(-tiges$modifie), , drop = FALSE]
  tiges <- tiges[!duplicated(tiges$uuid), , drop = FALSE]
  tiges[order(tiges$horodatage), , drop = FALSE]
}


# ---- Stockage ---------------------------------------------------------

#' Path of a project's imported Marculus stems
#' @noRd
.marculus_tiges_chemin <- function(project_id) {
  path <- get_project_path(project_id)
  if (is.null(path)) return(NULL)
  file.path(path, "data", "marculus_tiges.json")
}

#' Stems already imported into a project
#'
#' @param project_id Character.
#' @return A stems data.frame, possibly empty.
#' @noRd
marculus_charger_tiges <- function(project_id) {
  p <- .marculus_tiges_chemin(project_id)
  if (is.null(p) || !file.exists(p)) return(.marculus_tiges_vides())
  df <- tryCatch(jsonlite::fromJSON(p, simplifyDataFrame = TRUE),
                 error = function(e) NULL)
  .marculus_tiges_normaliser(df)
}

#' Save a project's stems, atomically
#' @noRd
.marculus_sauver_tiges <- function(project_id, tiges) {
  p <- .marculus_tiges_chemin(project_id)
  if (is.null(p)) return(FALSE)
  dir.create(dirname(p), recursive = TRUE, showWarnings = FALSE)
  tmp <- paste0(p, ".tmp")
  jsonlite::write_json(tiges, tmp, na = "null", digits = NA, pretty = FALSE)
  file.rename(tmp, p)
}


# ---- Totaux et application -------------------------------------------

#' Net stem counts per context, species and class
#'
#' Same rule as the phone (`TotauxMartelage.kt`): PLUS adds its quantity,
#' ANNULATION removes it.
#'
#' @param tiges A stems data.frame.
#' @return A data.frame `contexteId, essence, classe, tiges`, zero cells
#'   dropped.
#' @noRd
marculus_totaux <- function(tiges) {
  tiges <- .marculus_tiges_normaliser(tiges)
  vide <- data.frame(contexteId = character(0), essence = character(0),
                     classe = integer(0), tiges = integer(0))
  if (nrow(tiges) == 0L) return(vide)
  delta <- ifelse(toupper(tiges$action) == "ANNULATION", -1L, 1L) *
    as.integer(tiges$quantite)
  agg <- stats::aggregate(delta,
                          by = list(contexteId = tiges$contexteId,
                                    essence = tiges$essence,
                                    classe = tiges$classe),
                          FUN = sum)
  names(agg)[4] <- "tiges"
  agg <- agg[agg$tiges != 0L, , drop = FALSE]
  agg[order(agg$contexteId, agg$essence, agg$classe), , drop = FALSE]
}

#' Apply returned Marculus contexts to an action plan
#'
#' For each context whose id is an action of the plan: status, marking date
#' (`date_martelage`, `YYYY-MM-DD`), target year when it stays inside the
#' plan's horizon, and the net number of marked stems (`quantite$nb_tiges`).
#' The field wins over the app; every change goes through
#' [update_action_in_plan()] and is audited.
#'
#' @param plan The action plan.
#' @param contextes Contexts from [marculus_lire_exports()].
#' @param tiges All stems of the project (already merged).
#' @param user Character. Acting user, for the audit.
#' @param annee_base Integer. Current year (offsets are counted from it).
#' @return A list: `plan`, `n_actions` (updated), `n_orphelins` (contexts
#'   with no matching action), `ids` (updated action ids).
#' @noRd
marculus_appliquer_retour <- function(plan, contextes, tiges, user = NULL,
                                      annee_base = as.integer(format(Sys.Date(), "%Y"))) {
  ids_plan <- vapply(plan$actions %||% list(), function(a) a$id %||% "", "")
  horizon <- as.integer(plan$horizon_annees %||% 20L)
  totaux <- marculus_totaux(tiges)
  n_par_ctx <- if (nrow(totaux)) tapply(totaux$tiges, totaux$contexteId, sum) else integer(0)

  maj <- character(); orphelins <- 0L
  for (i in seq_len(nrow(contextes))) {
    cid <- contextes$id[i]
    if (!cid %in% ids_plan) { orphelins <- orphelins + 1L; next }
    action <- plan$actions[[match(cid, ids_plan)]]
    upd <- list()

    st <- unname(MARCULUS_STATUTS_RETOUR[toupper(contextes$statut[i] %||% "")])
    if (length(st) == 1L && !is.na(st)) upd$statut <- st

    dm <- contextes$dateMartelage[i]
    if (!is.na(dm)) {
      d <- as.Date(as.POSIXct(dm / 1000, origin = "1970-01-01", tz = "UTC"))
      upd$date_martelage <- format(d, "%Y-%m-%d")
      offset <- as.integer(format(d, "%Y")) - annee_base
      if (offset >= 1L && offset <= horizon) upd$annee_cible <- offset
    }

    if (cid %in% names(n_par_ctx)) {
      q <- action$quantite %||% list()
      q$nb_tiges <- as.integer(n_par_ctx[[cid]])
      upd$quantite <- q
    }

    # Ne garder que ce qui change : un reimport identique ne touche a rien.
    upd <- upd[!vapply(names(upd), function(k) identical(action[[k]], upd[[k]]),
                       logical(1))]
    if (length(upd) == 0L) next
    plan <- update_action_in_plan(plan, cid, upd, user = user)
    maj <- c(maj, cid)
  }
  list(plan = plan, n_actions = length(maj), n_orphelins = orphelins,
       ids = maj)
}

#' Import Marculus exports into a project
#'
#' Reads the files, merges their stems with those already imported (union by
#' uuid), saves them, applies the contexts to the plan and saves it.
#'
#' @param project_id Character.
#' @param chemins Character. Uploaded files.
#' @param user Character. Acting user.
#' @return A list: `plan` (updated, or `NULL` when nothing was readable),
#'   `n_actions`, `n_orphelins`, `n_tiges_nouvelles`, `n_tiges`,
#'   `illisibles`, `ids`.
#' @noRd
marculus_importer <- function(project_id, chemins, user = NULL) {
  lu <- marculus_lire_exports(chemins)
  vide <- list(plan = NULL, n_actions = 0L, n_orphelins = 0L,
               n_tiges_nouvelles = 0L, n_tiges = 0L,
               illisibles = lu$illisibles, csv_anciens = lu$csv_anciens,
               ids = character())
  if (nrow(lu$contextes) == 0L) return(vide)

  avant <- marculus_charger_tiges(project_id)
  tiges <- .marculus_tiges_union(rbind(avant, lu$tiges))
  n_nouvelles <- length(setdiff(tiges$uuid, avant$uuid))

  plan <- load_action_plan(project_id)
  if (is.null(plan)) return(vide)
  res <- marculus_appliquer_retour(plan, lu$contextes, tiges, user = user)

  .marculus_sauver_tiges(project_id, tiges)
  if (res$n_actions > 0L) save_action_plan(project_id, res$plan)

  list(plan = res$plan, n_actions = res$n_actions,
       n_orphelins = res$n_orphelins, n_tiges_nouvelles = n_nouvelles,
       n_tiges = nrow(tiges), illisibles = lu$illisibles,
       csv_anciens = lu$csv_anciens, ids = res$ids)
}

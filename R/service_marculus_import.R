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
# Troisieme forme (Marculus v0.48.0, volumes en format 3 ensuite) : le CSV de contexte en
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
             # Volumes calcules sur le telephone (Marculus, format 3) : valeurs
             # UNITAIRES, pour une tige. NA quand le fichier n'en porte pas.
             volumeTigeM3 = numeric(0), volumeHouppierM3 = numeric(0),
             volumeTotalM3 = numeric(0), surfaceTerriereM2 = numeric(0),
             cubage = character(0),
             # Mode de mesure du CONTEXTE (DIAMETRE / CIRCONFERENCE), recopie
             # sur chaque tige a l'import : il dit comment lire `classe`.
             mode = character(0),
             stringsAsFactors = FALSE)
}

# Colonnes de volume d'un contexte (totaux NETS calcules sur le telephone).
MARCULUS_VOLUMES_CONTEXTE <- c("volumeTigeTotalM3", "volumeTotalM3",
                               "surfaceTerriereTotaleM2", "nbTigesNonCubees")

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
  csv_anciens <- character(); vides <- character()
  for (p in chemins) {
    # Un fichier vide arrive tel quel (transfert du telephone interrompu,
    # synchronisation cloud pas encore faite) : le dire, plutot que
    # « illisible », qui fait chercher un probleme de format.
    if (!file.exists(p) || isTRUE(file.size(p) == 0)) {
      vides <- c(vides, basename(p))
      next
    }
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
    d0 <- data.frame(
      id            = as.character(c0$id),
      nom           = as.character(c0$nom %||% NA_character_),
      statut        = as.character(c0$statut %||% NA_character_),
      dateMartelage = as.numeric(c0$dateMartelage %||% NA_real_),
      modifie       = as.numeric(c0$modifie %||% 0),
      stringsAsFactors = FALSE)
    for (v in MARCULUS_VOLUMES_CONTEXTE) {
      d0[[v]] <- suppressWarnings(as.numeric(c0[[v]] %||% NA_real_))
    }
    ctx[[length(ctx) + 1L]] <- d0
    if (is.data.frame(j$tiges) && nrow(j$tiges) > 0L) {
      t0 <- .marculus_tiges_normaliser(j$tiges)
      modes <- stats::setNames(
        rep_len(as.character(c0$mode %||% NA_character_), nrow(c0)),
        as.character(c0$id))
      t0$mode <- unname(modes[t0$contexteId])
      tig[[length(tig) + 1L]] <- t0
    }
  }
  contextes <- if (length(ctx)) do.call(rbind, ctx) else {
    vide_ctx <- data.frame(id = character(0), nom = character(0),
                           statut = character(0), dateMartelage = numeric(0),
                           modifie = numeric(0))
    for (v in MARCULUS_VOLUMES_CONTEXTE) vide_ctx[[v]] <- numeric(0)
    vide_ctx
  }
  # Un meme contexte partage deux fois : garder sa version la plus recente.
  if (nrow(contextes) > 1L) {
    contextes <- contextes[order(-contextes$modifie), , drop = FALSE]
    contextes <- contextes[!duplicated(contextes$id), , drop = FALSE]
  }
  tiges <- if (length(tig)) do.call(rbind, tig) else .marculus_tiges_vides()
  list(contextes = contextes, tiges = .marculus_tiges_union(tiges),
       n_fichiers = length(chemins) - length(illisibles) - length(csv_anciens) -
         length(vides),
       illisibles = illisibles, csv_anciens = csv_anciens, vides = vides)
}

# Un CSV Marculus commence par `Contexte;` ; le JSON par `{`.
.marculus_est_csv <- function(chemin) {
  l1 <- tryCatch(readLines(chemin, n = 1L, warn = FALSE, encoding = "UTF-8"),
                 error = function(e) character(0))
  length(l1) == 1L && startsWith(sub("^\ufeff", "", l1), "Contexte;")
}

# Qualite du fix GNSS : le CSV ecrit le LIBELLE (`QualiteFix.libelle`, « RTK
# fixe »), le `.marsync` le NOM de l'enum (`RTK_FIXE`) - reponse Marculus
# v0.48.0. On ramene tout au nom, pour qu'une meme tige ne revienne pas sous
# deux formes selon le fichier (`FixGnss.kt`).
MARCULUS_QUALITE_FIX <- c(
  "Pas de fix" = "INVALIDE", "Autonome" = "AUTONOME", "DGPS" = "DGPS",
  "PPS" = "PPS", "RTK fixe" = "RTK_FIXE", "RTK flottant" = "RTK_FLOAT",
  "Estim\u00e9" = "ESTIME", "Manuel" = "MANUEL", "Simulation" = "SIMULATION"
)
.marculus_qualite_fix_nom <- function(x) {
  nom <- unname(MARCULUS_QUALITE_FIX[x])
  ifelse(is.na(nom), x, nom)
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
    # Format 3 : totaux nets du telephone ; absents au format 2 (NA).
    for (v in MARCULUS_VOLUMES_CONTEXTE) {
      cle <- paste0(toupper(substr(v, 1, 1)), substring(v, 2))
      contexte[[v]] <- suppressWarnings(as.numeric(entete[[cle]] %||% NA))
    }

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
          parcelle = jr$Parcelle,
          qualiteFix = .marculus_qualite_fix_nom(jr$QualiteFix),
          precisionM = num(jr$Precision_m), modifie = num(jr$Modifie),
          volumeTigeM3 = num(jr$VolumeTigeM3 %||% NA),
          volumeHouppierM3 = num(jr$VolumeHouppierM3 %||% NA),
          volumeTotalM3 = num(jr$VolumeTotalM3 %||% NA),
          surfaceTerriereM2 = num(jr$SurfaceTerriereM2 %||% NA),
          cubage = jr$Cubage %||% NA_character_,
          mode = entete$Mode %||% NA_character_,
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
  # `order()` est stable : a `modifie` egal, la premiere ligne passee gagne.
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
                     classe = integer(0), tiges = integer(0),
                     volume_m3 = numeric(0))
  if (nrow(tiges) == 0L) return(vide)
  delta <- ifelse(toupper(tiges$action) == "ANNULATION", -1L, 1L) *
    as.integer(tiges$quantite)
  agg <- stats::aggregate(delta,
                          by = list(contexteId = tiges$contexteId,
                                    essence = tiges$essence,
                                    classe = tiges$classe),
                          FUN = sum)
  names(agg)[4] <- "tiges"
  vol <- .marculus_volumes_nets(tiges)
  agg <- merge(agg, vol, by = c("contexteId", "essence", "classe"),
               all.x = TRUE, sort = FALSE)
  agg <- agg[agg$tiges != 0L, , drop = FALSE]
  agg[order(agg$contexteId, agg$essence, agg$classe), , drop = FALSE]
}

#' Net volume of each case (context x species x class), Marculus' rule
#'
#' Same algorithm as `VolumesMartelage.totaux()` on the phone: stems are
#' stacked per case in time order (then uuid), and a cancellation removes the
#' LAST counted stems of its case, with their volume - not the volume of its own
#' entry, which may lack the height EMERGE needs.
#'
#' @param tiges A stems data.frame carrying unit volumes.
#' @return A data.frame `contexteId, essence, classe, volume_m3` (bois fort
#'   tige, net), `NA` when no stem of the case carries a volume.
#' @noRd
.marculus_volumes_nets <- function(tiges) {
  vide <- data.frame(contexteId = character(0), essence = character(0),
                     classe = integer(0), volume_m3 = numeric(0))
  if (nrow(tiges) == 0L) return(vide)
  tiges <- tiges[order(tiges$contexteId, tiges$essence, tiges$classe,
                       tiges$horodatage, tiges$uuid), , drop = FALSE]
  cle <- paste(tiges$contexteId, tiges$essence, tiges$classe, sep = "\r")
  res <- lapply(split(seq_len(nrow(tiges)), factor(cle, levels = unique(cle))),
                function(ix) {
    t <- tiges[ix, , drop = FALSE]
    pile_v <- numeric(0); pile_n <- integer(0)
    for (k in seq_len(nrow(t))) {
      n <- as.integer(t$quantite[k])
      if (toupper(t$action[k]) == "ANNULATION") {
        while (n > 0L && length(pile_n)) {
          m <- length(pile_n); r <- min(n, pile_n[m])
          pile_n[m] <- pile_n[m] - r; n <- n - r
          if (pile_n[m] == 0L) { pile_n <- pile_n[-m]; pile_v <- pile_v[-m] }
        }
      } else {
        pile_v <- c(pile_v, t$volumeTigeM3[k]); pile_n <- c(pile_n, n)
      }
    }
    v <- if (all(is.na(t$volumeTigeM3))) NA_real_ else sum(pile_v * pile_n, na.rm = TRUE)
    data.frame(contexteId = t$contexteId[1], essence = t$essence[1],
               classe = t$classe[1], volume_m3 = v)
  })
  do.call(rbind, res)
}

#' Net number of "Biodiversite" stems per context
#'
#' Stems whose tree quality is `Biodiversite` (`Referentiels.QUALITE_BIODIVERSITE`
#' in Marculus: habitat trees, cavities, standing dead wood), net of
#' cancellations carrying the same quality. Accents and case are ignored.
#'
#' @param tiges A stems data.frame.
#' @return A named integer vector (names: context ids), zero entries kept out.
#' @noRd
marculus_nb_biodiversite <- function(tiges) {
  tiges <- .marculus_tiges_normaliser(tiges)
  q <- tolower(iconv(tiges$qualiteArbre, to = "ASCII//TRANSLIT"))
  bio <- tiges[!is.na(q) & q == "biodiversite", , drop = FALSE]
  if (nrow(bio) == 0L) return(stats::setNames(integer(0), character(0)))
  delta <- ifelse(toupper(bio$action) == "ANNULATION", -1L, 1L) *
    as.integer(bio$quantite)
  n <- tapply(delta, bio$contexteId, sum)
  stats::setNames(as.integer(n), names(n))
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
  n_biodiv <- marculus_nb_biodiversite(tiges)

  maj <- character(); orphelins <- 0L
  for (i in seq_len(nrow(contextes))) {
    cid <- contextes$id[i]
    if (!cid %in% ids_plan) { orphelins <- orphelins + 1L; next }
    action <- plan$actions[[match(cid, ids_plan)]]
    upd <- list()

    st <- unname(MARCULUS_STATUTS_RETOUR[toupper(contextes$statut[i] %||% "")])
    if (length(st) == 1L && !is.na(st)) upd$statut <- st
    # Des tiges designees = le martelage a eu lieu : l'action passe a
    # « realisee » dans le Kanban, quel que soit le statut laisse sur le
    # telephone (l'operateur y change rarement la colonne du contexte).
    if (isTRUE(n_par_ctx[cid] > 0L)) upd$statut <- "realisee"

    dm <- contextes$dateMartelage[i]
    if (!is.na(dm)) {
      d <- as.Date(as.POSIXct(dm / 1000, origin = "1970-01-01", tz = "UTC"))
      upd$date_martelage <- format(d, "%Y-%m-%d")
      offset <- as.integer(format(d, "%Y")) - annee_base
      if (offset >= 1L && offset <= horizon) upd$annee_cible <- offset
    }

    q_vol <- NULL
    vt <- contextes$volumeTigeTotalM3[i] %||% NA
    if (length(vt) == 1L && !is.na(vt)) {
      # Totaux du telephone, qui font foi : bois fort tige dans `volume_m3`
      # (colonne Volume, bilan), et une copie `volume_martele_m3` pour la
      # fiche, qui ne doit jamais montrer une estimation IA comme un martelage.
      q_vol <- list(
        volume_m3 = round(vt, 4),
        volume_martele_m3 = round(vt, 4),
        volume_total_m3 = round(contextes$volumeTotalM3[i], 4),
        surface_terriere_m2 = round(contextes$surfaceTerriereTotaleM2[i], 4),
        nb_tiges_non_cubees = as.integer(contextes$nbTigesNonCubees[i]))
    }
    if (cid %in% names(n_par_ctx) || !is.null(q_vol)) {
      q <- action$quantite %||% list()
      for (k in names(q_vol)) q[[k]] <- q_vol[[k]]
    }
    if (cid %in% names(n_par_ctx)) {
      q$nb_tiges <- as.integer(n_par_ctx[[cid]])
      q$nb_tiges_biodiversite <- as.integer(n_biodiv[cid] %||% 0L)
      if (is.na(q$nb_tiges_biodiversite)) q$nb_tiges_biodiversite <- 0L
    }
    if (cid %in% names(n_par_ctx) || !is.null(q_vol)) upd$quantite <- q

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
               vides = lu$vides, ids = character())
  if (nrow(lu$contextes) == 0L) return(vide)

  avant <- marculus_charger_tiges(project_id)
  # Les tiges TOUT JUSTE lues passent devant : a `modifie` egal (meme tige
  # reexportee), c'est la version importee qui gagne. Dans l'autre ordre, un
  # CSV au format 3 reimporte apres un format 2 laissait les tiges... sans
  # leurs volumes (constate sur « Reconfort », 2026-09-25).
  tiges <- .marculus_tiges_union(rbind(lu$tiges, avant))
  n_nouvelles <- length(setdiff(tiges$uuid, avant$uuid))

  plan <- load_action_plan(project_id)
  if (is.null(plan)) return(vide)
  res <- marculus_appliquer_retour(plan, lu$contextes, tiges, user = user)

  .marculus_sauver_tiges(project_id, tiges)
  if (res$n_actions > 0L) save_action_plan(project_id, res$plan)

  list(plan = res$plan, n_actions = res$n_actions,
       n_orphelins = res$n_orphelins, n_tiges_nouvelles = n_nouvelles,
       n_tiges = nrow(tiges), illisibles = lu$illisibles,
       csv_anciens = lu$csv_anciens, vides = lu$vides, ids = res$ids)
}


# ---- Fiche d'une action : tiges designees et categories ------------------

#' Stems still designated after cancellations, for one context
#'
#' Marculus' rule (`VolumesMartelage.totaux()`): per case (species x class),
#' stems are stacked in time order and a cancellation removes the LAST ones.
#' What is left is the list the marker actually designated.
#'
#' @param tiges All stems of the project.
#' @param contexte_id Character. The context (= action) id.
#' @return The PLUS rows still standing, `quantite` reduced when a
#'   cancellation took part of an entry, in time order.
#' @noRd
marculus_tiges_designees <- function(tiges, contexte_id) {
  tiges <- .marculus_tiges_normaliser(tiges)
  t <- tiges[tiges$contexteId == contexte_id, , drop = FALSE]
  if (nrow(t) == 0L) return(t)
  t <- t[order(t$horodatage, t$uuid), , drop = FALSE]
  garde <- integer(0); reste <- integer(0)
  piles <- list()
  for (k in seq_len(nrow(t))) {
    cle <- paste(t$essence[k], t$classe[k], sep = "\r")
    n <- as.integer(t$quantite[k])
    if (toupper(t$action[k]) == "ANNULATION") {
      p <- piles[[cle]] %||% integer(0)
      while (n > 0L && length(p)) {
        i <- p[length(p)]; r <- min(n, reste[i])
        reste[i] <- reste[i] - r; n <- n - r
        if (reste[i] == 0L) p <- p[-length(p)]
      }
      piles[[cle]] <- p
    } else {
      garde <- c(garde, k); reste <- c(reste, n)
      piles[[cle]] <- c(piles[[cle]] %||% integer(0), length(garde))
    }
  }
  out <- t[garde, , drop = FALSE]
  out$quantite <- reste
  out[out$quantite > 0L, , drop = FALSE]
}

#' Wood size category of a class, as Marculus files it
#'
#' Default thresholds of `SeuilsCategories.DEFAUT` (Marculus), on the
#' DIAMETER: PB < 27.5 cm <= BM < 47.5 <= GB < 67.5 <= TGB. A circumference
#' class is brought back to a diameter first (classe / pi). The phone lets the
#' operator change these thresholds; they are not exported, so the defaults
#' apply here.
#'
#' @param classe Integer class.
#' @param mode `"DIAMETRE"` (default when unknown) or `"CIRCONFERENCE"`.
#' @return Character `PB`, `BM`, `GB` or `TGB`.
#' @noRd
marculus_categorie <- function(classe, mode = "DIAMETRE") {
  mode <- rep_len(as.character(mode), length(classe))
  mode <- ifelse(is.na(mode), "DIAMETRE", toupper(mode))
  d <- ifelse(mode == "CIRCONFERENCE", classe / pi, classe)
  as.character(cut(d, c(-Inf, 27.5, 47.5, 67.5, Inf),
                   labels = c("PB", "BM", "GB", "TGB"), right = FALSE))
}

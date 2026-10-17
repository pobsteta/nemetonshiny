# Tournee de travaux (Surculus / sommieR) : programme exporte, realise relu.
#
# Brief surculus du 2026-10-15. nemetonshiny fournit le PREVU (un GeoPackage
# du plan d'action, filtre) et recoit le REALISE (le CSV ecrit par
# `sommier_actions_realisees()`), jamais les saisies du telephone : seul
# sommieR a valide l'import, seul son fichier fait foi.

#' Action types sent to the works round
#'
#' Everything but felling and observation: Marculus covers the marked cuts,
#' Surculus the rest (planting, regeneration, protection...).
#' @noRd
TOURNEE_TRAVAUX_TYPES_EXCLUS <- c("coupe_rase", "eclaircie", "observation")

#' Statuses sent to the works round
#' @noRd
TOURNEE_TRAVAUX_STATUTS <- c("validee", "planifiee")

#' Columns of the realised-actions CSV written by sommieR
#' @noRd
TOURNEE_REALISE_COLONNES <- c("action_id", "travaux_id", "code_travaux",
                              "date_reception", "quantite", "unite")

#' Ids of the actions sent to the works round
#'
#' @param plan List. Loaded action plan.
#' @return Character vector of action ids (validated or planned, works types).
#' @noRd
tournee_travaux_action_ids <- function(plan) {
  df <- actions_to_dataframe(plan)
  if (!nrow(df)) return(character(0))
  ok <- df$statut %in% TOURNEE_TRAVAUX_STATUTS &
    !is.na(df$type) & !(df$type %in% TOURNEE_TRAVAUX_TYPES_EXCLUS)
  df$id[ok]
}

#' Export the works programme for sommieR
#'
#' The same GeoPackage as [export_action_plan_gpkg()], restricted to
#' [tournee_travaux_action_ids()]: sommieR reads it as is (brief `travaux-3`).
#'
#' @param plan List. Loaded action plan.
#' @param ug_sf sf. Project UGFs.
#' @param file_path Character. Destination `.gpkg`.
#' @return Logical. `FALSE` when no action qualifies or the write failed.
#' @noRd
export_programme_travaux_gpkg <- function(plan, ug_sf, file_path) {
  ids <- tournee_travaux_action_ids(plan)
  if (!length(ids)) return(FALSE)
  export_action_plan_gpkg(plan, ug_sf, file_path, filter_action_ids = ids)
}

#' Read the realised-actions CSV written by sommieR
#'
#' One row per action: an action carried out in several works (two planting
#' batches) is cited once, with the sum of its quantities. Quantities in
#' different units are not summed (`quantite = NA`, units listed).
#'
#' @param path Character. CSV written by `sommier_actions_realisees()`.
#' @return data.frame `action_id, n_travaux, quantite, unite, date_reception`.
#'   Errors when a column is missing.
#' @noRd
lire_actions_realisees <- function(path) {
  df <- utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE,
                        colClasses = "character", encoding = "UTF-8")
  manquantes <- setdiff(TOURNEE_REALISE_COLONNES, names(df))
  if (length(manquantes)) {
    cli::cli_abort("Missing column(s): {.val {manquantes}}.",
                   class = "tournee_csv_invalide")
  }
  df$action_id <- trimws(df$action_id)
  df <- df[!is.na(df$action_id) & nzchar(df$action_id), , drop = FALSE]
  vide <- data.frame(action_id = character(), n_travaux = integer(),
                     quantite = numeric(), unite = character(),
                     date_reception = character(), stringsAsFactors = FALSE)
  if (!nrow(df)) return(vide)
  q <- suppressWarnings(as.numeric(sub(",", ".", df$quantite, fixed = TRUE)))
  lignes <- lapply(split(seq_len(nrow(df)), factor(df$action_id, unique(df$action_id))),
                   function(i) {
    unites <- unique(df$unite[i][!is.na(df$unite[i]) & nzchar(df$unite[i])])
    dates <- df$date_reception[i][!is.na(df$date_reception[i]) & nzchar(df$date_reception[i])]
    data.frame(
      action_id = df$action_id[i[1]],
      n_travaux = length(unique(df$travaux_id[i])),
      quantite = if (length(unites) <= 1L && any(!is.na(q[i])))
        sum(q[i], na.rm = TRUE) else NA_real_,
      unite = paste(unites, collapse = ", "),
      date_reception = if (length(dates)) max(dates) else NA_character_,
      stringsAsFactors = FALSE)
  })
  do.call(rbind, c(unname(lignes), list(make.row.names = FALSE)))
}

#' Status path from an action's status to `realisee`
#'
#' Only `planifiee -> realisee` is a Kanban transition; a validated action
#' sent to the round goes through `planifiee` first, each step allowed by
#' [is_valid_status_transition()]. Any other status is refused: an abandoned
#' or merely proposed action was never sent to the field.
#'
#' @param statut Character. Current status.
#' @return Character vector of the statuses to apply in order, or `NULL`.
#' @noRd
.tournee_chemin_realisee <- function(statut) {
  if (isTRUE(is_valid_status_transition(statut, "realisee")) &&
      !identical(statut, "realisee")) return("realisee")
  if (identical(statut, "validee") &&
      is_valid_status_transition("validee", "planifiee") &&
      is_valid_status_transition("planifiee", "realisee")) {
    return(c("planifiee", "realisee"))
  }
  NULL
}

#' Proposals of the realised-actions file against the plan
#'
#' @param plan List. Loaded action plan.
#' @param realise data.frame from [lire_actions_realisees()].
#' @return `realise` with `etat` (`a_passer`, `deja_realisee`, `inconnue`,
#'   `transition_invalide`), `statut`, `type` and `ug_id`.
#' @noRd
proposer_actions_realisees <- function(plan, realise) {
  df <- actions_to_dataframe(plan)
  idx <- match(realise$action_id, df$id)
  realise$statut <- df$statut[idx]
  realise$type <- df$type[idx]
  realise$ug_id <- df$ug_id[idx]
  realise$etat <- vapply(seq_len(nrow(realise)), function(i) {
    if (is.na(idx[i])) return("inconnue")
    if (identical(realise$statut[i], "realisee")) return("deja_realisee")
    if (is.null(.tournee_chemin_realisee(realise$statut[i]))) "transition_invalide"
    else "a_passer"
  }, character(1))
  realise
}

#' Flip the confirmed actions to `realisee`
#'
#' Goes through [update_action_in_plan()], which writes the audit entry
#' (`op = update`, user, date) of every status step. Ids whose transition is
#' no longer allowed are skipped.
#'
#' @param plan List. Loaded action plan.
#' @param action_ids Character. Confirmed ids.
#' @param user Character. Acting user.
#' @return `list(plan, n)`.
#' @noRd
appliquer_actions_realisees <- function(plan, action_ids, user = NULL) {
  n <- 0L
  for (id in unique(action_ids)) {
    a <- get_action_by_id(plan, id)
    chemin <- if (!is.null(a)) .tournee_chemin_realisee(a$statut %||% "")
    if (is.null(chemin)) next
    for (st in chemin) {
      plan <- update_action_in_plan(plan, id, list(statut = st), user = user)
    }
    n <- n + 1L
  }
  list(plan = plan, n = n)
}

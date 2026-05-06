#' Action Plan Service for nemetonApp
#'
#' @description
#' Service for managing the silvicultural action plan attached to a project.
#' Each project may carry a `data/action_plan.json` file storing the list of
#' planned actions per UGF, plus an audit trail of every change.
#'
#' Schema (JSON):
#' \preformatted{
#' {
#'   version: 1,
#'   project_id: <chr>,
#'   horizon_annees: 20,
#'   actions: [ <action> ... ],
#'   audit:   [ <audit_entry> ... ]
#' }
#'
#' action: {
#'   id, ug_id,
#'   type, type_libre,
#'   intensite, annee_cible, duree,
#'   priorite, objectifs_lies (vector of family codes),
#'   quantite: { volume_m3, surface_ha, nb_tiges, rdi, cout_eur, revenu_eur },
#'   statut, source: { origine, extrait_texte },
#'   commentaire,
#'   cree_par, cree_le, modifie_par, modifie_le
#' }
#'
#' audit_entry: { ts, user, action_id, op (create|update|delete),
#'                champ, ancien, nouveau }
#' }
#'
#' @name service_action_plan
#' @keywords internal
NULL


# ---- Constants --------------------------------------------------------

#' Predefined action types
#'
#' @description
#' Hybrid typology: predefined keys plus the catch-all `autre` (free text in
#' `type_libre`).
#'
#' @noRd
ACTION_PLAN_TYPES <- c(
  "coupe_rase", "eclaircie", "depressage", "plantation", "regeneration",
  "cloisonnement", "desserte", "observation", "protection", "entretien",
  "autre"
)

#' Allowed action statuses (Kanban workflow)
#' @noRd
ACTION_PLAN_STATUTS <- c(
  "proposee", "validee", "planifiee", "realisee", "abandonnee"
)

#' Allowed action priorities
#' @noRd
ACTION_PLAN_PRIORITES <- c("haute", "moyenne", "basse")

#' Family codes valid in `objectifs_lies`
#' @noRd
ACTION_PLAN_FAMILY_CODES <- c(
  "C", "B", "W", "A", "F", "L", "T", "R", "S", "P", "E", "N"
)

#' On-disk schema version
#' @noRd
ACTION_PLAN_SCHEMA_VERSION <- 1L


# ---- File paths -------------------------------------------------------

#' Path to a project's action plan file
#'
#' @param project_id Character. Project ID.
#' @return Character path or NULL if the project does not exist.
#' @noRd
get_action_plan_path <- function(project_id) {
  project_path <- get_project_path(project_id)
  if (is.null(project_path)) {
    return(NULL)
  }
  data_dir <- file.path(project_path, "data")
  if (!dir.exists(data_dir)) {
    dir.create(data_dir, recursive = TRUE, showWarnings = FALSE)
  }
  file.path(data_dir, "action_plan.json")
}


# ---- Empty / load / save ----------------------------------------------

#' Build an empty action plan
#'
#' @param project_id Character. Project ID.
#' @param horizon_annees Integer. Planning horizon in years (default 20).
#' @return List representing an empty plan.
#' @noRd
init_empty_action_plan <- function(project_id, horizon_annees = 20L) {
  list(
    version = ACTION_PLAN_SCHEMA_VERSION,
    project_id = project_id,
    horizon_annees = as.integer(horizon_annees),
    actions = list(),
    audit = list()
  )
}


#' Load a project's action plan
#'
#' @param project_id Character. Project ID.
#' @return List with the plan, or NULL if the project is unknown. An empty
#'   plan is returned when the file does not exist yet.
#' @noRd
load_action_plan <- function(project_id) {
  path <- get_action_plan_path(project_id)
  if (is.null(path)) {
    return(NULL)
  }
  if (!file.exists(path)) {
    return(init_empty_action_plan(project_id))
  }
  plan <- tryCatch(
    jsonlite::read_json(path, simplifyVector = FALSE),
    error = function(e) {
      cli::cli_warn("Failed to read action plan: {e$message}")
      NULL
    }
  )
  if (is.null(plan)) {
    return(init_empty_action_plan(project_id))
  }
  plan$version        <- plan$version        %||% ACTION_PLAN_SCHEMA_VERSION
  plan$project_id     <- plan$project_id     %||% project_id
  plan$horizon_annees <- plan$horizon_annees %||% 20L
  plan$actions        <- plan$actions        %||% list()
  plan$audit          <- plan$audit          %||% list()
  plan
}


#' Save a project's action plan atomically
#'
#' @param project_id Character. Project ID.
#' @param plan List. Plan structure to persist.
#' @return Logical. TRUE on success.
#' @noRd
save_action_plan <- function(project_id, plan) {
  path <- get_action_plan_path(project_id)
  if (is.null(path)) {
    cli::cli_warn("save_action_plan: unknown project {project_id}")
    return(FALSE)
  }
  tmp <- paste0(path, ".tmp")
  ok <- tryCatch({
    jsonlite::write_json(plan, tmp, auto_unbox = TRUE, pretty = TRUE,
                         null = "null", na = "null")
    file.rename(tmp, path)
  }, error = function(e) {
    cli::cli_warn("Failed to save action plan: {e$message}")
    if (file.exists(tmp)) file.remove(tmp)
    FALSE
  })
  isTRUE(ok)
}


# ---- Validation -------------------------------------------------------

#' Validate a single action against the schema
#'
#' @param action List. Action to validate.
#' @param ug_ids Character. Allowed UGF identifiers (typically from the
#'   project's `ugs`). Pass NULL to skip the membership check.
#' @param horizon_annees Integer. Planning horizon. Used to bound `annee_cible`.
#' @return List with `valid` (logical) and `errors` (character).
#' @noRd
validate_action <- function(action, ug_ids = NULL, horizon_annees = 20L) {
  errors <- character()

  if (!is.list(action)) {
    return(list(valid = FALSE, errors = "action is not a list"))
  }

  required <- c("ug_id", "type", "annee_cible", "priorite", "statut")
  missing <- setdiff(required, names(action))
  if (length(missing) > 0) {
    errors <- c(errors, sprintf("missing field(s): %s",
                                paste(missing, collapse = ", ")))
  }

  if (!is.null(action$ug_id) && !is.null(ug_ids) &&
      !(action$ug_id %in% ug_ids)) {
    errors <- c(errors, sprintf("unknown ug_id '%s'", action$ug_id))
  }

  if (!is.null(action$type) && !(action$type %in% ACTION_PLAN_TYPES)) {
    errors <- c(errors, sprintf("invalid type '%s'", action$type))
  }
  if (identical(action$type, "autre") &&
      (is.null(action$type_libre) || !nzchar(action$type_libre))) {
    errors <- c(errors, "type='autre' requires non-empty type_libre")
  }

  if (!is.null(action$annee_cible)) {
    annee <- suppressWarnings(as.integer(action$annee_cible))
    if (is.na(annee) || annee < 1L || annee > as.integer(horizon_annees)) {
      errors <- c(errors,
                  sprintf("annee_cible must be in 1..%d", as.integer(horizon_annees)))
    }
  }

  if (!is.null(action$duree)) {
    duree <- suppressWarnings(as.integer(action$duree))
    if (is.na(duree) || duree < 1L) {
      errors <- c(errors, "duree must be a positive integer")
    }
  }

  if (!is.null(action$priorite) &&
      !(action$priorite %in% ACTION_PLAN_PRIORITES)) {
    errors <- c(errors, sprintf("invalid priorite '%s'", action$priorite))
  }

  if (!is.null(action$statut) &&
      !(action$statut %in% ACTION_PLAN_STATUTS)) {
    errors <- c(errors, sprintf("invalid statut '%s'", action$statut))
  }

  if (!is.null(action$objectifs_lies)) {
    bad <- setdiff(unlist(action$objectifs_lies), ACTION_PLAN_FAMILY_CODES)
    if (length(bad) > 0) {
      errors <- c(errors, sprintf("invalid family code(s): %s",
                                  paste(bad, collapse = ", ")))
    }
  }

  list(valid = length(errors) == 0L, errors = errors)
}


#' Allowed Kanban transitions
#'
#' @noRd
ACTION_PLAN_TRANSITIONS <- list(
  proposee   = c("validee", "abandonnee"),
  validee    = c("planifiee", "abandonnee"),
  planifiee  = c("validee", "realisee", "abandonnee"),
  realisee   = character(),
  abandonnee = c("proposee")
)


#' Check whether a status transition is allowed
#'
#' @param from Character. Current status.
#' @param to Character. Target status.
#' @return Logical.
#' @noRd
is_valid_status_transition <- function(from, to) {
  if (identical(from, to)) return(TRUE)
  allowed <- ACTION_PLAN_TRANSITIONS[[from]]
  if (is.null(allowed)) return(FALSE)
  to %in% allowed
}


# ---- CRUD -------------------------------------------------------------

#' Generate a new action ID
#'
#' @noRd
new_action_id <- function() {
  paste0("act_",
         format(Sys.time(), "%Y%m%d%H%M%S"),
         "_",
         paste(sample(c(0:9, letters), 6, replace = TRUE), collapse = ""))
}


#' Build an audit entry
#'
#' @noRd
make_audit_entry <- function(op, action_id, user = NULL,
                             champ = NA_character_,
                             ancien = NULL, nouveau = NULL) {
  list(
    ts = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
    user = user %||% Sys.info()[["user"]] %||% "unknown",
    action_id = action_id,
    op = op,
    champ = champ,
    ancien = ancien,
    nouveau = nouveau
  )
}


#' Add a new action to a plan
#'
#' Auto-fills `id`, `cree_par`, `cree_le`. Validates the action against the
#' supplied UGF list and horizon.
#'
#' @param plan List. Existing plan (or NULL to start from scratch).
#' @param action List. Action to add (must contain at least the required
#'   fields - see `validate_action`).
#' @param ug_ids Character. Allowed UGF IDs. Pass NULL to skip.
#' @param user Character. Acting user (audit).
#' @return Updated plan (with action and audit entry appended).
#' @noRd
add_action_to_plan <- function(plan, action, ug_ids = NULL, user = NULL) {
  if (is.null(plan)) {
    cli::cli_abort("add_action_to_plan: plan is NULL")
  }
  horizon <- plan$horizon_annees %||% 20L

  if (is.null(action$id) || !nzchar(as.character(action$id))) {
    action$id <- new_action_id()
  }
  if (is.null(action$statut)) action$statut <- "proposee"
  if (is.null(action$priorite)) action$priorite <- "moyenne"
  action$cree_par   <- action$cree_par   %||% (user %||% Sys.info()[["user"]] %||% "unknown")
  action$cree_le    <- action$cree_le    %||% format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")
  action$modifie_par <- NULL
  action$modifie_le  <- NULL

  v <- validate_action(action, ug_ids = ug_ids, horizon_annees = horizon)
  if (!v$valid) {
    cli::cli_abort(c("invalid action",
                     "x" = paste(v$errors, collapse = "; ")))
  }

  plan$actions <- c(plan$actions, list(action))
  plan$audit   <- c(plan$audit,
                    list(make_audit_entry("create", action$id, user = user,
                                          nouveau = action)))
  plan
}


#' Find action index by id
#' @noRd
find_action_index <- function(plan, action_id) {
  if (length(plan$actions) == 0L) return(NA_integer_)
  ids <- vapply(plan$actions,
                function(a) as.character(a$id %||% ""), character(1))
  idx <- match(action_id, ids)
  if (is.na(idx)) return(NA_integer_) else as.integer(idx)
}


#' Get an action by ID
#' @noRd
get_action_by_id <- function(plan, action_id) {
  idx <- find_action_index(plan, action_id)
  if (is.na(idx)) return(NULL)
  plan$actions[[idx]]
}


#' Update fields of an existing action
#'
#' @param plan List. Plan.
#' @param action_id Character. ID of the action to update.
#' @param updates Named list. Fields to overwrite.
#' @param ug_ids Character. Allowed UGF IDs.
#' @param user Character. Acting user.
#' @return Updated plan.
#' @noRd
update_action_in_plan <- function(plan, action_id, updates,
                                  ug_ids = NULL, user = NULL) {
  idx <- find_action_index(plan, action_id)
  if (is.na(idx)) {
    cli::cli_abort("update_action_in_plan: unknown action_id '{action_id}'")
  }
  current <- plan$actions[[idx]]
  horizon <- plan$horizon_annees %||% 20L

  # Status transitions are constrained
  if ("statut" %in% names(updates) &&
      !is_valid_status_transition(current$statut %||% "proposee",
                                  updates$statut)) {
    cli::cli_abort("invalid status transition: {current$statut} -> {updates$statut}")
  }

  audit_entries <- list()
  ts <- format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")
  for (champ in names(updates)) {
    ancien <- current[[champ]]
    nouveau <- updates[[champ]]
    if (!identical(ancien, nouveau)) {
      audit_entries <- c(audit_entries,
                         list(make_audit_entry("update", action_id, user = user,
                                               champ = champ,
                                               ancien = ancien,
                                               nouveau = nouveau)))
      current[[champ]] <- nouveau
    }
  }
  current$modifie_par <- user %||% Sys.info()[["user"]] %||% "unknown"
  current$modifie_le  <- ts

  v <- validate_action(current, ug_ids = ug_ids, horizon_annees = horizon)
  if (!v$valid) {
    cli::cli_abort(c("invalid action after update",
                     "x" = paste(v$errors, collapse = "; ")))
  }

  plan$actions[[idx]] <- current
  plan$audit <- c(plan$audit, audit_entries)
  plan
}


#' Delete an action from a plan
#'
#' @param plan List. Plan.
#' @param action_id Character. ID to remove.
#' @param user Character. Acting user.
#' @return Updated plan.
#' @noRd
delete_action_from_plan <- function(plan, action_id, user = NULL) {
  idx <- find_action_index(plan, action_id)
  if (is.na(idx)) {
    cli::cli_abort("delete_action_from_plan: unknown action_id '{action_id}'")
  }
  removed <- plan$actions[[idx]]
  plan$actions[[idx]] <- NULL
  plan$audit <- c(plan$audit,
                  list(make_audit_entry("delete", action_id, user = user,
                                        ancien = removed)))
  plan
}


#' Bulk upsert: insert new actions and update existing ones (matched by id)
#'
#' Useful for LLM-generated batches where some actions may already exist.
#'
#' @param plan List. Plan.
#' @param actions List of action lists.
#' @param ug_ids Character. Allowed UGF IDs.
#' @param user Character. Acting user.
#' @return Updated plan.
#' @noRd
bulk_upsert_actions <- function(plan, actions, ug_ids = NULL, user = NULL) {
  for (a in actions) {
    if (!is.null(a$id) && !is.na(find_action_index(plan, a$id))) {
      plan <- update_action_in_plan(plan, a$id, a,
                                    ug_ids = ug_ids, user = user)
    } else {
      plan <- add_action_to_plan(plan, a,
                                 ug_ids = ug_ids, user = user)
    }
  }
  plan
}


# ---- Filtering / accessors --------------------------------------------

#' Filter actions
#'
#' @param plan List. Plan.
#' @param ug_id Character. UGF filter (NULL = no filter).
#' @param annee Integer. Year filter.
#' @param annee_min,annee_max Integer. Year range filter.
#' @param type Character. Action type filter.
#' @param statut Character. Status filter.
#' @param famille Character. Family code filter (action keeps the family in
#'   `objectifs_lies`).
#' @return List of matching actions.
#' @noRd
filter_actions <- function(plan, ug_id = NULL, annee = NULL,
                           annee_min = NULL, annee_max = NULL,
                           type = NULL, statut = NULL,
                           famille = NULL) {
  if (is.null(plan) || length(plan$actions) == 0L) return(list())
  Filter(function(a) {
    if (!is.null(ug_id)   && !(a$ug_id   %in% ug_id))   return(FALSE)
    if (!is.null(type)    && !(a$type    %in% type))    return(FALSE)
    if (!is.null(statut)  && !(a$statut  %in% statut))  return(FALSE)
    if (!is.null(annee)   && !identical(as.integer(a$annee_cible),
                                        as.integer(annee))) return(FALSE)
    if (!is.null(annee_min) &&
        as.integer(a$annee_cible %||% NA_integer_) < as.integer(annee_min)) {
      return(FALSE)
    }
    if (!is.null(annee_max) &&
        as.integer(a$annee_cible %||% NA_integer_) > as.integer(annee_max)) {
      return(FALSE)
    }
    if (!is.null(famille)) {
      fam <- unlist(a$objectifs_lies)
      if (length(intersect(fam, famille)) == 0L) return(FALSE)
    }
    TRUE
  }, plan$actions)
}


#' Get audit entries for a single action
#' @noRd
get_action_audit <- function(plan, action_id) {
  if (length(plan$audit) == 0L) return(list())
  Filter(function(e) identical(e$action_id, action_id), plan$audit)
}


#' Convert a list of audit entries into a tidy data.frame
#'
#' Used by the history modal in `mod_action_plan`.
#'
#' @param audit List of audit entries.
#' @return data.frame.
#' @noRd
audit_to_dataframe <- function(audit) {
  empty <- data.frame(ts = character(), user = character(),
                      op = character(), champ = character(),
                      ancien = character(), nouveau = character(),
                      stringsAsFactors = FALSE)
  if (is.null(audit) || length(audit) == 0L) return(empty)
  shorten <- function(x) {
    if (is.null(x)) return(NA_character_)
    if (is.list(x)) return(jsonlite::toJSON(x, auto_unbox = TRUE,
                                            null = "null"))
    paste(as.character(x), collapse = ", ")
  }
  rows <- lapply(audit, function(e) {
    data.frame(
      ts      = as.character(e$ts %||% NA_character_),
      user    = as.character(e$user %||% NA_character_),
      op      = as.character(e$op %||% NA_character_),
      champ   = as.character(e$champ %||% NA_character_),
      ancien  = shorten(e$ancien),
      nouveau = shorten(e$nouveau),
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, c(rows, list(make.row.names = FALSE)))
}


#' Convert a plan's actions into a tidy data.frame
#'
#' One row per action, scalar columns for tabular display.
#'
#' @param plan List. Plan.
#' @return data.frame (possibly empty).
#' @noRd
actions_to_dataframe <- function(plan) {
  empty <- data.frame(
    id = character(),
    ug_id = character(),
    type = character(),
    type_libre = character(),
    intensite = character(),
    annee_cible = integer(),
    duree = integer(),
    priorite = character(),
    objectifs_lies = character(),
    volume_m3 = numeric(),
    surface_ha = numeric(),
    nb_tiges = integer(),
    rdi = numeric(),
    cout_eur = numeric(),
    revenu_eur = numeric(),
    bilan_eur = numeric(),
    statut = character(),
    source_origine = character(),
    source_extrait = character(),
    commentaire = character(),
    cree_par = character(),
    cree_le = character(),
    modifie_par = character(),
    modifie_le = character(),
    stringsAsFactors = FALSE
  )
  if (is.null(plan) || length(plan$actions) == 0L) return(empty)

  rows <- lapply(plan$actions, function(a) {
    cout   <- suppressWarnings(as.numeric(a$quantite$cout_eur   %||% NA_real_))
    revenu <- suppressWarnings(as.numeric(a$quantite$revenu_eur %||% NA_real_))
    # Bilan = revenu - cout. Treat NA as 0 only when at least one of the
    # two has a value, so a totally unknown action stays NA.
    bilan <- if (is.na(cout) && is.na(revenu)) {
      NA_real_
    } else {
      (if (is.na(revenu)) 0 else revenu) - (if (is.na(cout)) 0 else cout)
    }
    data.frame(
      id           = as.character(a$id           %||% NA_character_),
      ug_id        = as.character(a$ug_id        %||% NA_character_),
      type         = as.character(a$type         %||% NA_character_),
      type_libre   = as.character(a$type_libre   %||% NA_character_),
      intensite    = as.character(a$intensite    %||% NA_character_),
      annee_cible  = suppressWarnings(as.integer(a$annee_cible %||% NA_integer_)),
      duree        = suppressWarnings(as.integer(a$duree       %||% NA_integer_)),
      priorite     = as.character(a$priorite     %||% NA_character_),
      objectifs_lies = paste(unlist(a$objectifs_lies %||% character()), collapse = ","),
      volume_m3    = suppressWarnings(as.numeric(a$quantite$volume_m3  %||% NA_real_)),
      surface_ha   = suppressWarnings(as.numeric(a$quantite$surface_ha %||% NA_real_)),
      nb_tiges     = suppressWarnings(as.integer(a$quantite$nb_tiges   %||% NA_integer_)),
      rdi          = suppressWarnings(as.numeric(a$quantite$rdi        %||% NA_real_)),
      cout_eur     = cout,
      revenu_eur   = revenu,
      bilan_eur    = bilan,
      statut       = as.character(a$statut       %||% NA_character_),
      source_origine = as.character(a$source$origine        %||% NA_character_),
      source_extrait = as.character(a$source$extrait_texte  %||% NA_character_),
      commentaire  = as.character(a$commentaire  %||% NA_character_),
      cree_par     = as.character(a$cree_par     %||% NA_character_),
      cree_le      = as.character(a$cree_le      %||% NA_character_),
      modifie_par  = as.character(a$modifie_par  %||% NA_character_),
      modifie_le   = as.character(a$modifie_le   %||% NA_character_),
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, c(rows, list(make.row.names = FALSE)))
}

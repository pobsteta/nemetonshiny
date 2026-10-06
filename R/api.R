# API hors interface : piloter un diagnostic sans l'application Shiny.
#
# Brief ~/dev/briefs/vers-nemetonshiny/2026-10-04-api-hors-interface-sans-effet-de-bord.md
# (aigora-nemeton). Fines enveloppes des services existants : l'objectif est un
# CONTRAT stable (CONTRAT.md, section 1 bis), pas une reecriture. Regle cle :
# une fonction de LECTURE n'ecrit jamais rien dans le projet. Depuis la 1.0.0,
# aucune migration : un projet anterieur est refuse (`nemetonshiny_projet_ancien`).

#' Headless API
#'
#' @description
#' Functions to run a Nemeton diagnostic without the Shiny interface: list and
#' inspect projects, read their results, create and compute a project, export
#' a report or a GeoPackage.
#'
#' Read functions ([projets_lister()], [projet_etat()], [projet_lire()],
#' [parcelles_commune()]) **never write** into a project. Version 1.0.0 starts
#' from scratch: a project created before it is not taken over, and the
#' functions that read or compute it fail with an error of class
#' `nemetonshiny_projet_ancien`.
#'
#' Projects live in the projects directory: `run_app(project_dir = )` in the
#' application; outside it, the `NEMETON_PROJECT_DIR` environment variable, or
#' the default directory.
#'
#' @section Errors:
#' Errors are classed, so callers can react without parsing messages:
#' * `nemetonshiny_projet_introuvable`: unknown or invalid project id;
#' * `nemetonshiny_projet_ancien`: project created before version 1.0.0, to be
#'   recreated (the condition carries the [projet_etat()] result in `$etat`);
#' * `nemetonshiny_sans_indicateurs`: the operation needs computed indicators;
#' * `nemetonshiny_calcul_echec`: the computation failed.
#' All of them also inherit `nemetonshiny_erreur`.
#'
#' @name api_hors_interface
#' @md
NULL

.api_abort <- function(message, class, ..., call = rlang::caller_env()) {
  cli::cli_abort(message, class = c(class, "nemetonshiny_erreur"), ...,
                 call = call, .envir = parent.frame())
}

.api_project_path <- function(id, call = rlang::caller_env()) {
  path <- if (is.character(id) && length(id) == 1L && !is.na(id)) {
    get_project_path(id)
  }
  if (is.null(path)) {
    .api_abort("Projet introuvable : {.val {id}}.",
               "nemetonshiny_projet_introuvable", projet = id, call = call)
  }
  path
}

#' List the projects
#'
#' @return A data.frame, one row per project, most recently updated first:
#'   `id`, `nom`, `statut`, `maj`, `ndp`, `ugf`, `indicateurs` (computed
#'   indicators present on disk), `format_ok` (`FALSE` for a project created
#'   before version 1.0.0, which is not taken over).
#'   Read-only.
#' @family api_hors_interface
#' @md
#' @export
projets_lister <- function() {
  root <- get_projects_root()
  dirs <- list.dirs(root, recursive = FALSE)
  rows <- lapply(dirs, function(d) {
    f <- file.path(d, "metadata.json")
    if (!file.exists(f)) return(NULL)
    m <- tryCatch(jsonlite::read_json(f), error = function(e) NULL)
    if (is.null(m)) return(NULL)
    has_ind <- file.exists(file.path(d, "data", "indicators.parquet"))
    data.frame(
      id = basename(d),
      nom = .chr1(m$name),
      statut = .chr1(m$status),
      maj = .chr1(m$updated_at),
      ndp = suppressWarnings(as.integer(.chr1(m$ndp_level))),
      ugf = suppressWarnings(as.integer(.chr1(m$ug_count))),
      indicateurs = has_ind,
      format_ok = .projet_format_ok(m),
      stringsAsFactors = FALSE
    )
  })
  rows <- Filter(Negate(is.null), rows)
  if (!length(rows)) {
    return(data.frame(id = character(), nom = character(), statut = character(),
                      maj = character(), ndp = integer(), ugf = integer(),
                      indicateurs = logical(), format_ok = logical(),
                      stringsAsFactors = FALSE))
  }
  out <- do.call(rbind, rows)
  out <- out[order(out$maj, decreasing = TRUE, na.last = TRUE), , drop = FALSE]
  rownames(out) <- NULL
  out
}

.chr1 <- function(x) {
  if (is.null(x) || !length(x) || is.list(x)) NA_character_ else as.character(x[[1]])
}

#' State of a project
#'
#' Everything needed to decide what to do with a project, without loading it.
#'
#' @param id Project id (the name of its directory).
#' @return A list:
#'   * `id`, `nom`, `statut`, `chemin`, `ndp`;
#'   * `format_projet`, `format_ok`: project format, and whether this version
#'     of the application reads it (`FALSE` for a project created before
#'     1.0.0: it is not taken over and must be recreated);
#'   * `indicateurs`: computed indicators present on disk;
#'   * `ugf`: readable management units (UGF) present (otherwise the default
#'     layout, one unit per parcel, is created at the first opening);
#'   * `archives`: set-aside indicator files (`metadata$indicateurs_perimes`).
#'
#'   Read-only.
#' @family api_hors_interface
#' @md
#' @export
projet_etat <- function(id) {
  path <- .api_project_path(id)
  meta <- load_project_metadata(id)
  if (is.null(meta)) {
    .api_abort("Projet {.val {id}} : {.file metadata.json} absent ou illisible.",
               "nemetonshiny_projet_introuvable", projet = id)
  }
  list(
    id = id,
    nom = .chr1(meta$name),
    statut = .chr1(meta$status),
    chemin = path,
    ndp = suppressWarnings(as.integer(.chr1(meta$ndp_level))),
    format_projet = suppressWarnings(as.integer(.chr1(meta$format_projet))),
    format_ok = .projet_format_ok(meta),
    indicateurs = file.exists(file.path(path, "data", "indicators.parquet")),
    ugf = !is.null(suppressWarnings(load_ug_data(id))),
    archives = meta$indicateurs_perimes %||% list()
  )
}

#' Refuse a project created before version 1.0.0
#' @noRd
.api_refuser_ancien <- function(etat, call = rlang::caller_env()) {
  if (isTRUE(etat$format_ok)) return(invisible(TRUE))
  .api_abort(c(
    "Projet {.val {etat$id}} : cr\u00e9\u00e9 avant la version 1.0.0, il n'est pas repris.",
    i = "Le recr\u00e9er avec {.fn projet_creer} (m\u00eames parcelles) ; rien n'a \u00e9t\u00e9 modifi\u00e9."
  ), "nemetonshiny_projet_ancien", etat = etat, call = call)
}

#' Read a project's results, without side effect
#'
#' Builds the project exactly as the Synthesis tab sees it (indicators per
#' management unit, R5 from the linked monitoring zone, R6/R7 from
#' reGeneration, family scores through the core). Nothing is written (a
#' project without management units is read with none).
#'
#' @param id Project id.
#' @param langue `"fr"` or `"en"`, for the family labels of `synthese`.
#' @return A list:
#'   * `projet`: the project object (as consumed by the export functions);
#'   * `indicateurs`: sf, one row per management unit (or `NULL` when not
#'     computed);
#'   * `familles`: sf with the 12 `famille_*` columns (or `NULL`);
#'   * `synthese`: global score, NDP and the 12 family scores, as plain values
#'     (same figures as the Synthesis tab).
#' @section Errors:
#' Class `nemetonshiny_projet_ancien` for a project created before version
#' 1.0.0 (not taken over); class `nemetonshiny_projet_introuvable` for an
#' unknown id.
#' @family api_hors_interface
#' @md
#' @export
projet_lire <- function(id, langue = "fr") {
  etat <- projet_etat(id)
  .api_refuser_ancien(etat)

  meta <- load_project_metadata(id)
  project <- c(list(id = id, path = etat$chemin, metadata = meta),
               .read_project_files(id))
  ug <- load_ug_data(id)
  if (!is.null(ug)) {
    project$tenements <- ug$tenements
    project$ugs <- ug$ugs
  }
  if (!is.null(project$indicators)) {
    project <- attach_indicators_sf(project)
  }
  familles <- project_family_scores(project)
  list(
    projet = project,
    indicateurs = project$indicators_sf,
    familles = familles,
    synthese = project_synthesis_summary(project, langue, family_sf = familles)
  )
}

#' Cadastral parcels of a commune
#'
#' @param insee INSEE code of the commune (5 characters).
#' @param ids Optional character vector of parcel ids to keep.
#' @return An sf object with `id`, `section`, `numero`, `contenance` (m2),
#'   `commune`, `code_insee`. When `ids` is given, an error lists the ids not
#'   found. Read-only (network access to the cadastre services).
#' @family api_hors_interface
#' @md
#' @export
parcelles_commune <- function(insee, ids = NULL) {
  parcelles <- get_cadastral_parcels(insee)
  if (is.null(ids)) return(parcelles)
  ids <- as.character(ids)
  garde <- filter_selected_parcels(parcelles, ids)
  manquants <- setdiff(ids, garde$id)
  if (length(manquants)) {
    cli::cli_abort(c("Parcelles introuvables dans la commune {.val {insee}} :",
                     x = "{.val {manquants}}"),
                   class = c("nemetonshiny_parcelles_introuvables", "nemetonshiny_erreur"),
                   manquants = manquants)
  }
  garde
}

#' Create a project
#'
#' Creates the project and initialises it as the application does when it
#' opens a new project (management units: one per parcel), so it can be
#' computed right away with [projet_calculer()].
#'
#' @param nom Project name (100 characters max).
#' @param parcelles sf object of cadastral parcels, as returned by
#'   [parcelles_commune()].
#' @param description,proprietaire Optional text.
#' @param profil_groupes Optional management-unit group profile (`"onf"`,
#'   `"crpf"`, ...); default from the configuration.
#' @return The project id.
#' @family api_hors_interface
#' @md
#' @export
projet_creer <- function(nom, parcelles, description = "", proprietaire = "",
                         profil_groupes = NULL) {
  if (!inherits(parcelles, "sf") || nrow(parcelles) == 0) {
    cli::cli_abort("{.arg parcelles} doit \u00eatre un objet sf non vide.")
  }
  pr <- create_project(nom, description = description, owner = proprietaire,
                       parcels = parcelles, groupes_profile = profil_groupes)
  ensure_project_ug(pr$id)
  pr$id
}

#' Compute a project's indicators
#'
#' Runs the full computation synchronously (it can take from minutes to more
#' than an hour). Progress is also written to `data/compute_progress.json`.
#'
#' @param id Project id.
#' @param indicateurs `"all"` or a character vector of indicator codes.
#' @param progression Optional function called with the progress state (a
#'   list with `status`, `progress`, `progress_max`, `current_task`, ...).
#' @return The computation result (list with `success`), invisibly.
#' @section Errors:
#' Class `nemetonshiny_projet_ancien` for a project created before version
#' 1.0.0, `nemetonshiny_calcul_echec` when the computation fails.
#' @family api_hors_interface
#' @md
#' @export
projet_calculer <- function(id, indicateurs = "all", progression = NULL) {
  etat <- projet_etat(id)
  .api_refuser_ancien(etat)
  if (!is.null(progression) && !is.function(progression)) {
    cli::cli_abort("{.arg progression} doit \u00eatre une fonction ou NULL.")
  }
  res <- start_computation(id, indicators = indicateurs,
                           progress_callback = progression,
                           use_file_progress = TRUE)
  if (!isTRUE(res$success)) {
    err <- res$error %||% res$state$errors %||% "erreur inconnue"
    if (is.list(err)) err <- vapply(err, function(e) paste(unlist(e), collapse = " "), "")
    .api_abort(c("Calcul du projet {.val {id}} en \u00e9chec.", x = "{err}"),
               "nemetonshiny_calcul_echec", resultat = res)
  }
  invisible(res)
}

.api_results <- function(id, call = rlang::caller_env()) {
  lu <- projet_lire(id)
  if (is.null(lu$familles)) {
    .api_abort(c("Projet {.val {id}} : aucun indicateur calcul\u00e9.",
                 i = "Lancer {.fn projet_calculer} d'abord."),
               "nemetonshiny_sans_indicateurs", call = call)
  }
  lu
}

#' PDF report of a project
#'
#' @param id Project id.
#' @param fichier Path of the PDF to write (its directory is created).
#' @param langue `"fr"` or `"en"`.
#' @param synthese Optional synthesis comment: one character string,
#'   Markdown allowed, with optional footnote references `[^1]`, `[^2]`...
#'   resolved against `sources`.
#' @param familles Optional family comments: a named list of character
#'   strings, names among the family codes `C`, `B`, `W`, `A`, `F`, `L`, `T`,
#'   `R`, `S`, `P`, `E`, `N`. Markdown allowed; footnote references `[^n]`
#'   are resolved against `sources` and renumbered per family. Empty
#'   comments are dropped.
#' @param sources Optional Markdown list of footnote definitions
#'   (`[^1]: author, title, p. N. <url>`), as produced by the documentary
#'   sources of the AI perspectives. Without it, references stay literal.
#' @return The path of the PDF. Quarto is used when installed, otherwise a
#'   simpler PDF. Only `fichier` is written.
#' @family api_hors_interface
#' @md
#' @export
projet_rapport <- function(id, fichier, langue = "fr", synthese = NULL,
                           familles = NULL, sources = NULL) {
  if (!is.null(synthese) && !(is.character(synthese) && length(synthese) == 1L)) {
    cli::cli_abort("{.arg synthese} doit \u00eatre une cha\u00eene de caract\u00e8res.")
  }
  if (!is.null(familles)) {
    codes <- names(INDICATOR_FAMILIES)
    if (!is.list(familles) || is.null(names(familles)) ||
        !all(names(familles) %in% codes)) {
      cli::cli_abort(c("{.arg familles} doit \u00eatre une liste nomm\u00e9e par code de famille.",
                       i = "Codes : {.val {codes}}."))
    }
    familles <- Filter(function(x) is.character(x) && length(x) == 1L &&
                         nzchar(trimws(x)), familles)
    if (!length(familles)) familles <- NULL
  }
  lu <- .api_results(id)
  src <- sources %||% ""
  if (!is.null(synthese)) synthese <- .prepare_footnotes(synthese, src)
  if (!is.null(familles)) {
    familles <- stats::setNames(
      lapply(names(familles), function(code) {
        .prepare_family_footnotes(familles[[code]], src, code)
      }),
      names(familles))
  }
  dir.create(dirname(fichier), recursive = TRUE, showWarnings = FALSE)
  out <- generate_report_pdf(project = lu$projet, family_scores = lu$familles,
                             output_file = fichier, language = langue,
                             synthesis_comments = synthese,
                             family_comments = familles)
  if (is.null(out) || !file.exists(fichier)) {
    cli::cli_abort("G\u00e9n\u00e9ration du PDF en \u00e9chec : {.path {fichier}}.")
  }
  normalizePath(fichier)
}

#' GeoPackage of a project's results
#'
#' @param id Project id.
#' @param fichier Path of the `.gpkg` to write (overwritten; its directory is
#'   created).
#' @return The path of the GeoPackage: one feature per management unit with
#'   the indicators and the 12 family scores. Only `fichier` is written.
#' @family api_hors_interface
#' @md
#' @export
projet_gpkg <- function(id, fichier) {
  lu <- .api_results(id)
  dir.create(dirname(fichier), recursive = TRUE, showWarnings = FALSE)
  export_geopackage(lu$familles, fichier)
  normalizePath(fichier)
}

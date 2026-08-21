#' Import a project from a CSV list of cadastral references
#'
#' @description
#' Creates a project from a one-line CSV holding the cadastral parcel
#' references of a forest, e.g. `A1;A2;A3;...;AO212;AO220`.
#'
#' **The commune is read from the FILE NAME**, by convention
#' `commune-code_insee.csv` (`couchey-21200.csv`). The file's content carries no
#' commune at all, so nothing else could tell which cadastre the references
#' belong to - `A1` exists in most communes of France.
#'
#' The references are the short form a forester writes: section letters followed
#' by the parcel number, without the leading zeros the cadastre stores
#' (`212000000A0036`). Matching therefore happens on the *pair* (section,
#' numero), never on the raw identifier.
#'
#' @name service_parcelles_csv
#' @keywords internal
NULL


#' Read the commune and the references out of a parcel CSV
#'
#' @description
#' Two independent readings, and both can fail on their own:
#'
#' * the **file name** gives the commune and its INSEE code. A name that does
#'   not follow `commune-code_insee.csv` is refused rather than guessed - a
#'   wrong INSEE would silently fetch another commune's cadastre and match a
#'   few references by coincidence.
#' * the **content** gives the references. Separator is `;`, everything is
#'   trimmed, empties are dropped. Several lines are accepted and concatenated:
#'   nothing depends on the file being one line, that is only how this one
#'   happens to be written.
#'
#' @param path Character. Path to the CSV.
#'
#' @return List with `commune`, `code_insee`, `refs` (character), or `NULL`
#'   with a warning when the name does not follow the convention.
#'
#' @noRd
parse_parcelles_csv <- function(path) {
  if (is.null(path) || !file.exists(path)) {
    cli::cli_warn("CSV introuvable : {path}")
    return(NULL)
  }

  base <- basename(path)
  # `commune-code_insee.csv`. Le code INSEE fait 5 caracteres et peut contenir
  # une lettre (Corse : 2A/2B), d'ou `[0-9AB]` et non `[0-9]`.
  m <- regmatches(base, regexec("^(.+)-([0-9][0-9AB][0-9]{3})\\.csv$", base,
                                ignore.case = TRUE))[[1]]
  if (length(m) != 3L) {
    cli::cli_warn(
      "Nom de fichier hors convention : {.file {base}} \\
       (attendu : commune-code_insee.csv)")
    return(NULL)
  }

  lignes <- tryCatch(readLines(path, warn = FALSE),
                     error = function(e) character(0))
  refs <- unlist(strsplit(paste(lignes, collapse = ";"), ";", fixed = TRUE))
  refs <- toupper(trimws(refs))
  refs <- refs[nzchar(refs)]

  list(
    commune    = .csv_commune_label(m[2]),
    code_insee = toupper(m[3]),
    refs       = unique(refs)
  )
}


#' Turn a file-name fragment into a commune label
#'
#' `couchey` -> `Couchey`, `la-vieille-loye` -> `La Vieille Loye`. Cosmetic
#' only: it names the project, nothing is resolved from it.
#' @noRd
.csv_commune_label <- function(x) {
  mots <- strsplit(gsub("[_-]+", " ", x), " ", fixed = TRUE)[[1]]
  mots <- mots[nzchar(mots)]
  paste(toupper(substring(mots, 1, 1)), substring(mots, 2), sep = "",
        collapse = " ")
}


#' Match short cadastral references against a commune's parcels
#'
#' @description
#' `A1` is section `A`, parcel `1`. The cadastre stores `section = "A"` and
#' `numero = "36"` - as text, sometimes zero-padded. Comparing the pair as
#' INTEGERS on the number side is what makes `A1`, `A01` and `A0001` the same
#' parcel; comparing the raw strings would silently drop most of a list.
#'
#' Sections are not always plain letters (`0A`, `ZB`), so the split takes the
#' TRAILING digits as the number and everything before as the section, rather
#' than assuming a letters-then-digits shape.
#'
#' @param refs Character. Short references.
#' @param parcels `sf` from [get_cadastral_parcels()].
#'
#' @return List with `parcelles` (`sf`, matched) and `absentes` (character,
#'   references the commune does not hold).
#'
#' @noRd
resolve_parcelles_refs <- function(refs, parcels) {
  # `parcelles` doit TOUJOURS supporter `nrow()`, y compris quand l'entree est
  # NULL : renvoyer NULL ferait planter un appelant sur `if (nrow(x) == 0)`,
  # `nrow(NULL)` valant NULL et non 0. D'ou ce sf vide de repli.
  vide_sf <- if (inherits(parcels, "sf")) {
    parcels[0, , drop = FALSE]
  } else {
    sf::st_sf(id = character(0), section = character(0), numero = character(0),
              geometry = sf::st_sfc(crs = 4326))
  }
  vide <- list(parcelles = vide_sf, absentes = refs)

  if (is.null(parcels) || !inherits(parcels, "sf") || nrow(parcels) == 0L) {
    return(vide)
  }
  if (!all(c("section", "numero") %in% names(parcels))) {
    cli::cli_warn("Parcellaire sans colonnes section/numero : appariement impossible")
    return(vide)
  }

  cle <- function(sec, num) {
    num <- suppressWarnings(as.integer(num))
    paste0(toupper(trimws(as.character(sec))), "|", num)
  }

  sec_csv <- sub("[0-9]+$", "", refs)
  num_csv <- sub("^.*?([0-9]+)$", "\\1", refs)
  # Une reference sans chiffre final n'est pas une parcelle : `sub()` la
  # rendrait telle quelle et elle s'apparierait a n'importe quoi.
  valides <- grepl("[0-9]+$", refs) & nzchar(sec_csv)

  cle_csv <- ifelse(valides, cle(sec_csv, num_csv), NA_character_)
  cle_cad <- cle(parcels$section, parcels$numero)

  garde <- cle_cad %in% cle_csv[!is.na(cle_csv)]
  list(
    parcelles = parcels[garde, , drop = FALSE],
    absentes  = refs[is.na(cle_csv) | !cle_csv %in% cle_cad]
  )
}


#' Build the parcel selection of a project from a CSV
#'
#' @description
#' Chains the two steps above with the cadastre fetch, and reports rather than
#' guesses. Three failures are distinguished because they call for three
#' different messages:
#'
#' * `"bad_name"` - the file name does not carry a commune and an INSEE code.
#' * `"no_refs"` - the file holds no reference at all.
#' * `"cadastre"` - the cadastre could not be fetched for that INSEE.
#' * `"no_match"` - references were read, but none exists in that commune.
#'   Almost always an INSEE that does not match the list.
#'
#' A PARTIAL match is a success, not a failure: a list may legitimately name a
#' parcel that has since been merged or renumbered. The caller gets `absentes`
#' and decides what to say.
#'
#' @param path Character. Path to the CSV.
#'
#' @return List with `status`, and on success `parcelles`, `commune`,
#'   `code_insee`, `absentes`.
#'
#' @noRd
importer_parcelles_csv <- function(path) {
  info <- parse_parcelles_csv(path)
  if (is.null(info)) return(list(status = "bad_name"))
  if (length(info$refs) == 0L) {
    return(list(status = "no_refs", commune = info$commune,
                code_insee = info$code_insee))
  }

  parcels <- tryCatch(get_cadastral_parcels(info$code_insee),
                      error = function(e) {
                        cli::cli_warn("Cadastre {info$code_insee} : {conditionMessage(e)}")
                        NULL
                      })
  if (is.null(parcels) || !inherits(parcels, "sf") || nrow(parcels) == 0L) {
    return(list(status = "cadastre", commune = info$commune,
                code_insee = info$code_insee))
  }

  res <- resolve_parcelles_refs(info$refs, parcels)
  if (nrow(res$parcelles) == 0L) {
    return(list(status = "no_match", commune = info$commune,
                code_insee = info$code_insee, absentes = res$absentes))
  }

  cli::cli_alert_success(
    "CSV {info$commune} ({info$code_insee}) : {nrow(res$parcelles)} parcelle{?s} \\
     sur {length(info$refs)} ref{?s}")

  list(status = "ok", parcelles = res$parcelles, commune = info$commune,
       code_insee = info$code_insee, absentes = res$absentes,
       n_refs = length(info$refs))
}

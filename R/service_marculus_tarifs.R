# service_marculus_tarifs.R - numeros de tarif Schaeffer proposes a Marculus
#
# Spec 062 du coeur (brief nemetonshiny du 2026-10-16) : pour chaque essence de
# la feuille de martelage, un numero de tarif Schaeffer par type (SCHR, SCHI,
# SCHL, SCHTL), calcule sur les placettes IFN de la FORET quand elle en compte
# assez, sinon pris dans la table de reference (SER, GRECO, France). Tout le
# calcul est dans `nemeton::tarif_schaeffer_local()` ; ici on le lance, on le
# met en cache et on le met en forme pour le `.marsync`.
#
# Le calcul telecharge les donnees brutes IFN : il tourne dans le worker qui
# prepare deja les fonds ortho (`marculus_ortho_task`, Plan d'actions), jamais
# dans l'export, qui est un `downloadHandler` synchrone. L'export ne fait que
# LIRE le cache ; sans cache, il part sans proposition et le dit.
#
# Cles emises (`ser`, `millesimeTarifs`, `tarifsProposes`) : des PROPOSITIONS,
# que Marculus ne recopie que dans un numero encore vide. On n'emet jamais
# `tarif`, `tarifNumero` ni `tarifNumeroParEssence`, reglages du terrain : une
# cle presente dans le `.marsync` ecraserait le choix du marteleur.

# Niveaux du coeur -> niveaux attendus par Marculus.
MARCULUS_NIVEAUX_TARIF <- c(foret = "foret", SER = "ser", GRECO = "greco",
                            FRANCE = "national")

#' Path of a project's cached Schaeffer tariff proposal
#' @noRd
.marculus_tarifs_chemin <- function(project_path) {
  if (is.null(project_path)) return(NULL)
  file.path(project_path, "cache", "marculus_tarifs.json")
}

#' Outline of the whole forest, for the IFN extraction
#'
#' Every parcel of the project (not one UGF: they are too small for the IFN,
#' and every action of a project shares the same proposal).
#' @noRd
.marculus_tarifs_unites <- function(project) {
  for (u in list(project$parcels, project$tenements)) {
    if (inherits(u, "sf") && nrow(u) > 0L) return(sf::st_sf(geometry = sf::st_geometry(u)))
  }
  NULL
}

#' Signature of what a cached proposal was computed for
#'
#' Species list and forest extent: when either changes, the cache is stale.
#' @noRd
.marculus_tarifs_signature <- function(unites, essences) {
  bb <- if (is.null(unites)) rep(NA_real_, 4) else
    round(as.numeric(sf::st_bbox(sf::st_transform(unites, 2154))))
  paste(c(sort(unique(as.character(essences))), nrow(unites) %||% 0L, bb),
        collapse = "|")
}

#' Tariff job of a project, or `NULL` when the cache is up to date
#'
#' @param project The loaded project.
#' @param essences Species labels of the marking sheet (default: the group
#'   profile's, as [marculus_export_bundle()]).
#' @return `NULL`, or `list(unites, essences, signature, chemin)` for
#'   [marculus_tarifs_preparer()].
#' @noRd
marculus_tarifs_travail <- function(project, essences = NULL) {
  if (is.null(essences)) essences <- get_groupes_essences(project$metadata$groupes_profile)
  essences <- unique(essences[!is.na(essences) & nzchar(essences)])
  chemin <- .marculus_tarifs_chemin(project$path %||% project$project_path)
  unites <- .marculus_tarifs_unites(project)
  if (is.null(chemin) || is.null(unites) || !length(essences)) return(NULL)
  sig <- .marculus_tarifs_signature(unites, essences)
  cache <- marculus_tarifs_lire(chemin)
  # Un echec (reseau IFN, couche SER) se retente au prochain clic.
  if (!is.null(cache) && identical(cache$signature, sig) &&
      identical(cache$statut, "ok")) return(NULL)
  list(unites = unites, essences = essences, signature = sig, chemin = chemin)
}

#' Read a cached tariff proposal
#' @return The cached list, or `NULL` when absent or unreadable.
#' @noRd
marculus_tarifs_lire <- function(chemin) {
  if (is.null(chemin) || !file.exists(chemin)) return(NULL)
  tryCatch(jsonlite::fromJSON(chemin, simplifyVector = FALSE),
           error = function(e) NULL)
}

#' Compute and cache a project's Schaeffer tariff proposal
#'
#' Runs in the background worker. Species labels are resolved to IFN codes
#' with [nemeton::resoudre_espar()]; a label it cannot resolve (a genus such
#' as "Frene" spans several species) gets no proposal. One call to
#' [nemeton::tarif_schaeffer_local()] for the whole forest.
#'
#' @param travail A job from [marculus_tarifs_travail()].
#' @param calculer Injection point for tests (defaults to the core function).
#' @return The cached list: `statut` (`"ok"` / `"echec"`), `signature`,
#'   `ser`, `millesime`, `n_essences`, `essences` (label -> proposal, see
#'   [.marculus_tarifs_essence()]), `erreur`.
#' @noRd
marculus_tarifs_preparer <- function(travail,
                                     calculer = nemeton::tarif_schaeffer_local) {
  if (is.null(travail)) return(NULL)
  espar <- nemeton::resoudre_espar(travail$essences)
  res <- list(statut = "ok", signature = travail$signature, ser = NULL,
              millesime = NULL, n_essences = length(travail$essences),
              essences = stats::setNames(list(), character(0)), erreur = NULL)
  connus <- !is.na(espar)
  if (any(connus)) {
    ser <- tryCatch({
      s <- nemeton::localiser_ser(sf::st_transform(travail$unites, 2154))$ser
      s <- s[!is.na(s)]
      if (length(s)) names(sort(table(s), decreasing = TRUE))[1] else NULL
    }, error = function(e) NULL)
    tab <- tryCatch(calculer(travail$unites, especes = unique(espar[connus]), ser = ser),
                    error = function(e) e)
    if (inherits(tab, "error")) {
      res$statut <- "echec"
      res$erreur <- conditionMessage(tab)
    } else {
      # SER retenue : la notre, sinon celle d'une ligne de repli SER.
      if (is.null(ser) && any(tab$niveau_utilise %in% "SER")) {
        ser <- tab$code[tab$niveau_utilise %in% "SER"][1]
      }
      res$ser <- ser
      res$millesime <- .marculus_tarifs_millesime()
      for (i in which(connus)) {
        p <- .marculus_tarifs_essence(tab[tab$espar == espar[i], , drop = FALSE])
        if (!is.null(p)) res$essences[[travail$essences[i]]] <- p
      }
    }
  }
  dir.create(dirname(travail$chemin), recursive = TRUE, showWarnings = FALSE)
  tmp <- paste0(travail$chemin, ".tmp")
  writeLines(as.character(jsonlite::toJSON(res, auto_unbox = TRUE, null = "null",
                                           digits = NA)), tmp)
  file.rename(tmp, travail$chemin)
  res
}

#' IFN vintage of the reference table ("IFN 2011-2025")
#' @noRd
.marculus_tarifs_millesime <- function() {
  m <- tryCatch(nemeton::ifn_tarif_schaeffer_ser(code = "FR")$millesime,
                error = function(e) character(0))
  m <- unique(m[!is.na(m) & nzchar(m)])
  if (!length(m)) return(NULL)
  m <- m[1]
  if (grepl("^IFN", m)) m else paste("IFN", m)
}

#' Proposal of one species, in the `.marsync` shape
#'
#' @param d Rows of [nemeton::tarif_schaeffer_local()] for one species.
#' @return `list(SCHL = list(numero, niveau, code, nArbres, nPlacettes, cv),
#'   ..., conseille = "SCHL")`, or `NULL` when no level qualified. `cv` goes
#'   from percent (core) to a fraction (Marculus).
#' @noRd
.marculus_tarifs_essence <- function(d) {
  d <- d[!is.na(d$numero), , drop = FALSE]
  if (!nrow(d)) return(NULL)
  out <- list()
  for (k in seq_len(nrow(d))) {
    niv <- unname(MARCULUS_NIVEAUX_TARIF[d$niveau_utilise[k]])
    if (is.na(niv)) niv <- tolower(d$niveau_utilise[k])
    e <- list(numero = round(d$numero[k], 1), niveau = niv)
    # `code` reste present a null au niveau foret (cle attendue par Marculus).
    e["code"] <- list(if (is.na(d$code[k])) NULL else d$code[k])
    e$nArbres <- as.integer(d$n_arbres[k])
    np <- d$n_placettes[k] %||% NA
    e["nPlacettes"] <- list(if (is.na(np)) NULL else as.integer(np))
    e["cv"] <- list(if (is.na(d$cv[k])) NULL else round(d$cv[k] / 100, 3))
    out[[d$type[k]]] <- e
  }
  cons <- d$type_conseille[!is.na(d$type_conseille)]
  if (length(cons)) out$conseille <- cons[1]
  out
}

#' Proposal to ship, if the cache matches the export
#'
#' @param project The loaded project.
#' @param essences Species of the marking sheets.
#' @return `list(tarifs, statut)`: `tarifs` is the cached proposal or `NULL`;
#'   `statut` is `"ok"`, `"non_calcule"` (no cache, or stale) or `"echec"`
#'   (with `erreur`).
#' @noRd
marculus_tarifs_pour_export <- function(project, essences) {
  chemin <- .marculus_tarifs_chemin(project$path %||% project$project_path)
  cache <- marculus_tarifs_lire(chemin)
  unites <- .marculus_tarifs_unites(project)
  essences <- unique(essences[!is.na(essences) & nzchar(essences)])
  sig <- .marculus_tarifs_signature(unites, essences)
  if (is.null(cache) || !identical(cache$signature, sig)) {
    return(list(tarifs = NULL, statut = "non_calcule"))
  }
  if (!identical(cache$statut, "ok")) {
    return(list(tarifs = NULL, statut = "echec", erreur = cache$erreur %||% ""))
  }
  list(tarifs = cache, statut = "ok")
}

#' Count of species with a proposal, per level (end-of-export message)
#'
#' The level of a species is the one of its advised type (or of its first
#' type): every type of a species comes from the same level.
#' @return `list(n, foret, ser, repli)`.
#' @noRd
.marculus_tarifs_comptes <- function(tarifs, essences) {
  props <- tarifs$essences[intersect(names(tarifs$essences %||% list()), essences)]
  niv <- vapply(props, function(p) {
    t <- p[[p$conseille %||% ""]] %||% p[[setdiff(names(p), "conseille")[1]]]
    t$niveau %||% NA_character_
  }, character(1))
  list(n = length(props), foret = sum(niv == "foret", na.rm = TRUE),
       ser = sum(niv == "ser", na.rm = TRUE),
       repli = sum(niv %in% c("greco", "national")))
}

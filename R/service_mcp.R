# Serveur MCP : outils exposes a un assistant (Claude Code via AIGORA, VICTOR).
#
# specs/BRIEF-pilotage-victor-aigora.md, A.2 et A.3. Fines enveloppes de l'API
# hors interface (R/api.R) : aucune logique metier, aucune lecture qui ecrit.
# Lanceur : inst/mcp/server.R. Chaque outil rend une chaine JSON courte, faite
# pour etre lue par un modele (et pour remplir le `display_report` de VICTOR).
# stdout est le canal du protocole MCP (stdio) : rien n'y est ecrit ici.

# ---------------------------------------------------------------- utilitaires

.mcp_json <- function(x) {
  as.character(jsonlite::toJSON(x, auto_unbox = TRUE, null = "null",
                                na = "null", digits = 4))
}

# Evalue `expr`. Succes : chaine JSON `{ok: true, ...}`. Echec : chaine JSON
# `{ok, classe, code, langue, erreur, action, candidats}` portant l'attribut
# `mcp_erreur` (voir .mcp_erreur()). C'est `mcp_tools()` qui la convertit en
# `ellmer::ContentToolResult` en erreur, que mcptools marque `isError: true`
# (le client lit "Tool calling failed with error <json>") : les fonctions
# `mcp_*` gardent ainsi leur contrat de chaine JSON.
# `langue` n'est evaluee qu'en cas d'erreur ; FR par defaut.
.mcp_call <- function(expr, langue = "fr") {
  res <- tryCatch(expr, error = function(e) e)
  if (inherits(res, "error")) return(.mcp_erreur(res, langue))
  .mcp_json(Filter(Negate(is.null), c(list(ok = TRUE), res)))
}

# Classe d'erreur (sans le prefixe `nemetonshiny_`) -> action (cle
# `mcp_act_<action>`) ; le message est `mcp_err_<code>` (utils_i18n.R).
# `detail` : le message technique de la condition est joint au texte (il porte
# ce que le texte fixe ne dit pas : IDU, parcelles, statut).
.MCP_ERREURS <- list(
  projet_introuvable     = list(action = "lister",    detail = FALSE),
  projet_ambigu          = list(action = "choisir",   detail = FALSE),
  projet_verrouille      = list(action = "verrou",    detail = FALSE),
  projet_ancien          = list(action = "recreer",   detail = FALSE),
  calcul_en_cours        = list(action = "suivre",    detail = FALSE),
  calcul_echec           = list(action = "suivre",    detail = TRUE),
  sans_indicateurs       = list(action = "calculer",  detail = FALSE),
  fichier_invalide       = list(action = "fichier",   detail = TRUE),
  idu_inconnu            = list(action = "fichier",   detail = TRUE),
  pavage_invalide        = list(action = "fichier",   detail = TRUE),
  onf_echec              = list(action = "reessayer", detail = TRUE),
  parcelles_introuvables = list(action = "signaler",  detail = TRUE),
  confirmation_requise   = list(action = "confirmer", detail = FALSE),
  jeton_invalide         = list(action = "redemander", detail = TRUE),
  erreur                 = list(action = "signaler",  detail = TRUE),
  erreur_inconnue        = list(action = "signaler",  detail = TRUE)
)

.mcp_langue <- function(langue) {
  l <- tryCatch(as.character(langue)[1], error = function(e) NA_character_)
  if (!is.na(l) && l %in% c("fr", "en")) l else "fr"
}

# Construit l'erreur MCP d'une condition R : classee, bilingue, avec l'action
# suivante. Aucun texte en dur : tout vient de i18n (cles mcp_err_*, mcp_act_*).
.mcp_erreur <- function(e, langue = "fr") {
  langue <- .mcp_langue(langue)
  i18n <- get_i18n(langue)
  cls <- sub("^nemetonshiny_", "",
             grep("^nemetonshiny_", class(e), value = TRUE))
  cls <- setdiff(cls, "erreur")
  code <- if (length(cls) && cls[1] %in% names(.MCP_ERREURS)) {
    cls[1]
  } else if (length(cls) || inherits(e, "nemetonshiny_erreur")) {
    "erreur"
  } else {
    "erreur_inconnue"
  }
  def <- .MCP_ERREURS[[code]]
  projet <- as.character(e$projet %||% "")[1]
  detenteur <- as.character(e$detenteur %||% "?")[1]
  # Un projet nomme (confirmation_requise) est cite par son nom ET son id.
  projet_txt <- if (is.null(e$projet_nom)) projet else {
    sprintf(if (langue == "fr") "\u00ab %s \u00bb (identifiant %s)" else
              "\u201c%s\u201d (id %s)", as.character(e$projet_nom)[1], projet)
  }
  texte <- i18n$t(paste0("mcp_err_", code), projet = projet_txt,
                  detenteur = detenteur)
  if (isTRUE(def$detail)) {
    detail <- gsub("\\s*\n\\s*", " ", cli::ansi_strip(conditionMessage(e)))
    texte <- paste0(texte, " (", trimws(detail), ")")
  }
  action <- i18n$t(paste0("mcp_act_", def$action), detenteur = detenteur)
  # `projet`, `projet_nom`, `jeton`, `apercu`, `expire_a` : portes par
  # confirmation_requise seule
  # (.mcp_jeton_emettre()).
  json <- .mcp_json(Filter(Negate(is.null), list(
    ok = FALSE, classe = paste0("nemetonshiny_", code), code = code,
    langue = langue, erreur = texte, action = action,
    candidats = e$candidats,
    projet = if (!is.null(e$projet_nom)) projet, projet_nom = e$projet_nom,
    jeton = e$jeton, apercu = e$apercu,
    expire_a = e$expire_a)))
  structure(json, mcp_erreur = TRUE)
}

# Une chaine d'erreur de .mcp_call() devient un resultat d'outil en erreur
# (`isError`) ; un succes passe tel quel.
.mcp_vers_outil <- function(x) {
  if (isTRUE(attr(x, "mcp_erreur"))) {
    return(ellmer::ContentToolResult(error = as.character(x)))
  }
  x
}

# Enveloppe `f` (meme signature, pour ellmer::tool) : son resultat passe par
# .mcp_vers_outil().
.mcp_marquer <- function(f) {
  noms <- names(formals(f))
  appel <- as.call(c(list(quote(.f)),
                     stats::setNames(lapply(noms, as.name), noms)))
  g <- f
  body(g) <- call(".mcp_vers_outil", appel)
  environment(g) <- list2env(list(.f = f), parent = environment(f))
  g
}

.mcp_norm <- function(x) {
  x <- iconv(as.character(x), from = "UTF-8", to = "ASCII//TRANSLIT", sub = "")
  tolower(trimws(x))
}

# Table de correspondance (points de code) des lettres accentuees vers l'ASCII.
# Minuscules U+00E0..U+00FF utiles ; majuscules = minuscule - 32, sauf U+00FF
# (y trema : sa majuscule U+0178 est a part, sinon U+00DF donnerait "y").
# Ligatures a deux lettres : oe (U+0153, U+0152), ae (U+00E6, U+00C6).
.MCP_ACCENTS <- local({
  bas <- c(224L, 225L, 226L, 227L, 228L, 229L,  # a
           232L, 233L, 234L, 235L,              # e
           236L, 237L, 238L, 239L,              # i
           242L, 243L, 244L, 245L, 246L,        # o
           249L, 250L, 251L, 252L,              # u
           231L, 241L, 253L)                    # c n y
  cible <- strsplit("aaaaaaeeeeiiiiooooouuuucny", "")[[1]]
  cp <- c(bas, bas - 32L, 255L, 376L, 339L, 338L, 230L, 198L)
  val <- c(cible, cible, "y", "y", "oe", "oe", "ae", "ae")
  stats::setNames(val, as.character(cp))
})

# Normalisation d'un nom dicte : minuscules, sans accents, tout non-alphanumerique
# devient une espace. Travaille par points de code (utf8ToInt/intToUtf8) : aucun
# appel dependant de la locale (chartr et tolower echouent sous LC_ALL=C). Une
# chaine non marquee et UTF-8 valide (locale C) est lue telle quelle ; enc2utf8
# la reinterpreterait comme du natif ASCII.
.mcp_norm_dicte <- function(x) {
  un <- function(s) {
    if (is.na(s)) return("")
    cp <- if (Encoding(s) == "unknown" && validUTF8(s)) utf8ToInt(s) else utf8ToInt(enc2utf8(s))
    if (!length(cp)) return("")
    maj <- cp >= 65L & cp <= 90L
    cp[maj] <- cp[maj] + 32L
    acc <- match(as.character(cp), names(.MCP_ACCENTS))
    lettres <- vapply(seq_along(cp), function(i) {
      if (!is.na(acc[i])) return(unname(.MCP_ACCENTS[acc[i]]))
      ok <- (cp[i] >= 97L && cp[i] <= 122L) || (cp[i] >= 48L && cp[i] <= 57L)
      intToUtf8(if (ok) cp[i] else 32L)
    }, character(1))
    trimws(gsub(" +", " ", paste(lettres, collapse = "")))
  }
  vapply(as.character(x), un, character(1), USE.NAMES = FALSE)
}

# Retire l'introduction "Foret [communale] de/du/des/d'" d'un nom normalise
# (l'apostrophe, droite ou typographique, est deja devenue une espace).
.mcp_sans_foret <- function(m) {
  trimws(sub("^foret (communale )?(de|du|des|d) ", "", m))
}

# Mot-cle (commune) d'un nom de projet : "Foret communale de X (ONF)" ou "Foret de X" -> "x" ;
# sans commune dans le nom, le nom lui-meme.
.mcp_motcle <- function(nom) {
  m <- .mcp_sans_foret(.mcp_norm_dicte(nom))
  trimws(sub(" onf$", "", m))
}

#' Indices of the projects matching a dictated name
#'
#' The keyword of a project is its commune, taken from its name ("Foret communale
#' de Couchey (ONF)" -> "couchey"; no commune in the name: the name itself).
#' Names are compared without accents or case. The introduction "Foret
#' [communale] de/du/des/d'" (straight or typographic apostrophe) is removed on
#' both sides, so "Foret de Dabot" finds "Foret de Dabo". A project matches
#' when the dictated name is equal to its keyword, or is a run of whole words at
#' the start of the keyword, or is within `seuil` edit distance of it. The distance applies only when both the dictated
#' name and the keyword have at least 4 letters (floor on both sides); below
#' that, only equality or a leading run of whole words counts ("de", "du" never
#' match "Foret de Be"; "la" never matches "Laix").
#' An exact keyword match wins over distance-only matches only when no other
#' project is within edit distance 1 of the dictated name; otherwise every
#' candidate is returned (ambiguity). "Ecouche" next to Couchey (distance 2)
#' finds Ecouche; "Couchey" next to Couchay (distance 1) returns both.
#' Never a bare substring ("de", "onf" match nothing). The threshold 2 is
#' provisional (spec P1, criterion 1).
#'
#' @param dit Character. The dictated name.
#' @param noms Character. Project names.
#' @param seuil Integer. Maximum edit distance.
#' @return Integer indices into `noms`.
#' @noRd
.mcp_rapprocher <- function(dit, noms, seuil = 2L) {
  d <- .mcp_norm_dicte(dit)
  variantes <- unique(c(d, .mcp_sans_foret(d)))
  variantes <- variantes[nzchar(variantes)]
  if (!length(variantes)) return(integer(0))
  mc <- vapply(noms, .mcp_motcle, character(1), USE.NAMES = FALSE)
  # Rang 0 : egalite ou suite de mots entiers en tete. Rang 1 : distance <= seuil
  # (plancher de 4 lettres des deux cotes). Une egalite exacte l'emporte sur les
  # voisins de rang 1 ("Ecouche" trouve Ecouche, pas Couchey) SAUF si un autre
  # projet est a distance <= 1 du mot dicte ("Couchey" face a Couchay : les deux
  # restent candidats) ; sans egalite exacte, tous les projets de rang 0 ou 1.
  dist <- function(v, m) if (min(nchar(v), nchar(m)) >= 4L) drop(utils::adist(v, m)) else Inf
  rang <- vapply(mc, function(m) {
    if (any(vapply(variantes, function(v) identical(m, v) || startsWith(m, paste0(v, " ")),
                   logical(1)))) return(0L)
    proche <- any(vapply(variantes, function(v) dist(v, m) <= seuil, logical(1)))
    if (proche) 1L else NA_integer_
  }, integer(1), USE.NAMES = FALSE)
  exact <- which(mc %in% variantes)
  if (length(exact)) {
    voisin <- vapply(seq_along(mc), function(i) {
      !(mc[i] %in% variantes) && any(vapply(variantes, function(v) dist(v, mc[i]) <= 1, logical(1)))
    }, logical(1))
    if (!any(voisin)) return(unname(which(rang == 0L)))
  }
  unname(which(!is.na(rang)))
}

#' Resolve a project from an id or an approximate name
#'
#' Exact id first, then exact name (case and accents ignored), then the
#' tolerant keyword match of `.mcp_rapprocher()`. Several candidates: error of class
#' `nemetonshiny_projet_ambigu` carrying `candidats`, so the assistant can ask.
#' Never a silent choice.
#'
#' @param projet Character. Project id or name.
#' @return The project id.
#' @noRd
.mcp_resolve_project <- function(projet) {
  if (!is.character(projet) || length(projet) != 1L || !nzchar(trimws(projet))) {
    .api_abort("Projet non pr\u00e9cis\u00e9.", "nemetonshiny_projet_introuvable", projet = projet)
  }
  l <- projets_lister()
  if (projet %in% l$id) return(projet)
  idx <- which(.mcp_norm(l$nom) == .mcp_norm(projet))
  if (!length(idx)) idx <- .mcp_rapprocher(projet, l$nom)
  if (length(idx) == 1L) return(l$id[idx])
  if (!length(idx)) {
    .api_abort("Aucun projet ne correspond \u00e0 {.val {projet}}.",
               "nemetonshiny_projet_introuvable", projet = projet)
  }
  candidats <- sprintf("%s (%s)", l$nom[idx], l$id[idx])
  .api_abort(c("Plusieurs projets correspondent \u00e0 {.val {projet}}.",
               i = "{candidats}"),
             "nemetonshiny_projet_ambigu", projet = projet, candidats = candidats)
}

# ---------------------------------------------------------------- lecture

#' @noRd
mcp_lister_projets <- function(limite = 20) {
  .mcp_call({
    l <- projets_lister()
    n <- nrow(l)
    l <- utils::head(l, max(1L, as.integer(limite %||% 20)))
    list(total = n, projets = l)
  })
}

# Heure naive d'un fichier projet (lue dans le fuseau du process) -> ISO 8601
# avec fuseau Europe/Paris ; NA si absente ou illisible. Accepte aussi un
# POSIXct (mtime d'un fichier).
.mcp_iso_paris <- function(x) {
  if (is.null(x) || length(x) != 1L || is.na(x)) return(NA_character_)
  t <- if (inherits(x, "POSIXct")) x else {
    x <- sub("T", " ", as.character(x), fixed = TRUE)
    suppressWarnings(as.POSIXct(x, format = "%Y-%m-%d %H:%M:%S"))
  }
  if (is.na(t)) return(NA_character_)
  paste0(format(t, "%Y-%m-%dT%H:%M:%S", tz = "Europe/Paris"),
         sub("(\\d{2})(\\d{2})$", "\\1:\\2", format(t, "%z", tz = "Europe/Paris")))
}

# Fin reelle du calcul : completed_at > fin_a (job "termine") > date du parquet.
.mcp_calcule_le <- function(path) {
  f <- file.path(path, "data", "progress_state.json")
  ps <- if (file.exists(f)) tryCatch(jsonlite::read_json(f), error = function(e) NULL)
  iso <- .mcp_iso_paris(ps$completed_at %||% NA_character_)
  if (!is.na(iso)) return(iso)
  job <- .mcp_job_read(path)
  if (identical(job$statut %||% "", "termine")) {
    iso <- .mcp_iso_paris(job$fin_a %||% NA_character_)
    if (!is.na(iso)) return(iso)
  }
  pq <- file.path(path, "data", "indicators.parquet")
  if (file.exists(pq)) return(.mcp_iso_paris(file.mtime(pq)))
  NA_character_
}

#' @noRd
mcp_resume_projet <- function(projet, langue = "fr") {
  .mcp_call({
    id <- .mcp_resolve_project(projet)
    p <- projet_lire(id, langue = langue %||% "fr")
    s <- p$synthese
    meta <- p$projet$metadata %||% list()
    fr <- !identical(langue, "en")

    # Memes chiffres que le tableau de la Synthese : scores a 2 decimales,
    # NDP du projet sur chaque ligne, indicateurs presents (a defaut, prevus).
    moy <- project_family_means(p$familles)
    base_sf <- p$projet$indicators_sf
    noms <- tryCatch({
      if (inherits(base_sf, "sf")) {
        e <- add_regen_r_indicators(add_r5_to_indicators(base_sf, p$projet), p$projet)
        names(sf::st_drop_geometry(e))
      } else character(0)
    }, error = function(e) character(0))
    codes <- names(INDICATOR_FAMILIES)
    fam <- data.frame(
      code = codes,
      famille = vapply(codes, function(c) {
        f <- INDICATOR_FAMILIES[[c]]
        if (fr) f$name_fr else f$name_en
      }, character(1), USE.NAMES = FALSE),
      score = vapply(codes, function(c) {
        col <- get_famille_col(c)
        if (col %in% names(moy)) round(unname(moy[[col]]), 2) else NA_real_
      }, numeric(1), USE.NAMES = FALSE),
      ndp = as.integer(meta$ndp_level %||% 0L),
      n_indicateurs = vapply(codes, function(c) {
        cn <- INDICATOR_FAMILIES[[c]]$column_names %||% INDICATOR_FAMILIES[[c]]$indicators
        n <- sum(vapply(cn, function(k)
          k %in% noms || paste0(k, "_norm") %in% noms, logical(1)))
        as.integer(if (n > 0L) n else length(INDICATOR_FAMILIES[[c]]$indicators))
      }, integer(1), USE.NAMES = FALSE),
      stringsAsFactors = FALSE
    )
    fam$score[is.nan(fam$score)] <- NA_real_
    s$families <- NULL
    s$familles <- fam
    s$description <- meta$description %||% NA_character_
    s$created_at <- .mcp_iso_paris(meta$created_at %||% NA_character_)
    s$updated_at <- .mcp_iso_paris(meta$updated_at %||% NA_character_)
    s$calcule_le <- .mcp_calcule_le(p$projet$path)
    s$chm_suspect <- isTRUE(meta$chm_suspect)
    if (!is.null(meta$chm_suspect_max)) s$chm_suspect_max <- as.numeric(meta$chm_suspect_max)
    s
  }, langue = langue)
}

# ---------------------------------------------------------------- calcul detache

.mcp_job_path <- function(project_path) {
  file.path(project_path, "data", "compute_job.json")
}

.mcp_job_read <- function(project_path) {
  f <- .mcp_job_path(project_path)
  if (!file.exists(f)) return(NULL)
  tryCatch(jsonlite::read_json(f), error = function(e) NULL)
}

.mcp_job_write <- function(project_path, job) {
  .write_json_atomic(job, .mcp_job_path(project_path), auto_unbox = TRUE,
                     pretty = TRUE, null = "null")
  invisible(job)
}

.mcp_pid_alive <- function(pid) {
  pid <- suppressWarnings(as.integer(pid %||% NA))
  length(pid) == 1L && !is.na(pid) && pid > 0L && isTRUE(tools::pskill(pid, 0L))
}

.mcp_job_running <- function(job) {
  !is.null(job) && (job$statut %||% "") %in% c("lancement", "en_cours") &&
    (.mcp_pid_alive(job$pid) ||
       # Lance mais pas encore enregistre par l'enfant : laisser 60 s.
       (is.null(job$pid) && .mcp_age_sec(job$lance_a) < 60))
}

.mcp_age_sec <- function(stamp) {
  t <- suppressWarnings(as.POSIXct(stamp %||% NA_character_,
                                   format = "%Y-%m-%dT%H:%M:%S"))
  if (length(t) != 1L || is.na(t)) return(Inf)
  as.numeric(difftime(Sys.time(), t, units = "secs"))
}

.mcp_now <- function() format(Sys.time(), "%Y-%m-%dT%H:%M:%S")

#' Start the detached computation process
#'
#' A fresh `Rscript`, in its own session (`setsid` when available) so it
#' survives the end of the MCP server - one `claude -p` task per request.
#' @noRd
.mcp_spawn <- function(project_id, project_dir, log_path) {
  expr <- sprintf("nemetonshiny:::.mcp_child_compute(%s, %s)",
                  deparse(project_id), deparse(project_dir))
  rscript <- file.path(R.home("bin"), "Rscript")
  if (nzchar(Sys.which("setsid"))) {
    system2("setsid", c("-f", shQuote(rscript), "-e", shQuote(expr)),
            stdout = log_path, stderr = log_path, wait = FALSE)
  } else {
    system2(rscript, c("-e", shQuote(expr)),
            stdout = log_path, stderr = log_path, wait = FALSE)
  }
  invisible(TRUE)
}

#' Body of the detached computation (runs in the child process)
#'
#' Same path as the application: `.compute_run_capped()` (memory ceiling,
#' child log, file progress). Records its pid then its outcome in
#' `data/compute_job.json`.
#' @noRd
.mcp_child_compute <- function(project_id, project_dir) {
  options(nemeton.app_options = list(project_dir = project_dir))
  path <- get_project_path(project_id)
  if (is.null(path)) stop("Projet introuvable : ", project_id)
  job <- .mcp_job_read(path) %||% list(job_id = NA_character_)
  job$pid <- Sys.getpid()
  job$statut <- "en_cours"
  job$demarre_a <- .mcp_now()
  .mcp_job_write(path, job)

  res <- tryCatch(.compute_run_capped(project_id, get_app_options()),
                  error = function(e) list(success = FALSE, error = conditionMessage(e)))
  job <- .mcp_job_read(path) %||% job
  job$statut <- if (isTRUE(res$success)) "termine"
                else if (isTRUE(res$cancelled)) "annule" else "echec"
  if (!isTRUE(res$success) && !isTRUE(res$cancelled)) {
    err <- res$error %||% res$state$errors %||% "erreur inconnue"
    job$erreur <- paste(unlist(err), collapse = " ; ")
  }
  job$fin_a <- .mcp_now()
  .mcp_job_write(path, job)
  invisible(job)
}

#' Refuse to write into a project that is busy
#'
#' Shared by every tool that writes: a computation running (detached or in the
#' application) gives `nemetonshiny_calcul_en_cours`, an edit lock held in the
#' application gives `nemetonshiny_projet_verrouille`.
#'
#' @param id Project id.
#' @return The [projet_etat()] list.
#' @noRd
.mcp_garde_ecriture <- function(id, call = rlang::caller_env()) {
  etat <- projet_etat(id)
  .api_refuser_ancien(etat, call = call)
  if (.mcp_job_running(.mcp_job_read(etat$chemin))) {
    .api_abort("Un calcul est d\u00e9j\u00e0 en cours pour ce projet.",
               "nemetonshiny_calcul_en_cours", call = call)
  }
  # Un fichier recent ne veut dire calcul vivant que s'il n'est pas termine :
  # le calcul qui finit l'ecrit une derniere fois a `completed`.
  if (progress_state_age_sec(id) < 60 &&
      !progress_state_termine(read_progress_state(id))) {
    .api_abort("L'application calcule ce projet en ce moment.",
               "nemetonshiny_calcul_en_cours", call = call)
  }
  verrou <- tryCatch(lock_status(id), error = function(e) NULL)
  if (!is.null(verrou) && !isTRUE(verrou$stale)) {
    detenteur <- verrou$holder_label %||% verrou$holder_id %||% "?"
    .api_abort("Projet en cours d'\u00e9dition dans l'application ({detenteur}).",
               "nemetonshiny_projet_verrouille", detenteur = detenteur,
               call = call)
  }
  etat
}

# ---------------------------------------------------------------- confirmation

# ADR 0002 : toute ecriture d'un projet existant se fait en deux temps. Le
# premier appel n'ecrit rien et rend un apercu et un jeton ; seul un second
# appel portant ce jeton ecrit. Barriere, pas preuve : un agent qui enchaine
# seul les deux appels contourne la garde.

# Duree de validite d'un jeton, en secondes. PROVISOIRE (fiche 009) : a
# revoir apres l'essai VICTOR. Seule definition.
.MCP_JETON_DUREE <- 10 * 60

# Registre des jetons, en memoire du processus serveur (ADR 0002) : rien
# n'est ecrit dans le dossier du projet a l'apercu. Cle : le jeton ; valeur :
# list(outil, projet, lien, expire).
.mcp_jetons <- new.env(parent = emptyenv())

.mcp_jetons_purger <- function(maintenant) {
  for (k in ls(.mcp_jetons, all.names = TRUE)) {
    if (.mcp_jetons[[k]]$expire < maintenant) rm(list = k, envir = .mcp_jetons)
  }
  invisible()
}

#' Issue a confirmation token instead of writing
#'
#' First step of a write (ADR 0002): records a single-use token, valid
#' `.MCP_JETON_DUREE` seconds, bound to `outil`, `projet` and `lien`, then
#' signals `nemetonshiny_confirmation_requise` carrying `jeton`, `apercu` and
#' `expire_a` (ISO 8601, UTC). Call it after the guards and every business
#' check, and before any write. The registry lives in memory only.
#'
#' @param outil Character. Name of the tool.
#' @param projet Character. Resolved project id.
#' @param lien What the confirmation must present again unchanged (compared
#'   with [identical()]): file and its md5, settings...
#' @param apercu Named list read to the user: what the write will change.
#' @return Never returns: always signals the condition.
#' @noRd
.mcp_jeton_emettre <- function(outil, projet, lien, apercu) {
  maintenant <- Sys.time()
  .mcp_jetons_purger(maintenant)
  repeat {
    jeton <- paste(sample(c(0:9, letters), 24L, replace = TRUE), collapse = "")
    if (!exists(jeton, envir = .mcp_jetons, inherits = FALSE)) break
  }
  expire <- maintenant + .MCP_JETON_DUREE
  # Nom du projet : cite dans le message, pour que l'utilisateur voie QUEL
  # projet sera modifie (le jeton reste lie a l'id).
  nom <- tryCatch({
    l <- projets_lister()
    as.character(l$nom[match(projet, l$id)])[1]
  }, error = function(e) NA_character_)
  if (is.na(nom) || !nzchar(nom)) nom <- projet
  assign(jeton, list(outil = outil, projet = projet, lien = lien,
                     expire = expire), envir = .mcp_jetons)
  .api_abort("Confirmation requise : rien n'a \u00e9t\u00e9 \u00e9crit.",
             "nemetonshiny_confirmation_requise", projet = projet,
             projet_nom = nom, jeton = jeton, apercu = apercu,
             expire_a = format(expire, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"))
}

#' Consume a confirmation token before writing
#'
#' Second step of a write (ADR 0002). A known token is removed from the
#' registry as soon as it is presented, whatever follows: a refused guard or a
#' refused binding kills it too. Exception: errors raised by the caller before
#' this function runs (unknown or ambiguous project, unreadable ONF metadata)
#' leave the token valid. Then `garde(projet)` is replayed (its error
#' passes through), and the token is checked: unknown, expired, or issued for
#' another tool, project or `lien` gives `nemetonshiny_jeton_invalide`.
#' `lien` is only evaluated after the guard.
#'
#' @param jeton Character. Token returned by [.mcp_jeton_emettre()].
#' @param outil,projet,lien As given to [.mcp_jeton_emettre()], recomputed now.
#' @param garde Function of the project id replayed before the check, or
#'   `NULL`. Default [.mcp_garde_ecriture()].
#' @return `TRUE`, invisibly, when the write may go on.
#' @noRd
.mcp_jeton_consommer <- function(jeton, outil, projet, lien,
                                 garde = .mcp_garde_ecriture) {
  cle <- if (is.character(jeton) && length(jeton) == 1L && !is.na(jeton) &&
               nzchar(jeton)) jeton
  entree <- if (!is.null(cle)) get0(cle, envir = .mcp_jetons, inherits = FALSE)
  if (!is.null(entree)) rm(list = cle, envir = .mcp_jetons)
  if (is.function(garde)) garde(projet)
  motif <- if (is.null(entree)) {
    "Jeton inconnu ou d\u00e9j\u00e0 utilis\u00e9."
  } else if (Sys.time() > entree$expire) {
    "Jeton p\u00e9rim\u00e9."
  } else if (!identical(entree$outil, outil) || !identical(entree$projet, projet)) {
    "Jeton d\u00e9livr\u00e9 pour un autre outil ou un autre projet."
  } else if (!identical(entree$lien, lien)) {
    "Param\u00e8tres, fichier ou r\u00e9glages diff\u00e9rents de ceux de l'aper\u00e7u."
  }
  if (!is.null(motif)) {
    .api_abort(motif, "nemetonshiny_jeton_invalide", projet = projet)
  }
  invisible(TRUE)
}

#' Fingerprint bound to a token
#'
#' md5 of the project's parcel files, plus what the tool adds in `...`: a
#' parcel set changed between the preview and the confirmation breaks the
#' binding.
#' @noRd
.mcp_empreinte <- function(id, ...) {
  d <- file.path(get_project_path(id) %||% "", "data")
  f <- file.path(d, c("parcels.gpkg", "parcels.parquet"))
  f <- f[file.exists(f)]
  list(parcelles = stats::setNames(unname(tools::md5sum(f)), basename(f)), ...)
}

#' @noRd
mcp_lancer_calcul <- function(projet) {
  .mcp_call({
    id <- .mcp_resolve_project(projet)
    etat <- .mcp_garde_ecriture(id)
    path <- etat$chemin
    log_path <- file.path(path, "data", "compute_mcp.log")
    job <- list(job_id = paste0(format(Sys.time(), "%Y%m%d%H%M%S"), "-",
                                paste0(sample(letters, 4), collapse = "")),
                statut = "lancement", source = "mcp", lance_a = .mcp_now(),
                log = log_path)
    .mcp_job_write(path, job)
    .mcp_spawn(id, get_projects_root(), log_path)
    list(projet = id, job_id = job$job_id, log = log_path,
         message = "Calcul lanc\u00e9 en t\u00e2che de fond ; suivre avec etat_calcul.")
  })
}

.mcp_log_tail <- function(log_path, n = 15L) {
  if (is.null(log_path) || !file.exists(log_path)) return(NULL)
  utils::tail(readLines(log_path, warn = FALSE), n)
}

#' @noRd
mcp_etat_calcul <- function(projet) {
  .mcp_call({
    id <- .mcp_resolve_project(projet)
    path <- get_project_path(id)
    job <- .mcp_job_read(path)
    statut <- job$statut %||% "aucun"
    if (statut %in% c("lancement", "en_cours") && !.mcp_job_running(job)) {
      statut <- "echec"
      job$erreur <- "Le processus de calcul s'est arr\u00eat\u00e9 sans terminer."
    }
    prog <- read_progress_state(id)
    list(
      projet = id,
      statut = statut,
      job_id = job$job_id,
      lance_a = job$lance_a,
      fin_a = job$fin_a,
      erreur = job$erreur,
      phase = prog$status,
      progression = prog$progress,
      progression_max = prog$progress_max,
      indicateurs_faits = prog$indicators_completed,
      indicateurs_total = prog$indicators_total,
      tache = prog$current_task,
      log = if (identical(statut, "echec")) .mcp_log_tail(job$log)
    )
  })
}

#' @noRd
mcp_annuler_calcul <- function(projet) {
  .mcp_call({
    id <- .mcp_resolve_project(projet)
    cancel_computation(id)
    list(projet = id, message = "Annulation demand\u00e9e ; le calcul s'arr\u00eate au prochain indicateur.")
  })
}

# ---------------------------------------------------------------- UGF

#' Load a project for writing its management units
#'
#' Parcels, tenements and UGF only - what [save_ug_data()] and
#' [save_parcels()] write back. A project without UGF gets the default layout
#' (one UGF per parcel) in memory.
#' @noRd
.mcp_projet_ugf <- function(id) {
  meta <- load_project_metadata(id) %||% list()
  meta$id <- meta$id %||% id
  projet <- list(id = id, metadata = meta, parcels = load_parcels(id))
  if (is.null(projet$parcels) || nrow(projet$parcels) == 0L) {
    .api_abort("Projet {.val {id}} : aucune parcelle cadastrale.",
               "nemetonshiny_projet_introuvable")
  }
  ug <- load_ug_data(id)
  if (is.null(ug)) {
    projet <- ug_init_default(projet)
  } else {
    projet$tenements <- ug$tenements
    projet$ugs <- ug$ugs
  }
  projet
}

#' Gap between each parcel and the tenements given for it
#'
#' `|parcel - union|` (holes, overflow) plus `sum - union` (overlaps), in m2,
#' in Lambert 93. A parcel passes under `max(1, 0.05 %)` of its area, the
#' tolerance of [validate_tiling()].
#'
#' @param parcels `sf` of the project's parcels, `idu` column.
#' @param imp `sf` of tenements, `idu` column.
#' @return data.frame `idu`, `ecart_m2`, `ok`.
#' @noRd
.mcp_ecart_pavage <- function(parcels, imp) {
  prev <- sf::sf_use_s2()
  suppressMessages(sf::sf_use_s2(FALSE))
  on.exit(suppressMessages(sf::sf_use_s2(prev)), add = TRUE)
  p <- sf::st_make_valid(sf::st_transform(parcels, 2154))
  t <- sf::st_make_valid(sf::st_transform(imp, 2154))
  ids <- unique(as.character(t$idu))
  ecart <- vapply(ids, function(i) {
    gp <- sf::st_geometry(p[as.character(p$idu) == i, ])
    gt <- sf::st_geometry(t[as.character(t$idu) == i, ])
    a_p <- sum(as.numeric(sf::st_area(gp)))
    a_s <- sum(as.numeric(sf::st_area(gt)))
    a_u <- as.numeric(sf::st_area(sf::st_union(gt)))
    a_i <- as.numeric(sf::st_area(suppressWarnings(
      sf::st_intersection(sf::st_union(gp), sf::st_union(gt)))))
    if (!length(a_i)) a_i <- 0
    # Trous (parcelle non couverte), debords (hors parcelle), chevauchements.
    (a_p - a_i) + (a_u - a_i) + (a_s - a_u)
  }, numeric(1))
  aires <- vapply(ids, function(i) {
    sum(as.numeric(sf::st_area(p[as.character(p$idu) == i, ])))
  }, numeric(1))
  data.frame(idu = ids, ecart_m2 = ecart,
             ok = ecart <= pmax(1, 0.0005 * aires), stringsAsFactors = FALSE)
}

# Fichier d'un appel : chemin normalise et md5 (NA si absent), pour le lien
# du jeton. Le meme chemin au contenu change ne confirme pas.
.mcp_lien_fichier <- function(fichier) {
  if (!is.character(fichier) || length(fichier) != 1L || is.na(fichier)) {
    return(list(chemin = NA_character_, md5 = NA_character_))
  }
  list(chemin = normalizePath(fichier, mustWork = FALSE),
       md5 = if (file.exists(fichier)) unname(tools::md5sum(fichier)) else NA_character_)
}

#' Persist a re-tiled project and say what it invalidated
#' @noRd
.mcp_sauver_ugf <- function(id, projet, avec_parcelles = FALSE) {
  if (isTRUE(avec_parcelles)) save_parcels(id, projet$parcels)
  # Les ug_id sont neufs : les indicateurs calcules portent sur les anciens.
  # save_ug_data() les met deja de cote quand l'affectation change ; un second
  # invalidate_indicators() ne trouverait plus rien et repondrait FALSE.
  ok <- save_ug_data(id, projet)
  isTRUE(attr(ok, "indicateurs_invalides")) ||
    isTRUE(invalidate_indicators(id, motif = "ugf"))
}

#' Apply management units from a file
#'
#' Replaces the tenement layout of a project with the one of a GeoPackage or
#' GeoJSON file of tenements - typically the output of
#' `nemeton::construire_ugf_onf()` reworked outside the application.
#'
#' The file needs an IDU column (`idu`, `parent_parcelle_id` or `id`) and a
#' UGF label (`label_ugf`, `ugf` or `nom_ugf`); it may carry `onf_foret_id`,
#' `onf_foret_nom`, `onf_parcelle`, `onf_domaniale`, `onf_part` and `groupe`.
#'
#' Nothing is written unless every check passes: an IDU absent from the project
#' is an error (no cadastral parcel is created behind the user's back), and so
#' is a parcel whose tiling is not exact - the error names the parcels. With
#' `remplacer = TRUE` every parcel of the project must be in the file; with
#' `FALSE` only the parcels in the file are re-tiled, the others keep theirs.
#'
#' Two steps (ADR 0002): without `jeton`, every check runs and nothing is
#' written; the error `nemetonshiny_confirmation_requise` carries the preview
#' (`n_ugf_remplacees`, `n_ugf_nouvelles`) and a token. With the token, the
#' guards are replayed, the token is consumed, the file is read again and the
#' layout is written. The token is bound to the project's parcels, the file
#' (path and md5) and `remplacer`.
#'
#' @param projet Project id or name.
#' @param fichier Path of the file.
#' @param remplacer Logical. Replace the whole layout (default) or only the
#'   parcels present in the file.
#' @param jeton Character or `NULL`. Token of the first call.
#' @return JSON: `ok`, `projet`, `n_ugf`, `n_tenements`, `ecart_pavage_m2`,
#'   `indicateurs_perimes`, `avertissements`.
#' @noRd
mcp_appliquer_ugf <- function(projet, fichier, remplacer = TRUE, jeton = NULL) {
  .mcp_call({
    id <- .mcp_resolve_project(projet)
    # Lien du jeton : evalue apres la garde (paresseux dans
    # .mcp_jeton_consommer()), avant la lecture du fichier.
    lien <- function() .mcp_empreinte(id, fichier = .mcp_lien_fichier(fichier),
                                      remplacer = isTRUE(remplacer))
    if (is.null(jeton)) {
      .mcp_garde_ecriture(id)
      lien_apercu <- lien()
    } else {
      .mcp_jeton_consommer(jeton, "appliquer_ugf", id, lien())
    }
    if (!is.character(fichier) || length(fichier) != 1L || !file.exists(fichier)) {
      .api_abort("Fichier introuvable : {.file {fichier}}.",
                 "nemetonshiny_fichier_invalide")
    }
    imp <- tryCatch(sf::st_read(fichier, quiet = TRUE), error = function(e) NULL)
    if (is.null(imp) || !inherits(imp, "sf") || nrow(imp) == 0L) {
      .api_abort("Fichier illisible ou vide : {.file {fichier}}.",
                 "nemetonshiny_fichier_invalide")
    }
    col_idu <- intersect(c("idu", "parent_parcelle_id", "id"), names(imp))[1]
    col_lab <- intersect(c("label_ugf", "ugf", "nom_ugf"), names(imp))[1]
    if (is.na(col_idu) || is.na(col_lab)) {
      .api_abort(c("Colonnes manquantes dans {.file {fichier}}.",
                   i = "Il faut un IDU (idu, parent_parcelle_id ou id) et un libell\u00e9 d'UGF (label_ugf, ugf ou nom_ugf)."),
                 "nemetonshiny_fichier_invalide")
    }
    imp$idu <- as.character(imp[[col_idu]])
    imp$label_ugf <- trimws(as.character(imp[[col_lab]]))
    if (any(is.na(imp$label_ugf) | !nzchar(imp$label_ugf))) {
      .api_abort("Des t\u00e8nements n'ont pas de libell\u00e9 d'UGF.",
                 "nemetonshiny_fichier_invalide")
    }
    if (is.na(sf::st_crs(imp))) sf::st_crs(imp) <- .tenement_guess_crs(imp)
    # Colonne de geometrie au nom fixe (`geom` dans un GeoPackage) : les
    # tenements repris plus bas lui sont empiles.
    if (!identical(attr(imp, "sf_column"), "geometry")) sf::st_geometry(imp) <- "geometry"

    p <- .mcp_projet_ugf(id)
    id_col <- intersect(c("id", "nemeton_id", "geo_parcelle"), names(p$parcels))[1]
    parcels <- p$parcels
    parcels$idu <- as.character(parcels[[id_col]])

    inconnus <- setdiff(unique(imp$idu), parcels$idu)
    if (length(inconnus)) {
      .api_abort(c("{length(inconnus)} IDU absent{?s} du projet : {.val {utils::head(inconnus, 10)}}.",
                   i = "Aucune parcelle n'est cr\u00e9\u00e9e ici ; le projet n'a pas \u00e9t\u00e9 modifi\u00e9."),
                 "nemetonshiny_idu_inconnu", idu = inconnus)
    }
    avert <- character(0)
    idu_fichier <- unique(imp$idu)
    n_nouvelles <- length(unique(imp$label_ugf))
    if (isTRUE(remplacer)) {
      manquantes <- setdiff(parcels$idu, imp$idu)
      if (length(manquantes)) {
        .api_abort(c("{length(manquantes)} parcelle{?s} du projet sans t\u00e8nement dans le fichier : {.val {utils::head(manquantes, 10)}}.",
                     i = "Les ajouter au fichier, ou appeler avec remplacer = FALSE ; le projet n'a pas \u00e9t\u00e9 modifi\u00e9."),
                   "nemetonshiny_pavage_invalide", parcelles = manquantes)
      }
    }
    pav <- .mcp_ecart_pavage(parcels, imp)
    if (!all(pav$ok)) {
      mauvaises <- pav$idu[!pav$ok]
      .api_abort(c("Pavage inexact pour {length(mauvaises)} parcelle{?s} : {.val {utils::head(mauvaises, 10)}}.",
                   i = "\u00c9cart total {round(sum(pav$ecart_m2[!pav$ok]), 1)} m\u00b2 ; le projet n'a pas \u00e9t\u00e9 modifi\u00e9."),
                   "nemetonshiny_pavage_invalide", parcelles = mauvaises)
    }

    # Parcelles hors du fichier (remplacer = FALSE) : leurs tenements sont
    # repris tels quels, sans libelle, donc rattaches a leur UGF actuelle par
    # recouvrement.
    if (!isTRUE(remplacer)) {
      garder <- p$tenements[!as.character(p$tenements$parent_parcelle_id) %in% imp$idu, ]
      if (nrow(garder)) {
        g <- sf::st_transform(garder, sf::st_crs(imp))
        g <- sf::st_sf(idu = as.character(g$parent_parcelle_id),
                       label_ugf = NA_character_, geometry = sf::st_geometry(g))
        commun <- intersect(names(imp), names(g))
        imp <- rbind(imp[, commun], g[, commun])
      }
    }

    p2 <- withCallingHandlers(
      tenement_import_replace(p, imp),
      warning = function(w) {
        avert <<- c(avert, conditionMessage(w))
        invokeRestart("muffleWarning")
      })
    # Groupe d'amenagement du fichier, s'il y en a un.
    if ("groupe" %in% names(imp)) {
      grp <- tapply(as.character(imp$groupe), imp$label_ugf, function(x) {
        x <- unique(stats::na.omit(x)); if (length(x) == 1L) x else NA_character_
      })
      k <- match(p2$ugs$label, names(grp))
      p2$ugs$groupe[!is.na(k)] <- grp[k[!is.na(k)]]
    }
    if (is.null(jeton)) {
      # Remplacement complet : toutes les UGF actuelles ; partiel : celles des
      # parcelles du fichier (arbitrage de Pascal, 2026-11-10).
      n_remplacees <- if (isTRUE(remplacer)) nrow(p$ugs) else {
        ten <- p$tenements
        length(unique(as.character(
          ten$ug_id[as.character(ten$parent_parcelle_id) %in% idu_fichier])))
      }
      .mcp_jeton_emettre("appliquer_ugf", id, lien_apercu, list(
        n_ugf_remplacees = n_remplacees, n_ugf_nouvelles = n_nouvelles))
    }
    perimes <- .mcp_sauver_ugf(id, p2)
    list(projet = id, n_ugf = nrow(p2$ugs), n_tenements = nrow(p2$tenements),
         ecart_pavage_m2 = round(sum(pav$ecart_m2), 3),
         indicateurs_perimes = perimes,
         avertissements = if (length(avert)) avert)
  })
}

#' Cross a project with the ONF forest parcels
#'
#' Same crossing as the application's button (`nemeton::construire_ugf_onf()`,
#' with the project's ONF settings), applied to the project. About 15 s per
#' commune; the first call downloads the DGFiP file (376 MB, once).
#'
#' Two steps (ADR 0002): without `jeton`, the crossing runs and nothing is
#' written; the error `nemetonshiny_confirmation_requise` carries the preview
#' (`ecartees`, the IDU leaving the project, `n_ecartees`, `n_retenues`) and a
#' token. With the token, the guards are replayed, the token is consumed and
#' the crossing is computed again, then written. The token is bound to the
#' project's parcels and to every resolved ONF setting (`purger` included).
#'
#' @param projet Project id or name.
#' @param purger Logical or `NULL`. `TRUE` keeps only the parcels under the
#'   forest regime (the others leave the project), `FALSE` keeps them all,
#'   `NULL` follows the project's setting.
#' @param jeton Character or `NULL`. Token of the first call.
#' @return JSON: `ok`, `projet`, `n_ugf`, `n_tenements`, `n_parcelles`,
#'   `n_cad`, `ecartees`, `ecart_median_m`, `indicateurs_perimes`.
#' @noRd
mcp_croiser_onf <- function(projet, purger = NULL, jeton = NULL) {
  .mcp_call({
    id <- .mcp_resolve_project(projet)
    if (is.null(jeton)) .mcp_garde_ecriture(id)
    # Reglages ONF resolus : ceux du projet, `purger` explicite l'emportant.
    cfg <- project_onf_params(load_project_metadata(id) %||% list())
    if (!is.null(purger)) cfg$purger <- isTRUE(purger)
    if (!is.null(jeton)) {
      .mcp_jeton_consommer(jeton, "croiser_onf", id, .mcp_empreinte(id, onf = cfg))
    }
    p <- .mcp_projet_ugf(id)
    out <- onf_croise_tache(p, cfg)
    if (!identical(out$status, "ok")) {
      msg <- switch(out$status,
        unavailable = "Parcellaire ONF ou fichier DGFiP injoignable.",
        empty       = "Aucune for\u00eat publique sur l'emprise du projet.",
        no_overlap  = "Aucun recoupement entre le parcellaire ONF et les parcelles du projet.",
        "Croisement ONF impossible.")
      .api_abort(c(msg, i = "Le projet n'a pas \u00e9t\u00e9 modifi\u00e9."),
                 "nemetonshiny_onf_echec", statut = out$status)
    }
    ecartees <- out$ecartees %||% .onf_ecartees_vide()
    if (is.null(jeton)) {
      # purger resolu FALSE : rien n'est ecarte (arbitrage du 2026-11-10).
      idu <- if (isTRUE(cfg$purger)) as.character(ecartees$idu) else character(0)
      .mcp_jeton_emettre("croiser_onf", id, .mcp_empreinte(id, onf = cfg), list(
        ecartees = I(idu), n_ecartees = length(idu),
        n_retenues = nrow(p$parcels) - length(idu)))
    }
    perimes <- .mcp_sauver_ugf(id, out$projet, avec_parcelles = nrow(ecartees) > 0L)
    r <- onf_croise_resume(out$tenements)
    list(projet = id, n_ugf = r$n_ugf, n_tenements = nrow(out$projet$tenements),
         n_parcelles = r$n_parcelles, n_cad = r$n_cad,
         ecartees = if (nrow(ecartees)) ecartees,
         ecart_median_m = suppressWarnings(as.numeric(out$calage[["ecart_median_m"]])),
         indicateurs_perimes = perimes)
  })
}

# ---------------------------------------------------------------- exports

#' @noRd
mcp_generer_rapport <- function(projet, langue = "fr", synthese = NULL) {
  .mcp_call({
    id <- .mcp_resolve_project(projet)
    langue <- langue %||% "fr"
    f <- file.path(get_project_path(id), "exports",
                   sprintf("rapport_%s_%s.pdf", langue, format(Sys.time(), "%Y%m%d-%H%M%S")))
    list(projet = id, fichier = projet_rapport(id, f, langue = langue, synthese = synthese))
  }, langue = langue)
}

#' @noRd
mcp_exporter_gpkg <- function(projet) {
  .mcp_call({
    id <- .mcp_resolve_project(projet)
    f <- file.path(get_project_path(id), "exports",
                   sprintf("resultats_%s.gpkg", format(Sys.time(), "%Y%m%d-%H%M%S")))
    list(projet = id, fichier = projet_gpkg(id, f))
  })
}

#' @noRd
mcp_url_app <- function(projet, onglet = "synthesis") {
  .mcp_call({
    id <- .mcp_resolve_project(projet)
    onglet <- onglet %||% "synthesis"
    if (!onglet %in% DEEP_LINK_TABS) {
      cli::cli_abort("Onglet inconnu : {.val {onglet}}. Onglets : {.val {DEEP_LINK_TABS}}.")
    }
    port <- Sys.getenv("NEMETON_APP_PORT", "3838")
    list(projet = id,
         url = sprintf("http://127.0.0.1:%s/?project=%s&tab=%s", port,
                       utils::URLencode(id, reserved = TRUE), onglet))
  })
}

# ---------------------------------------------------------------- declaration

#' MCP tools exposed by nemetonshiny
#'
#' @return A list of `ellmer::tool()` definitions, for
#'   `mcptools::mcp_server(tools = )` (see `inst/mcp/server.R`).
#' Annotations: read_only_hint TRUE for the 4 read tools; destructive_hint TRUE
#' for appliquer_ugf and croiser_onf only; idempotent_hint FALSE for
#' lancer_calcul only (arbitrated by Pascal, 2026-11-10).
#' @noRd
mcp_tools <- function() {
  t <- ellmer::type_string
  projet_arg <- t("Project id, or its name (case and accents ignored).")
  jeton_arg <- t("Confirmation token. Omit it first: the call writes nothing and returns the error confirmation_requise with a preview and a token. Read the preview to the user; only if they confirm, call again with the same arguments and this token (single use, 10 minutes).", required = FALSE)
  list(
    ellmer::tool(.mcp_marquer(mcp_lister_projets), name = "lister_projets",
      annotations = ellmer::tool_annotations(read_only_hint = TRUE,
        destructive_hint = FALSE, idempotent_hint = TRUE),
      description = "List the Nemeton forest projects (id, name, status, last update, computed indicators, up to date).",
      arguments = list(limite = ellmer::type_integer("Maximum number of projects (default 20).", required = FALSE))),
    ellmer::tool(.mcp_marquer(mcp_resume_projet), name = "resume_projet",
      annotations = ellmer::tool_annotations(read_only_hint = TRUE,
        destructive_hint = FALSE, idempotent_hint = TRUE),
      description = "Synthesis of a project: global score /100, NDP level, confidence and the 12 family scores. Reads only, never modifies the project.",
      arguments = list(projet = projet_arg,
                       langue = t("'fr' or 'en' (default 'fr').", required = FALSE))),
    ellmer::tool(.mcp_marquer(mcp_lancer_calcul), name = "lancer_calcul",
      annotations = ellmer::tool_annotations(read_only_hint = FALSE,
        destructive_hint = FALSE, idempotent_hint = FALSE),
      description = "Start the full indicator computation of a project in the background (minutes to more than an hour). Returns at once; follow with etat_calcul.",
      arguments = list(projet = projet_arg)),
    ellmer::tool(.mcp_marquer(mcp_etat_calcul), name = "etat_calcul",
      annotations = ellmer::tool_annotations(read_only_hint = TRUE,
        destructive_hint = FALSE, idempotent_hint = TRUE),
      description = "State of a project's background computation: status (en_cours, termine, echec, annule, aucun), progress, current task, error and log tail on failure.",
      arguments = list(projet = projet_arg)),
    ellmer::tool(.mcp_marquer(mcp_annuler_calcul), name = "annuler_calcul",
      annotations = ellmer::tool_annotations(read_only_hint = FALSE,
        destructive_hint = FALSE, idempotent_hint = TRUE),
      description = "Ask a project's running computation to stop.",
      arguments = list(projet = projet_arg)),
    ellmer::tool(.mcp_marquer(mcp_generer_rapport), name = "generer_rapport",
      annotations = ellmer::tool_annotations(read_only_hint = FALSE,
        destructive_hint = FALSE, idempotent_hint = TRUE),
      description = "Write the PDF report of a computed project into its exports folder and return the file path.",
      arguments = list(projet = projet_arg,
                       langue = t("'fr' or 'en' (default 'fr').", required = FALSE),
                       synthese = t("Optional synthesis comment (Markdown).", required = FALSE))),
    ellmer::tool(.mcp_marquer(mcp_exporter_gpkg), name = "exporter_gpkg",
      annotations = ellmer::tool_annotations(read_only_hint = FALSE,
        destructive_hint = FALSE, idempotent_hint = TRUE),
      description = "Write the GeoPackage of a computed project's results (one feature per management unit) and return the file path.",
      arguments = list(projet = projet_arg)),
    ellmer::tool(.mcp_marquer(mcp_appliquer_ugf), name = "appliquer_ugf",
      annotations = ellmer::tool_annotations(read_only_hint = FALSE,
        destructive_hint = TRUE, idempotent_hint = TRUE),
      description = "Replace the management units (UGF) of a project with those of a GeoPackage/GeoJSON file of tenements (columns idu and label_ugf, optional onf_foret_id, onf_parcelle...). Nothing is written if an IDU is unknown or a parcel is not exactly tiled. Two steps: the first call writes nothing and returns a preview and a token; the write needs the token. Computed indicators become outdated.",
      arguments = list(projet = projet_arg,
                       fichier = t("Path of the GeoPackage or GeoJSON file."),
                       remplacer = ellmer::type_boolean("TRUE (default): the file covers every parcel of the project. FALSE: only the parcels in the file are re-tiled.", required = FALSE),
                       jeton = jeton_arg)),
    ellmer::tool(.mcp_marquer(mcp_croiser_onf), name = "croiser_onf",
      annotations = ellmer::tool_annotations(read_only_hint = FALSE,
        destructive_hint = TRUE, idempotent_hint = TRUE),
      description = "Build the UGF of a project from the ONF forest parcels warped onto the cadastre, each UGF carrying its ONF forest and parcel number. About 15 s per commune. Two steps: the first call writes nothing and returns a preview (parcels leaving the project) and a token; the write needs the token. Computed indicators become outdated.",
      arguments = list(projet = projet_arg,
                       purger = ellmer::type_boolean("TRUE: drop the parcels outside the forest regime (private, or little covered by the ONF). FALSE: keep them all. Default: the project's setting.", required = FALSE),
                       jeton = jeton_arg)),
    ellmer::tool(.mcp_marquer(mcp_url_app), name = "url_app",
      annotations = ellmer::tool_annotations(read_only_hint = TRUE,
        destructive_hint = FALSE, idempotent_hint = TRUE),
      description = "URL opening the Nemeton application on a project and a tab (synthesis, selection, action_plan, terrain, monitoring, regeneration, famille_*).",
      arguments = list(projet = projet_arg,
                       onglet = t("Tab (default 'synthesis').", required = FALSE)))
  )
}

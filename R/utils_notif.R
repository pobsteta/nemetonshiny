# Notifications de progression persistantes (bas-droite) - cadre unifie.
#
# Meme cadre / police / picto (roue dentee animee) / chronometre qui defile
# pour les moteurs FAST / FORDEAD / RECONFORT (mod_monitoring) et reGeneration
# (mod_regeneration). Ce fichier centralise le rendu du contenu de la notif ;
# chaque module reste responsable du cycle de vie (showNotification / id /
# removeNotification) et du tick (observe + invalidateLater(1000)).

# Duree ecoulee depuis `start` en "MM:SS" (ou "H:MM:SS" au-dela d'une heure).
# "" si NULL. Partage par tous les chronos de notif async.
# `now` injectable (v0.143.17) : `as.integer()` TRONQUE, donc l'ecart entre la
# pose de `start` et l'appel doit rester sous la seconde pour qu'un test tombe
# juste. Sous charge il ne l'est pas, et l'assertion cassait sans qu'aucun code
# de production ne soit en cause (test-mod_monitoring.R:1604, suite du
# 2026-09-04). Injecter l'horloge rend le formateur deterministe a l'appel ;
# `Sys.time()` par defaut laisse la production strictement inchangee.
.fmt_elapsed <- function(start, now = Sys.time()) {
  if (is.null(start)) return("")
  s <- as.integer(difftime(now, start, units = "secs"))
  if (s < 0L) s <- 0L
  if (s >= 3600L) {
    sprintf("%d:%02d:%02d", s %/% 3600L, (s %% 3600L) %/% 60L, s %% 60L)
  } else {
    sprintf("%02d:%02d", s %/% 60L, s %% 60L)
  }
}

# Contenu unifie d'une notif " en cours " : picto roue dentee anime (classe CSS
# `nmt-spin`, rotation 360deg) + libelle + (si `start` fourni) chronometre
# monospace " - MM:SS " qui defile. `label` peut etre un texte ou des tags
# htmltools. Rendu identique partout (cadre / police / picto) - moteur de calcul
# monitoring FAST/FORDEAD/RECONFORT, moteur reGeneration et analyse
# Accessibilite.
#
# Roue dentee plutot que sablier : `nmt-spin` est une rotation continue, qui
# convient a un engrenage alors qu'un sablier devrait se retourner.
.running_notif_content <- function(label, start = NULL) {
  chrono <- if (!is.null(start)) {
    htmltools::tagList(
      " \u2014 ",
      htmltools::tags$span(class = "font-monospace", .fmt_elapsed(start)))
  }
  htmltools::tagList(
    htmltools::tags$span(
      class = "nmt-spin me-2 text-secondary",
      style = "display:inline-block;vertical-align:middle;",
      `aria-hidden` = "true",
      bsicons::bs_icon("gear-fill")
    ),
    htmltools::tags$span(
      style = "vertical-align:middle;",
      label,
      chrono
    )
  )
}

# ---------------------------------------------------------------------------
# Message d'erreur lisible (mission 090)
# ---------------------------------------------------------------------------
# Un calcul long en processus enfant plafonne remonte une erreur du genre
# `"run_x" failed in its capped child process (exit 1).` suivie du chemin du
# journal de l'enfant puis d'une pile d'appels (`-> 15. +-nemeton:::...`), le
# tout entoure de codes de terminal. Brut, c'est illisible et la cause racine
# (ex. delai depasse vers un hote) n'y figure pas : elle n'est que dans le
# journal. Le message COMPLET reste en console (voir `.monitoring_run_error_msg`).

# Fin d'un fichier texte, lecture seule, bornee a `max_bytes` octets ; "" si
# absent ou illisible.
.lire_fin_journal <- function(path, max_bytes = 16384L) {
  if (!is.character(path) || length(path) != 1L || is.na(path) ||
      !nzchar(path) || !file.exists(path)) return(character())
  tryCatch({
    taille <- file.size(path)
    if (is.na(taille) || taille <= 0) return(character())
    con <- file(path, open = "rb")
    on.exit(close(con), add = TRUE)
    if (taille > max_bytes) seek(con, taille - max_bytes)
    brut <- readBin(con, what = "raw", n = max_bytes)
    brut <- brut[brut != as.raw(0L)]
    txt <- iconv(rawToChar(brut), from = "UTF-8", to = "UTF-8", sub = "")
    if (is.na(txt)) return(character())
    strsplit(txt, "\r?\n")[[1]]
  }, error = function(e) character())
}

# Cause la plus profonde d'un journal : la derniere ligne `Caused by ...`
# (avec la ligne precedente et les deux suivantes), a defaut la derniere
# ligne d'erreur (`x ...` / `Error ...`). "" si rien.
.cause_racine_journal <- function(lignes) {
  lignes <- trimws(cli::ansi_strip(lignes))
  lignes <- lignes[nzchar(lignes)]
  if (!length(lignes)) return("")
  # Une ligne de pile d'appels n'est jamais une cause.
  lignes <- lignes[!grepl("^(\u2192|->)?\\s*[0-9]+\\.\\s", lignes)]
  lignes <- lignes[!grepl("^(Execution halted|Backtrace:)", lignes)]
  idx <- grep("Caused by", lignes, fixed = TRUE)
  if (length(idx)) {
    i <- max(idx)
    bloc <- lignes[max(1L, i - 1L):min(length(lignes), i + 2L)]
  } else {
    idx <- grep("^(\u2716|x |Error)", lignes)
    if (!length(idx)) return("")
    bloc <- lignes[max(idx)]
  }
  trimws(gsub("\\s+", " ", paste(bloc, collapse = " ")))
}

#' Message d'erreur d'un calcul long, en texte lisible
#'
#' Retire les codes de terminal et la pile d'appels, garde l'en-tete, ajoute la
#' cause racine (lue dans le message, a defaut dans la fin du journal de
#' l'enfant) et le chemin du journal. Un message simple reste inchange.
#' @param msg Message d'erreur brut.
#' @param i18n Objet i18n (libelle \u00ab Journal \u00bb); sinon libelle neutre.
#' @param max_chars Longueur maximale du resultat.
#' @noRd
.message_erreur_lisible <- function(msg, i18n = NULL, max_chars = 700L) {
  brut <- paste(as.character(msg %||% ""), collapse = "\n")
  if (!nzchar(brut)) return("")
  txt <- cli::ansi_strip(brut)
  # Chemin du journal de l'enfant (le message le replie sur deux lignes).
  m <- regmatches(txt, regexec("kept in\\s+'([^']+)'", txt))[[1]]
  journal <- if (length(m) == 2L) m[2] else ""
  lignes <- strsplit(txt, "\r?\n")[[1]]
  # Pile d'appels : tout depuis `Backtrace:`, et les lignes `-> N. ...`.
  bt <- grep("^\\s*Backtrace:", lignes)
  if (length(bt)) lignes <- lignes[seq_len(min(bt) - 1L)]
  pile <- grepl("^\\s*(\u2192|->)", lignes) | grepl("^\\s*Execution halted", lignes)
  lignes <- lignes[!pile]
  # Mention du journal (ligne `i The child's output was kept in` + chemin).
  k <- grep("output was kept in", lignes, fixed = TRUE)
  if (length(k)) {
    sauf <- unique(c(k, k + 1L))
    sauf <- sauf[sauf <= length(lignes) &
                   (sauf %in% k | grepl("^\\s+'?/|^\\s+'?[A-Za-z]:", lignes[sauf]))]
    lignes <- lignes[-sauf]
  }
  lignes <- trimws(lignes, which = "right")
  while (length(lignes) && !nzchar(lignes[length(lignes)])) lignes <- lignes[-length(lignes)]
  corps <- paste(lignes, collapse = "\n")
  # Cause racine : dans le message sinon dans le journal.
  cause <- ""
  if (!grepl("Caused by", corps, fixed = TRUE)) {
    cause <- .cause_racine_journal(.lire_fin_journal(journal))
    if (nzchar(cause) && grepl(cause, corps, fixed = TRUE)) cause <- ""
  }
  if (nzchar(cause)) corps <- paste0(corps, "\n", cause)
  etiquette <- if (!is.null(i18n)) i18n$t("erreur_journal_prefixe") else "Log"
  if (nzchar(journal)) corps <- paste0(corps, "\n", etiquette, " ", journal)
  corps <- trimws(corps)
  if (nchar(corps) > max_chars) {
    # On coupe le corps, jamais la ligne du journal.
    reste <- if (nzchar(journal)) paste0("\n", etiquette, " ", journal) else ""
    tete <- substr(sub(paste0(gsub("([][{}()+*^$|\\\\?.])", "\\\\\\1", reste), "$"), "", corps),
                   1L, max(40L, max_chars - nchar(reste) - 1L))
    corps <- paste0(tete, "\u2026", reste)
  }
  corps
}

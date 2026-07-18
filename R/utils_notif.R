# Notifications de progression persistantes (bas-droite) — cadre unifié.
#
# Même cadre / police / picto (sablier animé) / chronomètre qui defile
# pour les moteurs FAST / FORDEAD / RECONFORT (mod_monitoring) et reGénération
# (mod_regeneration). Ce fichier centralise le rendu du contenu de la notif ;
# chaque module reste responsable du cycle de vie (showNotification / id /
# removeNotification) et du tick (observe + invalidateLater(1000)).

# Durée écoulée depuis `start` en "MM:SS" (ou "H:MM:SS" au-delà d'une heure).
# "" si NULL. Partagé par tous les chronos de notif async.
.fmt_elapsed <- function(start) {
  if (is.null(start)) return("")
  s <- as.integer(difftime(Sys.time(), start, units = "secs"))
  if (s < 0L) s <- 0L
  if (s >= 3600L) {
    sprintf("%d:%02d:%02d", s %/% 3600L, (s %% 3600L) %/% 60L, s %% 60L)
  } else {
    sprintf("%02d:%02d", s %/% 60L, s %% 60L)
  }
}

# Contenu unifié d'une notif « en cours » : picto sablier animé (classe CSS
# `nmt-spin`) + libellé + (si `start` fourni) chronomètre monospace « — MM:SS »
# qui défile. `label` peut être un texte ou des tags htmltools. Rendu identique
# partout (cadre / police / picto) — moteur de calcul monitoring
# FAST/FORDEAD/RECONFORT, moteur reGénération et analyse Accessibilité.
.running_notif_content <- function(label, start = NULL) {
  chrono <- if (!is.null(start)) {
    htmltools::tagList(
      " — ",
      htmltools::tags$span(class = "font-monospace", .fmt_elapsed(start)))
  }
  htmltools::tagList(
    htmltools::tags$span(
      class = "nmt-spin me-2 text-secondary",
      style = "display:inline-block;vertical-align:middle;",
      `aria-hidden` = "true",
      bsicons::bs_icon("hourglass-split")
    ),
    htmltools::tags$span(
      style = "vertical-align:middle;",
      label,
      chrono
    )
  )
}

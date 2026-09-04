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

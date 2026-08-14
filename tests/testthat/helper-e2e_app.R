# helper-e2e_app.R — démarrage d'AppDriver pour les smoke E2E
#
# Les trois smoke E2E (rag_admin, monitoring, validation-control-classes)
# démarraient l'app à l'identique, avec les deux mêmes fragilités. Elles sont
# traitées ici une fois pour toutes.
#
# 1. SANS BASE. Les trois tests déclarent en en-tête « No DB » — c'est leur
#    prémisse, et elle doit être IMPOSÉE, pas espérée. Sur un poste dont le
#    `.Renviron` pose `NEMETON_DB_URL`, le démarrage de l'app fait de vrais
#    travaux synchrones : résolution de config, init du schéma, migrations,
#    requêtes projets/zones. La boucle Shiny étant mono-thread, un `showModal()`
#    ou un changement d'onglet arrive après la fenêtre d'attente du test, qui
#    échoue sans rien apprendre du comportement visé (mesuré sur rag_admin :
#    même échec avec le code de `main`, PASS dès que la base est hors-jeu).
#    `NEMETON_DB_LOCAL=1` est l'interrupteur prévu par `.resolve_db_config()`
#    (retourne NULL) et couvre la project DB comme la base de suivi. Il n'est
#    pas déclaré dans `.Renviron`, donc la valeur posée ici survit dans le
#    sous-processus — là où un `NEMETON_DB_URL=""` serait écrasé au démarrage
#    de R.
#
# 2. DÉMARRAGE RÉESSAYÉ. `Page.navigate` expire au bout des 10 s codées en dur
#    dans le champ `default_timeout` de chromote — que `load_timeout` d'AppDriver
#    ne gouverne pas, et que `options(chromote.timeout=)` n'atteint pas (les deux
#    vérifiés sans effet). L'UI complète (tous les onglets, leaflet + plotly + DT)
#    dépasse parfois ce délai. Sans retry, le test se sautait une fois sur deux
#    et n'apportait donc aucun signal.

#' Boot the full app under shinytest2, hermetically and with retries
#'
#' @param name Snapshot/driver name, as passed to `AppDriver$new()`.
#' @param load_timeout,timeout Passed through to `AppDriver$new()`.
#' @param language App language option.
#' @param attempts Number of boot attempts before skipping the test.
#' @param envir Test frame owning the temporary environment variable.
#' @return A live `AppDriver`. Skips the calling test if every attempt failed.
#' @noRd
e2e_boot_app <- function(name,
                         load_timeout = 40 * 1000,
                         timeout      = 15 * 1000,
                         language     = "fr",
                         attempts     = 3L,
                         envir        = parent.frame()) {
  withr::local_envvar(c(NEMETON_DB_LOCAL = "1"), .local_envir = envir)

  app_object <- shiny::shinyApp(
    ui     = nemetonshiny:::app_ui,
    server = nemetonshiny:::app_server
  )

  derniere_erreur <- "aucune"
  for (essai in seq_len(attempts)) {
    res <- tryCatch(
      shinytest2::AppDriver$new(
        app_object,
        name         = name,
        load_timeout = load_timeout,
        timeout      = timeout,
        variant      = NULL,
        view         = FALSE,
        options      = list(nemeton.app_options = list(language = language))
      ),
      error = function(e) e
    )
    if (!inherits(res, "error")) return(res)
    derniere_erreur <- conditionMessage(res)
    # Un démarrage échoué laisse son navigateur en vie : sans ce nettoyage, la
    # tentative suivante s'exécute sur une machine que la précédente a chargée,
    # et le retry se sabote lui-même (mesuré).
    try(chromote::default_chromote_object()$close(), silent = TRUE)
  }

  testthat::skip(sprintf("AppDriver failed to boot after %d attempts: %s",
                         attempts, derniere_erreur))
}

#' TRUE when this machine can run a shinytest2 E2E at all
#'
#' A NON-snap Chrome is required: the chromium snap wedges on `Page.navigate`
#' under AppDriver (confined sandbox), whereas a google-chrome .deb boots the
#' app reliably.
#'
#' @return `TRUE` if some Chrome/Chromium binary is on the PATH.
#' @noRd
e2e_has_chrome <- function() {
  any(nzchar(Sys.which(c("google-chrome", "chromium",
                         "chromium-browser", "chrome"))))
}

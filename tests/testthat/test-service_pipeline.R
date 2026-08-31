# Tests for service_pipeline.R
# Machine a etats du lancement enchaine ("Tout calculer").

test_that("pipeline_new_run impose l'ordre du registre", {
  # L'appelant peut passer les ids dans n'importe quel ordre (cases a cocher
  # d'une modale) : c'est le registre qui decide de l'ordre d'execution, pas
  # l'ordre de selection - sinon l'IA pourrait tourner avant les indicateurs
  # qu'elle resume.
  etat <- nemetonshiny:::pipeline_new_run(
    c("ia_plan", "indicateurs", "desserte"))
  expect_equal(etat$steps, c("indicateurs", "desserte", "ia_plan"))
})

test_that("pipeline_new_run ignore les ids inconnus sans echouer", {
  etat <- nemetonshiny:::pipeline_new_run(c("indicateurs", "moteur_fantome"))
  expect_equal(etat$steps, "indicateurs")
})

test_that("un run parcourt ses etapes dans l'ordre", {
  etat <- nemetonshiny:::pipeline_new_run(c("indicateurs", "desserte"))
  expect_equal(nemetonshiny:::pipeline_current_step(etat), "indicateurs")

  etat <- nemetonshiny:::pipeline_record(etat, "indicateurs", "ok")
  expect_equal(nemetonshiny:::pipeline_current_step(etat), "desserte")

  etat <- nemetonshiny:::pipeline_record(etat, "desserte", "ok")
  expect_null(nemetonshiny:::pipeline_current_step(etat))
  expect_true(nemetonshiny:::pipeline_is_done(etat))
  expect_false(is.null(etat$ended))
})

test_that("une etape en echec n'interrompt pas la chaine", {
  # Decision produit : on continue et on rapporte a la fin. Un run de plusieurs
  # heures ne doit pas s'arreter au bout de dix minutes sur un moteur
  # secondaire.
  etat <- nemetonshiny:::pipeline_new_run(
    c("indicateurs", "desserte", "ia_plan"))
  etat <- nemetonshiny:::pipeline_record(etat, "indicateurs", "error", "boum")
  expect_equal(nemetonshiny:::pipeline_current_step(etat), "desserte")

  etat <- nemetonshiny:::pipeline_record(etat, "desserte", "ok")
  expect_equal(nemetonshiny:::pipeline_current_step(etat), "ia_plan")

  rep <- nemetonshiny:::pipeline_report(etat)
  expect_equal(rep$status[rep$step_id == "indicateurs"], "error")
  expect_equal(rep$message[rep$step_id == "indicateurs"], "boum")
})

test_that("une reponse tardive ne rejoue pas et ne decale pas le curseur", {
  # Un module qui repond deux fois (retry, resultat rejoue apres reconnexion)
  # ecraserait sinon le resultat de l'etape SUIVANTE et ferait sauter une
  # etape - le run se terminerait en silence avec une etape jamais lancee.
  etat <- nemetonshiny:::pipeline_new_run(
    c("indicateurs", "desserte", "ia_plan"))
  etat <- nemetonshiny:::pipeline_record(etat, "indicateurs", "ok")
  etat <- nemetonshiny:::pipeline_record(etat, "indicateurs", "error", "tardif")

  expect_equal(nemetonshiny:::pipeline_current_step(etat), "desserte")
  rep <- nemetonshiny:::pipeline_report(etat)
  expect_equal(rep$status[rep$step_id == "indicateurs"], "ok")
  expect_true(is.na(rep$message[rep$step_id == "indicateurs"]))
})

test_that("une reponse hors du run est ignoree", {
  etat <- nemetonshiny:::pipeline_new_run("indicateurs")
  avant <- etat
  etat <- nemetonshiny:::pipeline_record(etat, "sante_fordead", "ok")
  expect_equal(etat$index, avant$index)
  expect_equal(nemetonshiny:::pipeline_current_step(etat), "indicateurs")
})

test_that("skipped se distingue de error dans le decompte", {
  # « 3 sautees faute de configuration » ne doit pas se lire « 3 en echec » :
  # c'est toute la difference entre un run a corriger et un run nominal sur un
  # projet qui n'a pas de zone monitoring.
  etat <- nemetonshiny:::pipeline_new_run(
    c("indicateurs", "sante_fast", "sante_fordead"))
  etat <- nemetonshiny:::pipeline_record(etat, "indicateurs", "ok")
  etat <- nemetonshiny:::pipeline_record(etat, "sante_fast", "skipped", "pas de zone")
  etat <- nemetonshiny:::pipeline_record(etat, "sante_fordead", "error", "boum")

  compte <- nemetonshiny:::pipeline_tally(etat)
  expect_equal(unname(compte[["ok"]]), 1L)
  expect_equal(unname(compte[["skipped"]]), 1L)
  expect_equal(unname(compte[["error"]]), 1L)
})

test_that("un statut inconnu est traite comme une erreur, jamais accepte tel quel", {
  etat <- nemetonshiny:::pipeline_new_run("indicateurs")
  etat <- nemetonshiny:::pipeline_record(etat, "indicateurs", "n_importe_quoi")
  expect_equal(nemetonshiny:::pipeline_report(etat)$status, "error")
})

test_that("l'annulation tranche tout ce qui n'a pas encore repondu", {
  etat <- nemetonshiny:::pipeline_new_run(
    c("indicateurs", "desserte", "ia_plan"))
  etat <- nemetonshiny:::pipeline_record(etat, "indicateurs", "ok")
  etat <- nemetonshiny:::pipeline_mark_running(etat)
  etat <- nemetonshiny:::pipeline_cancel(etat)

  expect_true(nemetonshiny:::pipeline_is_done(etat))
  rep <- nemetonshiny:::pipeline_report(etat)
  expect_equal(rep$status[rep$step_id == "indicateurs"], "ok")
  expect_equal(rep$status[rep$step_id == "desserte"], "cancelled")
  expect_equal(rep$status[rep$step_id == "ia_plan"], "cancelled")
})

test_that("le rapport porte une ligne par etape, dans l'ordre, avec sa duree", {
  etat <- nemetonshiny:::pipeline_new_run(c("indicateurs", "desserte"))
  etat <- nemetonshiny:::pipeline_mark_running(etat)
  etat <- nemetonshiny:::pipeline_record(etat, "indicateurs", "ok")

  rep <- nemetonshiny:::pipeline_report(etat)
  expect_equal(nrow(rep), 2L)
  expect_equal(rep$step_id, c("indicateurs", "desserte"))
  expect_equal(rep$label[1], "pipeline_step_indicateurs")
  expect_false(is.na(rep$seconds[1]))
  expect_equal(rep$status[2], "pending")
})

test_that("chaque etape du registre porte une cle i18n qui existe", {
  # Une etape sans traduction s'afficherait sous sa cle brute dans le panneau
  # de progression - le genre de defaut qu'on ne voit qu'en production.
  for (s in nemetonshiny:::PIPELINE_STEPS) {
    expect_true(s$label %in% names(nemetonshiny:::TRANSLATIONS),
                info = sprintf("cle i18n absente : %s", s$label))
  }
})

test_that("les ids du registre sont uniques", {
  ids <- nemetonshiny:::pipeline_all_step_ids()
  expect_equal(length(ids), length(unique(ids)))
})

test_that("les prealables reGeneration precedent le moteur dans le registre", {
  # Regression signalee 2026-08-28 : la chaine ne lancait que `engine_task`.
  # Or `eobs_task` DETERMINE les annees moyenne / canicule et les pousse dans
  # les champs que le moteur lit ensuite - lance seul, le moteur tournait sur
  # les valeurs par defaut codees en dur (2018 / 2022), sans que rien ne le
  # signale. L'ordre du registre est donc une contrainte de CORRECTION des
  # resultats, pas de confort de lecture.
  ids <- nemetonshiny:::pipeline_all_step_ids()
  rang <- function(x) which(ids == x)

  expect_true(rang("regen_annees") < rang("regeneration"))
  expect_true(rang("regen_gel") < rang("regeneration"))
  # Les deux fetch E-OBS sont bornes par les annees detectees.
  expect_true(rang("regen_annees") < rang("regen_eobs_rr"))
  expect_true(rang("regen_annees") < rang("regen_eobs_tg"))
})

test_that("l'ingest sante precede les moteurs qui lisent son cache", {
  # FORDEAD et RECONFORT recoivent tous deux `cache_dir = .resolve_s2_cache_dir()`,
  # que seul l'ingest de la surveillance rapide remplit.
  ids <- nemetonshiny:::pipeline_all_step_ids()
  rang <- function(x) which(ids == x)
  expect_true(rang("sante_fast") < rang("sante_fordead"))
  expect_true(rang("sante_fast") < rang("sante_reconfort"))
})

test_that("les indicateurs precedent tout, et les IA ferment la marche", {
  # Les 31 indicateurs alimentent chaque vue ; la perspective IA les resume, et
  # le plan d'actions se construit sur les commentaires qu'elle vient d'ecrire
  # (`plan_llm_context()`).
  ids <- nemetonshiny:::pipeline_all_step_ids()
  expect_equal(ids[1], "indicateurs")
  expect_equal(utils::tail(ids, 2), c("ia_synthese", "ia_plan"))
})

test_that("chaque module cite par le registre repond bien a ses etapes", {
  # Filet contre l'oubli le plus couteux du dispositif : une etape declaree
  # dans le registre que PERSONNE n'ecoute bloque la chaine en silence. On
  # verifie que chaque id apparait dans un `pipeline_targets()` du module
  # annonce.
  fichiers <- c(home = "mod_home.R", accessibility = "mod_accessibility.R",
                desserte = "mod_desserte.R", regeneration = "mod_regeneration.R",
                monitoring = "mod_monitoring.R", synthesis = "mod_synthesis.R",
                action_plan = "mod_action_plan.R")
  for (etape in nemetonshiny:::PIPELINE_STEPS) {
    f <- chemin_source("R", fichiers[[etape$module]])
    skip_sans_sources(f)
    src <- paste(readLines(f, warn = FALSE), collapse = "\n")
    expect_true(
      grepl(sprintf('pipeline_targets(req, "%s")', etape$id), src, fixed = TRUE),
      info = sprintf("aucun ecouteur pour l'etape %s dans %s",
                     etape$id, fichiers[[etape$module]]))
  }
})

test_that("le typage de la desserte suit immediatement son moteur", {
  # `run_desserte_typage()` lit le cache que le moteur desserte vient de
  # remplir. Le placer ailleurs le ferait travailler sur le cache d'un run
  # precedent, ou sur rien.
  ids <- nemetonshiny:::pipeline_all_step_ids()
  expect_equal(ids[which(ids == "desserte") + 1L], "desserte_typage")
})

test_that("la correction LiDAR precede l'analyse d'accessibilite", {
  # L'analyse consomme le reseau corrige des qu'il existe sur disque. L'ordre
  # inverse produirait une analyse sur reseau brut, puis deux heures de
  # correction dont plus rien ne se servirait dans ce run.
  ids <- nemetonshiny:::pipeline_all_step_ids()
  expect_true(which(ids == "accessibilite_correction") < which(ids == "accessibilite"))
})

test_that("les controles desserte suivent leur moteur", {
  ids <- nemetonshiny:::pipeline_all_step_ids()
  rang <- function(x) which(ids == x)
  expect_true(rang("desserte") < rang("desserte_typage"))
  expect_true(rang("desserte") < rang("desserte_integrite"))
})

test_that("toute lecture d'une memoire de requete pipeline est isolee", {
  # Deux defauts distincts, un seul remede.
  #
  # 1. CONTEXTE. La reponse de l'etape « indicateurs » etait posee depuis
  #    `poll_fn`, un callback `later::later()` qui s'execute HORS contexte
  #    reactif. La lecture y levait « Operation not allowed without an active
  #    reactive context » ; l'erreur remontait, la reponse n'etait jamais posee,
  #    et la chaine restait sur « Indicateurs / En cours » indefiniment - alors
  #    meme que le calcul s'etait termine normalement (Couchey, 2026-08-29).
  #
  # 2. DEPENDANCE PARASITE. Ailleurs les lectures sont en contexte reactif,
  #    donc legales - mais elles abonnent l'observer de statut a la memoire de
  #    requete. Poser la requete le redeclenche alors, et s'il porte encore le
  #    statut « success » d'un run precedent, il repond AVANT que le moteur
  #    n'ait redemarre : l'etape serait rapportee reussie sans avoir tourne.
  #
  # Le test lit les SOURCES : ces defauts sont des contextes d'execution, que
  # ni un testServer ni un appel direct ne reproduisent.
  fichiers <- c("mod_home.R", "mod_accessibility.R", "mod_desserte.R",
                "mod_regeneration.R", "mod_monitoring.R", "mod_synthesis.R",
                "mod_action_plan.R")

  for (nom in fichiers) {
    f <- chemin_source("R", nom)
    skip_sans_sources(f)
    src <- readLines(f, warn = FALSE)
    lectures <- grep("pipeline_req\\(\\)|item\\$rv\\(\\)", src, value = TRUE)
    # Les declarations `xxx <- shiny::reactiveVal(NULL)` ne sont pas des lectures.
    lectures <- grep("reactiveVal", lectures, value = TRUE, invert = TRUE)
    nues <- grep("shiny::isolate\\(", lectures, value = TRUE, invert = TRUE)
    expect_length(nues, 0L)
  }
})

test_that("la creation des zones de suivi precede les moteurs sante", {
  # Premier run reel sur Couchey : les trois moteurs sante se sont sautes,
  # « Aucune zone de suivi enregistree ». Ils exigent tous un `zone_id` ; la
  # chaine cree donc les zones elle-meme, juste avant.
  ids <- nemetonshiny:::pipeline_all_step_ids()
  rang <- function(x) which(ids == x)
  expect_true(rang("sante_zone") < rang("sante_fast"))
  expect_true(rang("sante_zone") < rang("sante_fordead"))
  expect_true(rang("sante_zone") < rang("sante_reconfort"))
})

test_that("les moteurs sante nomment la zone manquante, pas un prerequis vague", {
  # « Le lancement a ete refuse par l'onglet (prerequis manquant) » n'apprend
  # rien : l'utilisateur ne peut pas savoir quoi corriger. Chaque moteur sante
  # doit citer la cle qui nomme la vraie cause.
  f <- chemin_source("R", "mod_monitoring.R")
  skip_sans_sources(f)
  src <- paste(readLines(f, warn = FALSE), collapse = "\n")
  expect_equal(
    length(gregexpr("pipeline_skip_no_zone", src, fixed = TRUE)[[1]]), 3L)
})

test_that("la generation IA impose le remplissage des 12 familles", {
  # Le switch « toutes les familles » de l'onglet est DECOCHE par defaut. Sans
  # forcage, l'etape ne generait que la synthese, alors que son libelle annonce
  # « synthese + 12 familles ».
  f <- chemin_source("R", "mod_synthesis.R")
  skip_sans_sources(f)
  src <- paste(readLines(f, warn = FALSE), collapse = "\n")
  expect_true(grepl("remplir_familles = TRUE", src, fixed = TRUE))
})

test_that("une perspective non generee ne peut pas etre rapportee reussie", {
  # Faux positif constate sur Couchey : etape « Perspective IA » verte en 1 s,
  # pour ce qui demande 13 appels LLM. L'appel avait echoue, `tryCatch` avait
  # rendu NULL, et la fonction continuait jusqu'a `invisible(TRUE)`.
  f <- chemin_source("R", "mod_synthesis.R")
  skip_sans_sources(f)
  src <- paste(readLines(f, warn = FALSE), collapse = "\n")
  expect_true(grepl("if (is.null(synthesis_response)) {", src, fixed = TRUE))
})

test_that("les moteurs sante resolvent leur zone en base, pas dans le menu", {
  # Run Couchey 2026-08-29 : les quatre zones venaient d'etre creees EN BASE,
  # et les trois moteurs se sautaient quand meme sur « Aucune zone de suivi
  # enregistree ». La garde interrogeait `input$zone_id`, un selectInput
  # alimente par `updateSelectInput()` - pas encore remonte du client. C'est la
  # TROISIEME occurrence du meme piege dans cette chaine (annees E-OBS,
  # `use_corrected`, zone de suivi).
  f <- chemin_source("R", "mod_monitoring.R")
  skip_sans_sources(f)
  src <- paste(readLines(f, warn = FALSE), collapse = "\n")
  # Chacune des trois etapes sante resout la zone via la lecture en base.
  expect_equal(
    length(gregexpr("zid_pipeline <- suppressWarnings(as.integer(fordead_zone_id()))",
                    src, fixed = TRUE)[[1]]), 3L)
  # ... et la transmet au moteur, qui la prefere a son menu.
  expect_equal(
    length(gregexpr("zone_id = zid_pipeline", src, fixed = TRUE)[[1]]), 3L)
})

test_that("aucune raison de saut ne transite par un <<- depuis un bloc tryCatch", {
  # Le bloc d'un `tryCatch` s'evalue dans le frame APPELANT : un `<<-` y saute
  # par-dessus l'observer pour chercher la variable dans le namespace du
  # paquet. La raison restait NULL et le rapport affichait « prerequis
  # manquant » au lieu de la vraie cause - ce qui a masque un echec Mistral
  # pendant tout un run (Couchey, 2026-08-29). Un `<<-` dans un HANDLER
  # `error = function(e)` est licite : son enclos est bien le frame de la
  # fonction. Seuls les blocs sont vises ici.
  f <- chemin_source("R", "mod_synthesis.R")
  skip_sans_sources(f)
  src <- readLines(f, warn = FALSE)
  # `raison <<-` etait la formulation fautive ; elle ne doit plus exister.
  expect_length(grep("raison\\s*<<-", src), 0L)
})

test_that("pipeline_task_error extrait le message reel d'une tache en echec", {
  # `task$result()` RE-LEVE l'erreur du worker : c'est le seul endroit ou son
  # message existe encore. Sans cette extraction, le rapport n'affichait qu'un
  # « Erreur » nu - sur des moteurs qui tournent des heures (13 h 40 pour
  # l'ingest FAST, 4 h 10 pour FORDEAD sur Couchey), c'est la seule information
  # exploitable sans tout relancer.
  tache_ko <- list(result = function() stop("connexion refusee par le serveur S2"))
  expect_match(nemetonshiny:::pipeline_task_error(tache_ko),
               "connexion refusee", fixed = TRUE)
})

test_that("pipeline_task_error lit aussi un echec rendu PAR VALEUR", {
  # Plusieurs moteurs de l'app ne levent pas : ils rendent
  # `list(status = "error", reason = ..., detail = ...)`. Ne regarder que les
  # conditions raterait ces cas-la.
  avec_detail <- list(result = function() list(status = "error", detail = "raster illisible"))
  expect_equal(nemetonshiny:::pipeline_task_error(avec_detail), "raster illisible")

  avec_reason <- list(result = function() list(status = "error", reason = "desserte_engine_failed"))
  expect_equal(nemetonshiny:::pipeline_task_error(avec_reason), "desserte_engine_failed")
})

test_that("pipeline_task_error tronque les tracebacks et retombe sur le defaut", {
  # Un traceback Python (FORDEAD via reticulate) tiendrait sur des dizaines de
  # lignes et noierait le rapport.
  long <- paste(rep("ligne de traceback", 60), collapse = "\n")
  out <- nemetonshiny:::pipeline_task_error(list(result = function() stop(long)))
  expect_lte(nchar(out), 320L)
  expect_match(out, "\\[\\.\\.\\.\\]$")
  expect_false(grepl("\n", out, fixed = TRUE))

  # Rien d'extractible -> message de repli, jamais NULL ni NA.
  expect_equal(
    nemetonshiny:::pipeline_task_error(list(result = function() NULL), "Erreur"),
    "Erreur")
})

# Mission 099 : source Sentinel-2 du suivi sanitaire (choix par projet, appels du
# coeur, dossiers de lecture, "indicatif", repli, erreurs de cles THEIA).

.projet_s2 <- function(meta = list(), env = parent.frame()) {
  dir <- withr::local_tempdir(.local_envir = env)
  jsonlite::write_json(meta, file.path(dir, "metadata.json"), auto_unbox = TRUE)
  dir
}

.bd_gpkg_s2 <- function(env = parent.frame()) {
  gpkg <- file.path(withr::local_tempdir(.local_envir = env), "bd.gpkg")
  sf::st_write(sf::st_sf(code_tfv = "FF1G01-01",
                         geometry = sf::st_sfc(sf::st_point(c(0, 0)), crs = 2154)),
               gpkg, quiet = TRUE)
  gpkg
}

.render_html_s2 <- function(x) paste(as.character(x), collapse = " ")

# --- Choix du projet ----------------------------------------------------------

test_that("un projet sans reglage est 'par methode' et ne passe aucun argument", {
  expect_null(nemetonshiny:::project_source_s2(NULL))
  expect_null(nemetonshiny:::project_source_s2(list(name = "x")))
  expect_identical(nemetonshiny:::.source_s2_args(NULL), list())
  expect_identical(nemetonshiny:::.source_s2_vers_choix(NULL), "methode")
  # Une valeur inconnue ou 'methode' retombe sur le defaut.
  expect_null(nemetonshiny:::project_source_s2(list(source_s2 = "methode")))
  expect_null(nemetonshiny:::project_source_s2(list(source_s2 = "autre")))
  expect_identical(nemetonshiny:::.source_s2_args("theia"),
                   list(source_s2 = "theia"))
})

test_that("le choix se sauve, se relit, et 'par methode' retire la cle", {
  dir <- .projet_s2(list(name = "p"))
  testthat::local_mocked_bindings(get_project_path = function(id) dir)
  nemetonshiny:::set_project_source_s2("p1", "theia")
  meta <- jsonlite::read_json(file.path(dir, "metadata.json"))
  expect_identical(nemetonshiny:::project_source_s2(meta), "theia")
  nemetonshiny:::set_project_source_s2("p1", "methode")
  meta <- jsonlite::read_json(file.path(dir, "metadata.json"))
  expect_null(meta$source_s2)
  expect_identical(meta$name, "p")
})

test_that("le reglage propose trois valeurs, 'par methode' preselectionne", {
  shiny::testServer(nemetonshiny:::mod_sources_config_server, args = list(
    app_state = shiny::reactiveValues(
      language = "fr", project_id = "p1",
      current_project = list(id = "p1", metadata = list()))
  ), {
    session$setInputs(x = 1)
    html <- .render_html_s2(output$s2_block)
    i18n <- get_i18n("fr")
    for (k in c("s2_source_titre", "s2_choix_methode", "s2_choix_theia",
                "s2_choix_sen2cor", "s2_source_aide")) {
      expect_true(grepl(htmltools::htmlEscape(i18n$t(k)), html, fixed = TRUE), info = k)
    }
    expect_match(html, "value=\"methode\" selected")
  })
})

# --- Appels du coeur ----------------------------------------------------------

test_that("l'ICD garde exactement ses arguments 'par methode' et recoit source_s2 sinon", {
  rd <- withr::local_tempdir(); gpkg <- .bd_gpkg_s2()
  p <- nemetonshiny:::project_icd_poids(NULL)
  v <- nemetonshiny:::icd_poids_resume(p)$valide
  vu <- NULL
  testthat::local_mocked_bindings(
    run_icd_decoloration = function(...) {
      vu <<- list(...)
      list(status = "success", poids = v, annees = 2020:2021)
    }, .package = "nemeton")
  nemetonshiny:::.icd_enfant(NULL, 5, "c", rd, p, gpkg)
  expect_identical(
    names(vu), c("con", "zone_id", "cache_dir", "result_dir", "poids", "bdforet",
                 "sufosat_dates", "sufosat_proba", "progress_callback",
                 "cancel_path"))
  expect_false("source_s2" %in% names(vu))

  nemetonshiny:::.icd_enfant(NULL, 5, "c", rd, p, gpkg, source_s2 = "theia")
  expect_identical(vu$source_s2, "theia")
  # Le temoin de l'app suit le dossier effectif (`_theia`).
  expect_true(file.exists(file.path(paste0(rd, "_theia"), "zone_5", "app_icd.json")))
})

test_that(".icd_executer transmet source_s2 a l'enfant seulement s'il est choisi", {
  vu <- NULL
  testthat::local_mocked_bindings(
    .run_capped = function(...) { vu <<- list(...); list(status = "success") })
  base <- list(db_url = "db://x", zone_id = 5L, cache_dir = "c", result_dir = "r",
               poids = "P", bdforet_path = "bd.gpkg", plafonne = TRUE)
  do.call(nemetonshiny:::.icd_executer, base)
  expect_false("source_s2" %in% names(vu$args))
  do.call(nemetonshiny:::.icd_executer, c(base, list(source_s2 = "sen2cor")))
  expect_identical(vu$args$source_s2, "sen2cor")
})

test_that("FAST, FORDEAD et RECONFORT passent .source_s2_args() au coeur", {
  src <- chemin_source("R", "service_monitoring.R")
  skip_sans_sources(src)
  txt <- paste(readLines(src, warn = FALSE), collapse = "\n")
  expect_equal(lengths(regmatches(txt, gregexpr("\\.source_s2_args\\(source_s2\\)", txt))), 3L)
  inv <- chemin_source("R", "mod_monitoring.R")
  skip_sans_sources(inv)
  m <- paste(readLines(inv, warn = FALSE), collapse = "\n")
  expect_equal(lengths(regmatches(m, gregexpr("source_s2 += project_source_s2\\(", m))), 4L)
})

# --- Dossiers de lecture ------------------------------------------------------

test_that("les dossiers de lecture ne changent pas sans choix", {
  d <- withr::local_tempdir()
  dir.create(paste0(d, "_theia")); dir.create(file.path(paste0(d, "_theia"), "zone_3"))
  expect_identical(nemetonshiny:::.source_s2_dossier_lecture(d, NULL, 3), d)
  expect_identical(nemetonshiny:::.source_s2_dossier_lecture(d, "sen2cor", 3), d)
  expect_identical(nemetonshiny:::.source_s2_dossier_lecture(d, "theia", 3),
                   paste0(d, "_theia"))
  # Run THEIA absent de la zone (repli sur Sen2Cor) : le dossier d'origine.
  expect_identical(nemetonshiny:::.source_s2_dossier_lecture(d, "theia", 4), d)
})

test_that("l'ICD est lu dans <result_dir>_theia avec THEIA, ailleurs inchange", {
  p <- withr::local_tempdir()
  rd <- file.path(p, "cache", "layers", "icd")
  dir.create(file.path(paste0(rd, "_theia"), "zone_7"), recursive = TRUE)
  expect_identical(nemetonshiny:::.source_s2_dossier_methode("icd", p, NULL, 7), rd)
  expect_identical(nemetonshiny:::.source_s2_dossier_methode("icd", p, "theia", 7),
                   paste0(rd, "_theia"))
  fd <- file.path(p, "cache", "layers", "fordead")
  dir.create(file.path(paste0(fd, "_theia"), "zone_7"), recursive = TRUE)
  expect_identical(nemetonshiny:::.source_s2_dossier_methode("fordead", p, NULL, 7), fd)
  expect_identical(nemetonshiny:::.source_s2_dossier_methode("fordead", p, "theia", 7),
                   paste0(fd, "_theia"))
})

# --- "indicatif" et repli -----------------------------------------------------

test_that("indicatif : THEIA sur FAST, FORDEAD et ICD, Sen2Cor sur RECONFORT seulement", {
  ind <- function(m, choix) nemetonshiny:::source_s2_info(m, NULL, choix)$indicatif
  for (m in c("fast", "fordead", "icd")) {
    expect_true(ind(m, "theia"), info = m)
    expect_false(ind(m, "sen2cor"), info = m)
    expect_false(ind(m, NULL), info = m)
  }
  expect_false(ind("reconfort", "theia"))
  expect_true(ind("reconfort", "sen2cor"))
  expect_false(ind("reconfort", NULL))
  i <- nemetonshiny:::source_s2_info("fast", NULL, "theia")
  expect_match(i$mention, "FAST")
})

test_that("RECONFORT sur la voie Sen2Cor est indicatif sans source_s2 (voie 095)", {
  meta <- list(source_reflectances = "sen2cor", mention = "indicative only")
  i <- nemetonshiny:::source_s2_info("reconfort", meta, NULL)
  expect_identical(i$source, "sen2cor")
  expect_true(i$indicatif)
  expect_identical(i$mention, "indicative only")
})

test_that("la ligne Source porte le badge d'avertissement et la mention en infobulle", {
  i18n <- get_i18n("fr")
  info <- nemetonshiny:::source_s2_info("icd", NULL, "theia")
  html <- .render_html_s2(nemetonshiny:::.source_s2_ligne_ui(info, i18n))
  expect_match(html, "Source : THEIA (MAJA)", fixed = TRUE)
  expect_match(html, "text-bg-warning", fixed = TRUE)
  expect_false(grepl("bg-ia", html, fixed = TRUE))
  expect_match(html, "title=", fixed = TRUE)
  expect_match(html, i18n$t("s2_indicatif"), fixed = TRUE)
  # Source de calage : pas de badge.
  ok <- .render_html_s2(nemetonshiny:::.source_s2_ligne_ui(
    nemetonshiny:::source_s2_info("icd", NULL, NULL), i18n))
  expect_false(grepl("badge", ok, fixed = TRUE))
})

test_that("un repli est lu dans le meta.json de l'ICD et la cause s'affiche", {
  p <- withr::local_tempdir()
  zd <- file.path(p, "cache", "layers", "icd", "zone_7")
  dir.create(zd, recursive = TRUE)
  jsonlite::write_json(
    list(source_s2 = "sen2cor", source_s2_demandee = "theia",
         calee_sur = "sen2cor", repli_sen2cor = TRUE,
         cause_repli = "stac.example: HTTP 503"),
    file.path(zd, "meta.json"), auto_unbox = TRUE)
  proj <- list(path = p, metadata = list(source_s2 = "theia"))
  info <- nemetonshiny:::.source_s2_info_projet("icd", proj, 7)
  expect_true(info$repli)
  expect_false(info$indicatif)
  html <- .render_html_s2(nemetonshiny:::.source_s2_ligne_ui(info, get_i18n("fr")))
  expect_match(html, "repli : stac.example: HTTP 503", fixed = TRUE)
  msgs <- nemetonshiny:::.source_s2_notifications("icd", info, get_i18n("fr"))
  expect_length(msgs, 1L)
  expect_match(msgs, "Sen2Cor")
})

test_that("RECONFORT : causes_repli donne la ligne repli", {
  info <- nemetonshiny:::source_s2_info(
    "reconfort", list(acquisition = "sen2cor", source_reflectances = "sen2cor",
                      causes_repli = c("THEIA: delai", "GEODES: absent")), NULL)
  expect_true(info$repli)
  expect_match(info$cause, "THEIA: delai ; GEODES: absent", fixed = TRUE)
})

# --- Erreurs et avertissements du coeur ---------------------------------------

test_that("nemeton_s2_theia_refuse parle des cles THEIA, sans pile d'appels", {
  i18n <- get_i18n("fr")
  e <- structure(class = c("nemeton_s2_theia_refuse", "rlang_error", "error", "condition"),
                 list(message = paste0(
                   "THEIA STAC catalogue (\"stac.teledetection.fr\") refused the request: HTTP 403.\n",
                   "Backtrace:\n  1. nemeton:::.s2_choisir_source()\n"),
                   call = NULL))
  txt <- nemetonshiny:::.monitoring_run_error_msg(conditionMessage(e), i18n, "monitoring_health_error")
  expect_match(txt, "THEIA", fixed = TRUE)
  expect_match(txt, "TLD_ACCESS_KEY", fixed = TRUE)
  expect_false(grepl("Backtrace|nemeton:::", txt))
  # Par la classe seule, et pour les deux autres erreurs du coeur.
  for (cl in c("nemeton_s2_theia_refuse", "nemeton_s2_theia_sans_cles",
               "nemeton_reconfort_stac_refuse")) {
    cnd <- structure(class = c(cl, "error", "condition"),
                     list(message = "x", call = NULL))
    expect_identical(nemetonshiny:::.source_s2_message_erreur(cnd, i18n),
                     i18n$t("s2_err_cles_theia"))
  }
  expect_null(nemetonshiny:::.source_s2_message_erreur("panne reseau", i18n))
  expect_match(nemetonshiny:::.source_s2_message_erreur(
    "`source_s2 = \"theia\"` needs THEIA credentials to sign the asset URLs.", i18n),
    "Sources & param", fixed = FALSE)
})

test_that("les quatre avertissements du coeur deviennent des textes lisibles", {
  i18n <- get_i18n("fr")
  mk <- function(cl, msg) structure(class = c(cl, "warning", "condition"),
                                    list(message = msg, call = NULL))
  w1 <- nemetonshiny:::.source_s2_message_avertissement(
    mk("nemeton_s2_repli_sen2cor", "THEIA unavailable (timeout): the whole FAST series switches"), i18n)
  expect_match(w1, "timeout", fixed = TRUE)
  expect_match(w1, "Sen2Cor", fixed = TRUE)
  for (cl in c("nemeton_s2_source_non_calee", "nemeton_reconfort_repli_archive",
               "nemeton_reconfort_repli_sen2cor")) {
    expect_type(nemetonshiny:::.source_s2_message_avertissement(mk(cl, "x (y)"), i18n),
                "character")
  }
  expect_null(nemetonshiny:::.source_s2_message_avertissement(
    mk("autre_classe", "x"), i18n))
})

test_that("les cles i18n de la mission 099 existent en FR et EN, 30 caracteres au plus", {
  cles <- grep("^s2_", names(TRANSLATIONS), value = TRUE)
  expect_gte(length(cles), 15L)
  expect_true(all(nchar(cles) <= 30L))
  for (k in cles) {
    expect_true(all(c("fr", "en") %in% names(TRANSLATIONS[[k]])), info = k)
  }
})

# Mission 067 : type de gestion declare par UGF + tableau "effet de la gestion"
# de la Synthese (nemeton::effet_gestion, simule : aucun calcul reel ici).

withr::local_options(nemetonshiny.effet_gestion_inline = TRUE,
                     .local_envir = testthat::teardown_env())

.eg_projet <- function() {
  p <- lapply(1:3, function(i) sf::st_polygon(list(matrix(c(
    i - 1, 0, i, 0, i, 1, i - 1, 1, i - 1, 0), ncol = 2, byrow = TRUE))))
  parcels <- sf::st_sf(
    id = c("p1", "p2", "p3"), geo_parcelle = c("R1", "R2", "R3"),
    contenance = c(10000, 10000, 10000),
    geometry = sf::st_sfc(p, crs = 4326))
  nemetonshiny:::ug_init_default(list(parcels = parcels, metadata = list(id = "t")))
}

# --- 1. Saisie : defaut, valeur valide, valeur hors liste -----------------------

test_that("le type de gestion vaut non_renseigne par defaut", {
  projet <- .eg_projet()
  expect_true(all(projet$ugs$type_gestion == "non_renseigne"))
  expect_true(all(nemetonshiny:::ug_list(projet)$type_gestion == "non_renseigne"))
})

test_that("ug_set_type_gestion enregistre une valeur de la liste", {
  projet <- .eg_projet()
  uid <- projet$ugs$ug_id[2]
  projet <- nemetonshiny:::ug_set_type_gestion(projet, uid, "taillis_sous_futaie")
  expect_equal(projet$ugs$type_gestion, c("non_renseigne", "taillis_sous_futaie", "non_renseigne"))
  for (t in nemetonshiny:::UG_TYPES_GESTION) {
    expect_no_error(nemetonshiny:::ug_set_type_gestion(projet, uid, t))
  }
})

test_that("une valeur hors liste est refusee, rien n'est ecrit", {
  projet <- .eg_projet()
  uid <- projet$ugs$ug_id[1]
  for (mauvais in c("futaie", "", NA_character_, "REGULIERE")) {
    expect_error(nemetonshiny:::ug_set_type_gestion(projet, uid, mauvais), "management type")
  }
  expect_error(nemetonshiny:::ug_set_type_gestion(projet, "inconnu", "taillis"), "Unknown UG")
  expect_true(all(projet$ugs$type_gestion == "non_renseigne"))
})

test_that("le type est enregistre avec le projet et relu ; un projet sans type garde son fichier", {
  skip_if_not_installed("sf")
  withr::with_tempdir({
    testthat::local_mocked_bindings(
      get_app_options = function() list(project_dir = getwd()),
      .package = "nemetonshiny")
    pid <- suppressMessages(nemetonshiny:::create_project(name = "EG", parcels = NULL)$id)
    projet <- .eg_projet()
    f <- file.path(nemetonshiny:::get_project_path(pid), "data", "ugs.json")

    suppressMessages(nemetonshiny:::save_ug_data(pid, projet))
    expect_false(any(grepl("type_gestion", readLines(f))))
    expect_true(all(nemetonshiny:::load_ug_data(pid)$ugs$type_gestion == "non_renseigne"))

    projet <- nemetonshiny:::ug_set_type_gestion(projet, projet$ugs$ug_id[1], "irreguliere")
    projet <- nemetonshiny:::ug_set_type_gestion(projet, projet$ugs$ug_id[3], "libre_evolution")
    suppressMessages(nemetonshiny:::save_ug_data(pid, projet))
    lu <- nemetonshiny:::load_ug_data(pid)$ugs
    lu <- lu[match(projet$ugs$ug_id, lu$ug_id), ]
    expect_identical(lu$type_gestion, c("irreguliere", "non_renseigne", "libre_evolution"))

    # Fichier edite a la main avec une valeur inconnue : retombe sur le defaut.
    txt <- sub("libre_evolution", "n_importe_quoi", paste(readLines(f), collapse = "\n"))
    writeLines(txt, f)
    lu <- nemetonshiny:::load_ug_data(pid)$ugs
    lu <- lu[match(projet$ugs$ug_id, lu$ug_id), ]
    expect_identical(lu$type_gestion, c("irreguliere", "non_renseigne", "non_renseigne"))
  })
})

test_that("fusion et scission : le type est garde seulement quand il est sans ambiguite", {
  projet <- .eg_projet()
  ids <- projet$ugs$ug_id
  p <- nemetonshiny:::ug_set_type_gestion(projet, ids[1], "taillis")
  p <- nemetonshiny:::ug_set_type_gestion(p, ids[2], "taillis")
  m <- nemetonshiny:::ug_merge(p, ids[1:2], "fusion")
  expect_equal(m$ugs$type_gestion[m$ugs$label == "fusion"], "taillis")

  p2 <- nemetonshiny:::ug_set_type_gestion(projet, ids[1], "taillis")
  p2 <- nemetonshiny:::ug_set_type_gestion(p2, ids[2], "reguliere")
  m2 <- nemetonshiny:::ug_merge(p2, ids[1:2], "melange")
  expect_equal(m2$ugs$type_gestion[m2$ugs$label == "melange"], "non_renseigne")

  s <- nemetonshiny:::ug_split(m, m$ugs$ug_id[m$ugs$label == "fusion"])
  expect_equal(sum(s$ugs$type_gestion == "taillis"), 2L)
})

# --- 2. Service : types renseignes, unites, appel du coeur -----------------------

.eg_ugs <- function(types) {
  data.frame(ug_id = paste0("u", seq_along(types)), label = "x", groupe = NA_character_,
             type_gestion = types, stringsAsFactors = FALSE)
}

test_that("non_renseigne ne compte pas comme un type ; il en faut deux", {
  f <- nemetonshiny:::effet_gestion_calculable
  expect_false(f(.eg_ugs(c("non_renseigne", "non_renseigne"))))
  expect_false(f(.eg_ugs(c("reguliere", "reguliere", "non_renseigne"))))
  expect_true(f(.eg_ugs(c("reguliere", "taillis", "non_renseigne"))))
  expect_false(f(NULL))
  expect_equal(nemetonshiny:::effet_gestion_types_renseignes(.eg_ugs(c("taillis", "taillis", "reguliere"))),
               c(reguliere = 1L, taillis = 2L))
})

.eg_fam <- function(n = 6) {
  data.frame(ug_id = paste0("u", seq_len(n)),
             famille_biodiversite = seq_len(n) * 10,
             famille_production = seq_len(n) * 5,
             indicateur_p2_station = seq_len(n) / 2,
             .p2_ser = "A11", stringsAsFactors = FALSE)
}

test_that("effet_gestion_unites : type NA si non renseigne, covariables du projet", {
  ugs <- .eg_ugs(c("reguliere", "taillis", "non_renseigne", "reguliere", "taillis", "reguliere"))
  u <- nemetonshiny:::effet_gestion_unites(.eg_fam(), ugs)
  expect_equal(u$type_gestion, c("reguliere", "taillis", NA, "reguliere", "taillis", "reguliere"))
  expect_true(all(c("P2", "ser", "famille_biodiversite", "famille_production") %in% names(u)))
  expect_false("altitude" %in% names(u))   # absente du projet : jamais inventee
  expect_null(nemetonshiny:::effet_gestion_unites(NULL, ugs))
})

.eg_resume <- function() {
  data.frame(
    traitement = c("taillis", "taillis", "irreguliere", "libre_evolution", "libre_evolution"),
    reference = "reguliere",
    resultat = c("famille_biodiversite", "famille_production", "famille_biodiversite",
                 "famille_biodiversite", "famille_production"),
    n_traite = c(12L, 12L, 14L, 3L, 3L), n_reference = c(30L, 30L, 30L, 30L, 30L),
    part_support = c(0.9, 0.9, 0.5, NA, NA),
    effet_moyen = c(5.94, -2.5, NA, NA, NA),
    ic_bas = c(2.1, -6.04, NA, NA, NA), ic_haut = c(9.8, 1.2, NA, NA, NA),
    statut = c("calcule", "calcule", "recouvrement_insuffisant", "trop_peu_unites", "trop_peu_unites"),
    stringsAsFactors = FALSE)
}

test_that("le coeur recoit la bonne colonne de traitement et la reference", {
  recu <- NULL
  fn <- function(units, traitement, reference, ...) {
    recu <<- list(units = units, traitement = traitement, reference = reference)
    structure(list(resume = .eg_resume(), reference = "reguliere", apprenant = "ranger",
                   covariables = c("P2", "ser")), class = "effet_gestion")
  }
  ugs <- .eg_ugs(c("reguliere", "taillis", "non_renseigne", "reguliere", "taillis", "reguliere"))
  u <- nemetonshiny:::effet_gestion_unites(.eg_fam(), ugs)
  r <- nemetonshiny:::effet_gestion_calculer(u, reference = "reguliere", fn = fn)
  expect_identical(recu$traitement, "type_gestion")
  expect_identical(recu$reference, "reguliere")
  expect_true("type_gestion" %in% names(recu$units))
  expect_null(r$erreur)
  expect_equal(nrow(r$resume), 5L)
})

test_that("sans covariable de station ou si le coeur echoue : une erreur rendue, pas de plantage", {
  appele <- FALSE
  fn <- function(...) { appele <<- TRUE; stop("boom") }
  u <- data.frame(type_gestion = c("a", "b"), famille_x = 1:2)
  r <- nemetonshiny:::effet_gestion_calculer(u, fn = fn)
  expect_identical(r$erreur, "sans_covariable")
  expect_false(appele)
  u$P2 <- 1:2
  r <- nemetonshiny:::effet_gestion_calculer(u, fn = fn)
  expect_identical(r$erreur, "boom")
  expect_null(r$resume)
})

# --- 3. Tableau : IC avant le chiffre, statut seul, mention ----------------------

test_that("le tableau met l'intervalle avant l'effet, avec n traite / n reference", {
  for (lang in c("fr", "en")) {
    i18n <- nemetonshiny:::get_i18n(lang)
    tab <- nemetonshiny:::effet_gestion_tableau(.eg_resume(), i18n)
    noms <- names(tab)
    expect_lt(match(i18n$t("eg_col_ic"), noms), match(i18n$t("eg_col_effet"), noms))
    l1 <- tab[tab[[i18n$t("eg_col_famille")]] == "Biodiversité" |
              tab[[i18n$t("eg_col_famille")]] == "Biodiversity", ][1, ]
    expect_match(l1[[i18n$t("eg_col_ic")]], if (lang == "fr") "^2,1 – 9,8$" else "^2\\.1 – 9\\.8$")
    expect_match(l1[[i18n$t("eg_col_effet")]], if (lang == "fr") "^5,9$" else "^5\\.9$")
    expect_equal(l1[[i18n$t("eg_col_n")]], "12 / 30")
  }
})

test_that("statut different de calcule : le statut traduit et rien d'autre", {
  for (lang in c("fr", "en")) {
    i18n <- nemetonshiny:::get_i18n(lang)
    tab <- nemetonshiny:::effet_gestion_tableau(.eg_resume(), i18n)
    ge <- tab[[i18n$t("eg_col_gestion")]]
    rec <- tab[ge == nemetonshiny:::effet_gestion_libelle_type("irreguliere", i18n), ]
    tro <- tab[ge == nemetonshiny:::effet_gestion_libelle_type("libre_evolution", i18n), ]
    expect_equal(nrow(rec), 1L)
    expect_equal(nrow(tro), 1L)   # un contraste refuse tient en une ligne
    expect_equal(rec[[i18n$t("eg_col_ic")]], i18n$t("eg_st_recouvrement"))
    expect_equal(tro[[i18n$t("eg_col_ic")]], i18n$t("eg_st_trop_peu"))
    for (l in list(rec, tro)) {
      expect_equal(l[[i18n$t("eg_col_effet")]], "")
      expect_equal(l[[i18n$t("eg_col_n")]], "")
    }
    # jamais le code brut
    expect_false(any(grepl("recouvrement_insuffisant|trop_peu_unites", unlist(tab))))
  }
})

test_that("cles i18n : FR et EN presentes, 30 caracteres au plus, mention de non-causalite", {
  cles <- grep("^(eg_|ug_gestion)", names(nemetonshiny:::TRANSLATIONS), value = TRUE)
  expect_gt(length(cles), 25L)
  expect_true(all(nchar(cles) <= 30L))
  for (k in cles) {
    expect_true(nzchar(nemetonshiny:::get_i18n("fr")$t(k)), info = k)
    expect_true(nzchar(nemetonshiny:::get_i18n("en")$t(k)), info = k)
  }
  expect_match(nemetonshiny:::get_i18n("fr")$t("eg_mention"), "pas une preuve causale")
  expect_match(nemetonshiny:::get_i18n("en")$t("eg_mention"), "not causal proof")
})

# --- 4. Module : bouton, ExtendedTask, notification ------------------------------

.eg_state <- function(types, lang = "fr") {
  projet <- list(id = "p", ugs = .eg_ugs(types))
  shiny::reactiveValues(language = lang, project_id = "p", current_project = projet)
}

.eg_attendre <- function(session, n = 30L) {
  for (i in seq_len(n)) { later::run_now(0.05); session$flushReact() }
}

test_that("moins de deux types : pas de tableau, message de saisie, aucun appel au coeur", {
  appele <- FALSE
  testthat::local_mocked_bindings(
    get_app_options = function() list(language = "fr"),
    project_family_scores = function(p) .eg_fam(),
    .package = "nemetonshiny")
  testthat::local_mocked_bindings(
    effet_gestion = function(...) { appele <<- TRUE; stop("ne doit pas etre appele") },
    .package = "nemeton")
  notes <- character(0)
  testthat::local_mocked_bindings(
    showNotification = function(ui, ...) { notes <<- c(notes, as.character(ui)); invisible("id") },
    removeNotification = function(...) invisible(NULL),
    .package = "shiny")
  st <- .eg_state(c("reguliere", "reguliere", "non_renseigne", "non_renseigne", "reguliere", "reguliere"))
  shiny::testServer(nemetonshiny:::mod_effet_gestion_server, args = list(app_state = st), {
    session$setInputs(run = 1L)
    .eg_attendre(session, 5L)
    expect_false(appele)
    expect_true(any(grepl(nemetonshiny:::get_i18n("fr")$t("eg_need_types"), notes, fixed = TRUE)))
    expect_match(output$intro$html, nemetonshiny:::get_i18n("fr")$t("eg_need_types"), fixed = TRUE)
    expect_null(output$tableau)
    expect_equal(session$returned$status(), "initial")
  })
})

test_that("deux types : calcul par ExtendedTask, notification, tableau avec la mention", {
  recu <- NULL
  testthat::local_mocked_bindings(
    get_app_options = function() list(language = "fr"),
    project_family_scores = function(p) .eg_fam(),
    .package = "nemetonshiny")
  testthat::local_mocked_bindings(
    effet_gestion = function(units, traitement, reference, ...) {
      recu <<- list(units = units, traitement = traitement, reference = reference)
      structure(list(resume = .eg_resume(), reference = "reguliere", apprenant = "ranger",
                     covariables = c("P2", "ser")), class = "effet_gestion")
    },
    .package = "nemeton")
  notes <- list()
  testthat::local_mocked_bindings(
    showNotification = function(ui, id = NULL, ...) {
      notes[[length(notes) + 1L]] <<- list(txt = as.character(ui), id = id); invisible("id")
    },
    removeNotification = function(id, ...) { notes[[length(notes) + 1L]] <<- list(txt = "REMOVED", id = id); invisible(NULL) },
    .package = "shiny")
  i18n <- nemetonshiny:::get_i18n("fr")
  st <- .eg_state(c("reguliere", "taillis", "non_renseigne", "reguliere", "taillis", "reguliere"))
  shiny::testServer(nemetonshiny:::mod_effet_gestion_server, args = list(app_state = st), {
    session$setInputs(run = 1L)
    session$flushReact()
    # retour immediat : la tache tourne (le bouton lie est grise), notification posee
    expect_equal(session$returned$status(), "running")
    expect_true(any(vapply(notes, function(n) identical(n$txt, i18n$t("eg_running")), logical(1))))
    .eg_attendre(session)
    expect_equal(session$returned$status(), "success")
    expect_identical(recu$traitement, "type_gestion")
    expect_true(any(vapply(notes, function(n) identical(n$txt, "REMOVED"), logical(1))))
    expect_true(any(vapply(notes, function(n) identical(n$txt, i18n$t("eg_done")), logical(1))))
    html <- output$tableau
    expect_match(html, "2,1 – 9,8", fixed = TRUE)
    expect_match(output$mention$html, "pas une preuve causale", fixed = TRUE)
    expect_match(output$etat$html, i18n$t("eg_ref_fmt") |> sub(pattern = "%s", replacement = "", fixed = TRUE) |> trimws(), fixed = TRUE)
  })
})

test_that("si les types changent apres le calcul, le tableau disparait", {
  testthat::local_mocked_bindings(
    get_app_options = function() list(language = "fr"),
    project_family_scores = function(p) .eg_fam(),
    .package = "nemetonshiny")
  testthat::local_mocked_bindings(
    effet_gestion = function(...) structure(list(resume = .eg_resume(), reference = "reguliere"),
                                            class = "effet_gestion"),
    .package = "nemeton")
  testthat::local_mocked_bindings(
    showNotification = function(...) invisible("id"), removeNotification = function(...) invisible(NULL),
    .package = "shiny")
  st <- .eg_state(c("reguliere", "taillis", "non_renseigne", "reguliere", "taillis", "reguliere"))
  shiny::testServer(nemetonshiny:::mod_effet_gestion_server, args = list(app_state = st), {
    session$setInputs(run = 1L)
    .eg_attendre(session)
    expect_match(output$tableau, "2,1", fixed = TRUE)
    p <- app_state$current_project
    p$ugs$type_gestion <- "reguliere"
    app_state$current_project <- p
    session$flushReact()
    expect_null(output$tableau)
  })
})

test_that("l'UI de la synthese porte le bouton de calcul (tache lie) et le selecteur de type l'UGF", {
  skip_if_not_installed("bslib")
  h <- testthat::with_mocked_bindings(
    get_app_options = function() list(language = "fr"),
    as.character(nemetonshiny:::mod_synthesis_ui("syn")),
    .package = "nemetonshiny")
  expect_match(h, "syn-effet_gestion-run", fixed = TRUE)
  expect_match(h, "bslib-task-button", fixed = TRUE)
  ug <- testthat::with_mocked_bindings(
    get_app_options = function() list(language = "fr"),
    as.character(nemetonshiny:::mod_ug_table_actions_bar("ug")),
    .package = "nemetonshiny")
  expect_match(ug, "ug-sel_type_gestion", fixed = TRUE)
  expect_match(ug, "taillis_sous_futaie", fixed = TRUE)
})

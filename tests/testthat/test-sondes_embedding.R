# Mission 078 : vue « Ce que les modeles voient » (nemeton::probe_indicator,
# spec 066 du coeur). Le coeur est simule : aucun calcul reel ici.

withr::local_options(nemetonshiny.sondes_inline = TRUE,
                     .local_envir = testthat::teardown_env())

# 25 UGF de 200 m dans un raster factice de 64 bandes (Lambert-93).
.sd_unites_sf <- function(avec_s2 = TRUE) {
  g <- lapply(0:24, function(i) {
    x <- 900000 + (i %% 5) * 220 + 20
    y <- 6800000 + (i %/% 5) * 220 + 20
    sf::st_polygon(list(matrix(c(x, y, x + 200, y, x + 200, y + 200, x, y + 200, x, y),
                               ncol = 2, byrow = TRUE)))
  })
  u <- sf::st_sf(ug_id = sprintf("u%02d", 1:25),
                 indicateur_c2_ndvi = seq(0.2, 0.8, length.out = 25),
                 indicateur_b1_protection = rep(c(0, 50, 100), length.out = 25),
                 indicateur_p1_volume = rep(NA_real_, 25),
                 geometry = sf::st_sfc(g, crs = 2154))
  u$.c2_status <- "calculated"
  u
}

.sd_raster <- function(dir) {
  set.seed(1)
  r <- terra::rast(nrows = 120, ncols = 120, nlyrs = 64,
                   xmin = 900000, xmax = 901200, ymin = 6800000, ymax = 6801200,
                   crs = "EPSG:2154")
  terra::values(r) <- matrix(stats::rnorm(120 * 120 * 64), ncol = 64)
  names(r) <- sprintf("A%02d", 0:63)
  f <- file.path(dir, "aef_2025_test.tif")
  terra::writeRaster(r, f, overwrite = TRUE)
  f
}

# Projet sur disque avec l'embedding en cache (comme apres un calcul).
.sd_projet <- function(avec_embedding = TRUE, env = parent.frame()) {
  d <- withr::local_tempdir(.local_envir = env)
  u <- .sd_unites_sf()
  if (avec_embedding) {
    dir <- nemetonshiny:::.aef_cache_dir(d)
    dir.create(dir, recursive = TRUE)
    f <- .sd_raster(dir)
    jsonlite::write_json(
      list(cle = nemetonshiny:::.aef_cle(u), fichier = basename(f), annee = 2025L),
      file.path(dir, "courant.json"), auto_unbox = TRUE)
  }
  list(id = "p", path = d, indicators_sf = u)
}

.sd_table <- function() {
  data.frame(
    code = c("B1", "C2", "R2", "P1"),
    famille = c("B", "C", "R", "P"),
    n = c(25L, 25L, 25L, 0L),
    r2 = c(0.1, 0.72, 0.31, NA),
    rmse = c(20.1, 5.5, 12.2, NA),
    lecture = c("angle_mort", "voit", "partiel", NA),
    statut = c("ok", "ok", "ok", "trop_peu_unites"),
    source_commune = c(FALSE, TRUE, FALSE, FALSE),
    source_embedding = "aef", stringsAsFactors = FALSE)
}

.sd_state <- function(projet, lang = "fr") {
  shiny::reactiveValues(language = lang, project_id = "p", current_project = projet)
}

.sd_attendre <- function(session, n = 30L) {
  for (i in seq_len(n)) { later::run_now(0.05); session$flushReact() }
}

.sd_mocks <- function(env = parent.frame()) {
  testthat::local_mocked_bindings(
    get_app_options = function() list(language = "fr"),
    project_aef_active = function(metadata) TRUE,
    .package = "nemetonshiny", .env = env)
  testthat::local_mocked_bindings(
    showNotification = function(...) invisible("id"),
    removeNotification = function(...) invisible(NULL),
    .package = "shiny", .env = env)
}

# --- 1. Unites pour le coeur -----------------------------------------------------

test_that("sondes_unites : colonnes nommees par code, statut renomme, colonne vide ecartee", {
  u <- nemetonshiny:::sondes_unites(.sd_unites_sf())
  expect_s3_class(u, "sf")
  expect_true(all(c("C2", "B1", "c2_status") %in% names(u)))
  expect_false("P1" %in% names(u))          # aucune valeur : pas de sonde
  expect_false(any(grepl("^indicateur_", names(u))))
  expect_equal(u$C2, .sd_unites_sf()$indicateur_c2_ndvi)
  expect_null(nemetonshiny:::sondes_unites(NULL))
  expect_null(nemetonshiny:::sondes_unites(.sd_unites_sf()[0, ]))
})

# --- 2. Embedding : cache seulement ----------------------------------------------

test_that("sondes_embedding relit le cache AlphaEarth, sans paquet ni reseau ; sinon NULL", {
  p <- .sd_projet(TRUE)
  e <- nemetonshiny:::sondes_embedding(p$indicators_sf, p$path)
  expect_true(file.exists(e$fichier))
  expect_equal(e$annee, 2025L)
  sans <- .sd_projet(FALSE)
  expect_null(nemetonshiny:::sondes_embedding(sans$indicators_sf, sans$path))
  expect_null(nemetonshiny:::sondes_embedding(NULL, p$path))
  # une autre emprise : le cache n'est pas le bon
  autre <- p$indicators_sf
  sf::st_geometry(autre) <- sf::st_geometry(autre) + c(50000, 0)
  sf::st_crs(autre) <- 2154
  expect_null(nemetonshiny:::sondes_embedding(autre, p$path))
})

# --- 3. Tableau : tri, traductions, marqueur --------------------------------------

test_that("tableau : trie par R2 decroissant, les non calculees a la suite", {
  i18n <- nemetonshiny:::get_i18n("fr")
  tab <- nemetonshiny:::sondes_tableau(.sd_table()[c(4, 1, 3, 2), ], i18n)
  codes <- sub(" - .*$", "", tab[[i18n$t("sd_col_indicateur")]])
  expect_equal(codes, c("C2", "R2", "B1", "P1"))
  expect_equal(tab[[i18n$t("sd_col_r2")]], c("0,72", "0,31", "0,10", ""))
})

test_that("tableau : trois lectures traduites FR et EN, jamais le code brut ; statut traduit", {
  for (lang in c("fr", "en")) {
    i18n <- nemetonshiny:::get_i18n(lang)
    tab <- nemetonshiny:::sondes_tableau(.sd_table(), i18n)
    lect <- tab[[i18n$t("sd_col_lecture")]]
    expect_equal(lect, c(i18n$t("sd_lect_voit"), i18n$t("sd_lect_partiel"),
                         i18n$t("sd_lect_angle"), i18n$t("sd_st_trop_peu")))
    expect_false(any(grepl("angle_mort|partiel$|trop_peu_unites|^voit$", lect)))
  }
  expect_match(nemetonshiny:::get_i18n("fr")$t("sd_lect_voit"), "voit")
  expect_match(nemetonshiny:::get_i18n("en")$t("sd_lect_angle"), "Blind spot")
})

test_that("tableau : source_commune visible pour C2, absent ailleurs", {
  i18n <- nemetonshiny:::get_i18n("fr")
  tab <- nemetonshiny:::sondes_tableau(.sd_table(), i18n)
  commune <- tab[[i18n$t("sd_col_commune")]]
  code <- sub(" - .*$", "", tab[[i18n$t("sd_col_indicateur")]])
  expect_equal(commune[code == "C2"], i18n$t("sd_commune_oui"))
  expect_equal(commune[code != "C2"], rep("", 3))
  expect_match(i18n$t("sd_commune_oui"), "ne prouve rien")
})

test_that("tableau vide : colonnes presentes, zero ligne", {
  tab <- nemetonshiny:::sondes_tableau(NULL, nemetonshiny:::get_i18n("fr"))
  expect_equal(nrow(tab), 0L)
  expect_equal(ncol(tab), 7L)
})

# --- 4. Service : le coeur et ses erreurs -----------------------------------------

test_that("sondes_calculer : appelle le coeur avec l'embedding relu, source aef", {
  p <- .sd_projet(TRUE)
  units <- nemetonshiny:::sondes_unites(p$indicators_sf)
  e <- nemetonshiny:::sondes_embedding(p$indicators_sf, p$path)
  recu <- NULL
  res <- nemetonshiny:::sondes_calculer(
    units, e$fichier, e$annee,
    fn = function(units, embedding, ...) {
      recu <<- list(units = units, embedding = embedding)
      .sd_table()
    })
  expect_null(res$erreur)
  expect_true(methods::is(recu$embedding, "SpatRaster"))
  expect_equal(terra::nlyr(recu$embedding), 64L)
  expect_identical(attr(recu$embedding, "embedding_source"), "aef")
  expect_equal(nrow(res$table), 4L)
})

test_that("sondes_calculer : erreur du coeur rendue dans erreur, sans plantage", {
  p <- .sd_projet(TRUE)
  units <- nemetonshiny:::sondes_unites(p$indicators_sf)
  e <- nemetonshiny:::sondes_embedding(p$indicators_sf, p$path)
  res <- nemetonshiny:::sondes_calculer(units, e$fichier, e$annee,
                                        fn = function(...) stop("boom du coeur"))
  expect_match(res$erreur, "boom du coeur")
  expect_null(res$table)
  expect_equal(nemetonshiny:::sondes_calculer(NULL, e$fichier)$erreur, "sans_unites")
  expect_equal(nemetonshiny:::sondes_calculer(units, "/nope.tif")$erreur, "sans_embedding")
})

# --- 5. Cache par projet ------------------------------------------------------------

test_that("cache : relu si la signature est la meme, ignore sinon ; une erreur n'est pas rangee", {
  p <- .sd_projet(TRUE)
  units <- nemetonshiny:::sondes_unites(p$indicators_sf)
  e <- nemetonshiny:::sondes_embedding(p$indicators_sf, p$path)
  sig <- nemetonshiny:::sondes_signature(units, e)
  expect_true(nzchar(sig))
  expect_null(nemetonshiny:::sondes_cache_lire(p$path, sig))
  res <- list(table = .sd_table(), source = "aef", annee = 2025L, erreur = NULL)
  nemetonshiny:::sondes_cache_ecrire(p$path, sig, res)
  expect_equal(nemetonshiny:::sondes_cache_lire(p$path, sig)$table, .sd_table())
  autre <- units
  autre$C2[1] <- 0.99
  sig2 <- nemetonshiny:::sondes_signature(autre, e)
  expect_false(identical(sig, sig2))
  expect_null(nemetonshiny:::sondes_cache_lire(p$path, sig2))
  p2 <- .sd_projet(TRUE)
  expect_false(nemetonshiny:::sondes_cache_ecrire(
    p2$path, sig, list(table = NULL, erreur = "x")))
  expect_null(nemetonshiny:::sondes_cache_lire(p2$path, sig))
})

# --- 6. Module ------------------------------------------------------------------------

test_that("sans embedding : message, aucun appel au coeur, pas de table", {
  .sd_mocks()
  appele <- FALSE
  testthat::local_mocked_bindings(
    probe_indicator = function(...) { appele <<- TRUE; stop("ne doit pas etre appele") },
    .package = "nemeton")
  notes <- character(0)
  testthat::local_mocked_bindings(
    showNotification = function(ui, ...) { notes <<- c(notes, as.character(ui)); invisible("id") },
    removeNotification = function(...) invisible(NULL), .package = "shiny")
  i18n <- nemetonshiny:::get_i18n("fr")
  st <- .sd_state(.sd_projet(FALSE))
  shiny::testServer(nemetonshiny:::mod_sondes_embedding_server, args = list(app_state = st), {
    expect_match(output$intro$html, "Aucun embedding AlphaEarth pour ce projet", fixed = TRUE)
    session$setInputs(run = 1L)
    .sd_attendre(session, 5L)
    expect_false(appele)
    expect_true(i18n$t("sd_no_embedding") %in% notes)
    expect_null(output$tableau)
    expect_equal(session$returned$status(), "initial")
  })
})

test_that("avec embedding : tache, table triee par R2, lecture et marqueur, mention", {
  .sd_mocks()
  recu <- NULL
  testthat::local_mocked_bindings(
    probe_indicator = function(units, embedding, ...) { recu <<- list(units, embedding); .sd_table() },
    .package = "nemeton")
  i18n <- nemetonshiny:::get_i18n("fr")
  st <- .sd_state(.sd_projet(TRUE))
  shiny::testServer(nemetonshiny:::mod_sondes_embedding_server, args = list(app_state = st), {
    expect_match(output$intro$html, "2025", fixed = TRUE)
    session$setInputs(run = 1L)
    session$flushReact()
    expect_equal(session$returned$status(), "running")
    .sd_attendre(session)
    expect_equal(session$returned$status(), "success")
    expect_true("C2" %in% names(recu[[1]]))
    html <- output$tableau
    expect_lt(regexpr("C2 - ", html, fixed = TRUE)[1], regexpr("B1 - ", html, fixed = TRUE)[1])
    expect_match(html, i18n$t("sd_lect_voit"), fixed = TRUE)
    expect_match(html, i18n$t("sd_lect_angle"), fixed = TRUE)
    expect_match(html, "ne prouve rien", fixed = TRUE)
    expect_match(output$mention$html, "pas un estimateur", fixed = TRUE)
    expect_match(output$mention$html, "0,5", fixed = TRUE)
  })
})

test_that("le second calcul sur les memes donnees relit le cache du projet (coeur appele une fois)", {
  .sd_mocks()
  n <- 0L
  testthat::local_mocked_bindings(
    probe_indicator = function(...) { n <<- n + 1L; .sd_table() }, .package = "nemeton")
  st <- .sd_state(.sd_projet(TRUE))
  shiny::testServer(nemetonshiny:::mod_sondes_embedding_server, args = list(app_state = st), {
    session$setInputs(run = 1L)
    .sd_attendre(session)
    expect_equal(n, 1L)
    expect_true(file.exists(nemetonshiny:::.sondes_cache_fichier(app_state$current_project$path)))
    session$setInputs(run = 2L)
    .sd_attendre(session)
    expect_equal(n, 1L)
    expect_match(output$tableau, "C2 - ", fixed = TRUE)
  })
})

test_that("erreur du coeur : message, pas de table, pas de plantage, rien en cache", {
  .sd_mocks()
  notes <- character(0)
  testthat::local_mocked_bindings(
    probe_indicator = function(...) stop("echec du coeur"), .package = "nemeton")
  testthat::local_mocked_bindings(
    showNotification = function(ui, ...) { notes <<- c(notes, as.character(ui)); invisible("id") },
    removeNotification = function(...) invisible(NULL), .package = "shiny")
  st <- .sd_state(.sd_projet(TRUE))
  shiny::testServer(nemetonshiny:::mod_sondes_embedding_server, args = list(app_state = st), {
    session$setInputs(run = 1L)
    .sd_attendre(session)
    expect_equal(session$returned$status(), "success")   # le service rend l'erreur
    expect_true(any(grepl("echec du coeur", notes)))
    expect_match(output$etat$html, "echec du coeur", fixed = TRUE)
    expect_null(output$tableau)
    expect_false(file.exists(nemetonshiny:::.sondes_cache_fichier(app_state$current_project$path)))
  })
})

test_that("si les indicateurs changent apres le calcul, la table disparait", {
  .sd_mocks()
  testthat::local_mocked_bindings(
    probe_indicator = function(...) .sd_table(), .package = "nemeton")
  st <- .sd_state(.sd_projet(TRUE))
  shiny::testServer(nemetonshiny:::mod_sondes_embedding_server, args = list(app_state = st), {
    session$setInputs(run = 1L)
    .sd_attendre(session)
    expect_match(output$tableau, "C2 - ", fixed = TRUE)
    p <- app_state$current_project
    p$indicators_sf$indicateur_c2_ndvi[1] <- 0.95
    app_state$current_project <- p
    session$flushReact()
    expect_null(output$tableau)
    expect_match(output$etat$html, nemetonshiny:::get_i18n("fr")$t("sd_perime"), fixed = TRUE)
  })
})

test_that("l'UI porte le bouton lie a la tache, a bordure ; la synthese l'accueille", {
  h <- testthat::with_mocked_bindings(
    get_app_options = function() list(language = "fr"),
    as.character(nemetonshiny:::mod_sondes_embedding_ui("sd")),
    .package = "nemetonshiny")
  expect_match(h, "sd-run", fixed = TRUE)
  expect_match(h, "bslib-task-button", fixed = TRUE)
  expect_match(h, "btn-outline-primary", fixed = TRUE)
  f <- "../../R/app_ui.R"
  skip_if_not(file.exists(f))
  expect_match(paste(readLines(f, warn = FALSE), collapse = "\n"),
               'mod_sondes_embedding_ui(ns("sondes_embedding"))', fixed = TRUE)
})

test_that("cles i18n sd_ : FR et EN, 30 caracteres au plus", {
  cles <- grep("^sd_", names(nemetonshiny:::TRANSLATIONS), value = TRUE)
  expect_gt(length(cles), 25L)
  expect_true(all(nchar(cles) <= 30L))
  for (k in cles) {
    expect_true(nzchar(nemetonshiny:::get_i18n("fr")$t(k)), info = k)
    expect_true(nzchar(nemetonshiny:::get_i18n("en")$t(k)), info = k)
  }
})

# --- 7. Revue de la mission 078 -----------------------------------------------------

test_that("signature : geometrie deplacee, autre annee, autre fichier, autre version du coeur -> perime", {
  p <- .sd_projet(TRUE)
  units <- nemetonshiny:::sondes_unites(p$indicators_sf)
  e <- nemetonshiny:::sondes_embedding(p$indicators_sf, p$path)
  sig <- nemetonshiny:::sondes_signature(units, e)
  expect_equal(nemetonshiny:::sondes_signature(units, e), sig)   # stable

  deplace <- units
  g <- sf::st_geometry(deplace); g[[1]] <- g[[1]] + c(30, 0)
  sf::st_geometry(deplace) <- g; sf::st_crs(deplace) <- 2154
  expect_false(identical(nemetonshiny:::sondes_signature(deplace, e), sig))

  expect_false(identical(
    nemetonshiny:::sondes_signature(units, list(fichier = e$fichier, annee = 2024L)), sig))

  # autre fichier (meme nom, autre taille) : la taille compte
  f2 <- file.path(withr::local_tempdir(), basename(e$fichier))
  file.copy(e$fichier, f2, copy.date = TRUE)
  expect_equal(nemetonshiny:::sondes_signature(units, list(fichier = f2, annee = e$annee)), sig)
  cat("x", file = f2, append = TRUE)
  expect_false(identical(
    nemetonshiny:::sondes_signature(units, list(fichier = f2, annee = e$annee)), sig))
  # meme taille, autre date
  f3 <- file.path(withr::local_tempdir(), basename(e$fichier))
  file.copy(e$fichier, f3)
  Sys.setFileTime(f3, Sys.time() + 3600)
  expect_false(identical(
    nemetonshiny:::sondes_signature(units, list(fichier = f3, annee = e$annee)), sig))

  # autre version du coeur
  testthat::local_mocked_bindings(
    packageVersion = function(pkg, ...) package_version("9999.1.1"), .package = "utils")
  expect_false(identical(nemetonshiny:::sondes_signature(units, e), sig))
})

test_that("cache : une signature d'une autre version du coeur n'est pas relue", {
  p <- .sd_projet(TRUE)
  units <- nemetonshiny:::sondes_unites(p$indicators_sf)
  e <- nemetonshiny:::sondes_embedding(p$indicators_sf, p$path)
  sig <- nemetonshiny:::sondes_signature(units, e)
  nemetonshiny:::sondes_cache_ecrire(p$path, sig, list(table = .sd_table(), erreur = NULL))
  testthat::local_mocked_bindings(
    packageVersion = function(pkg, ...) package_version("9999.1.1"), .package = "utils")
  sig2 <- nemetonshiny:::sondes_signature(units, e)
  expect_null(nemetonshiny:::sondes_cache_lire(p$path, sig2))
})

test_that("la notification de progression est retiree sous le meme id (succes et erreur)", {
  for (mode in c("succes", "erreur")) {
    ids_posees <- character(0); ids_retirees <- character(0)
    testthat::local_mocked_bindings(
      get_app_options = function() list(language = "fr"),
      project_aef_active = function(metadata) TRUE, .package = "nemetonshiny")
    testthat::local_mocked_bindings(
      showNotification = function(ui, id = NULL, ...) {
        if (!is.null(id)) ids_posees <<- c(ids_posees, id); invisible("id") },
      removeNotification = function(id, ...) { ids_retirees <<- c(ids_retirees, id); invisible(NULL) },
      .package = "shiny")
    testthat::local_mocked_bindings(
      probe_indicator = function(...) if (mode == "erreur") stop("x") else .sd_table(),
      .package = "nemeton")
    st <- .sd_state(.sd_projet(TRUE))
    shiny::testServer(nemetonshiny:::mod_sondes_embedding_server, args = list(app_state = st), {
      session$setInputs(run = 1L)
      .sd_attendre(session)
    })
    expect_length(ids_posees, 1L)
    expect_equal(ids_retirees, ids_posees, info = mode)
  }
})

test_that("projet change pendant la tache : resultat ignore, table NULL, rien en cache", {
  .sd_mocks()
  testthat::local_mocked_bindings(
    probe_indicator = function(...) .sd_table(), .package = "nemeton")
  projet <- .sd_projet(TRUE)
  st <- .sd_state(projet)
  shiny::testServer(nemetonshiny:::mod_sondes_embedding_server, args = list(app_state = st), {
    session$setInputs(run = 1L)
    session$flushReact()
    expect_equal(session$returned$status(), "running")
    app_state$project_id <- "autre"
    app_state$current_project <- list(id = "autre", path = NULL, indicators_sf = NULL)
    .sd_attendre(session)
    expect_null(session$returned$resultat())
    expect_null(output$tableau)
    expect_false(file.exists(nemetonshiny:::.sondes_cache_fichier(projet$path)))
  })
})

test_that("reponse inattendue : libelle dedie, pas celui du refus du coeur", {
  i18n <- nemetonshiny:::get_i18n("fr")
  expect_equal(nemetonshiny:::.sd_erreur_lisible("reponse_inattendue", i18n),
               i18n$t("sd_reponse_inattendue"))
  expect_match(i18n$t("sd_reponse_inattendue"), "inattendue")
  expect_match(i18n$t("sd_col_rmse"), "0-100")
})

# --- 8. Integration : le vrai coeur, sans simulacre ----------------------------------

test_that("integration : vrai probe_indicator sur 60 UGF et un raster de 64 bandes", {
  skip_if_not_installed("nemeton")
  skip_if_not("probe_indicator" %in% getNamespaceExports("nemeton"))
  set.seed(7)
  n <- 60
  g <- lapply(0:(n - 1), function(i) {
    x <- 900000 + (i %% 10) * 2400 + 10; y <- 6800000 + (i %/% 10) * 2400 + 10
    sf::st_polygon(list(matrix(c(x, y, x + 1000, y, x + 1000, y + 1000, x, y + 1000, x, y),
                               ncol = 2, byrow = TRUE)))
  })
  geo <- sf::st_sfc(g, crs = 2154)
  r <- terra::rast(nrows = 160, ncols = 160, nlyrs = 64, xmin = 900000, xmax = 925000,
                   ymin = 6800000, ymax = 6815000, crs = "EPSG:2154")
  terra::values(r) <- matrix(stats::rnorm(160 * 160 * 64), ncol = 64)
  names(r) <- sprintf("A%02d", 0:63)
  f <- withr::local_tempfile(fileext = ".tif")
  terra::writeRaster(r, f)
  emb <- terra::rast(f)
  x <- nemeton::embedding_summary(sf::st_sf(geometry = geo), emb, stats = "mean")$mean
  u <- sf::st_sf(
    ug_id = seq_len(n),
    # B1 = combinaison lineaire de l'embedding (le modele doit la voir) ;
    # B2 = bruit (angle mort)
    indicateur_b1_protection = as.numeric(scale(x[, 1:4] %*% c(1, 1, 1, 1))) * 20 + 50,
    indicateur_b2_structure = stats::runif(n) * 100,
    geometry = geo)
  units <- nemetonshiny:::sondes_unites(u)
  # noms de colonnes attendus par le coeur
  expect_true(all(c("B1", "B2") %in% names(units)))
  res <- nemetonshiny:::sondes_calculer(units, f, 2025L)
  expect_null(res$erreur)
  t <- res$table
  expect_true(all(c("code", "famille", "n", "r2", "rmse", "lecture", "statut",
                    "source_commune", "source_embedding") %in% names(t)))
  expect_equal(sort(t$code), c("B1", "B2"))
  expect_true(all(t$statut == "ok"))
  expect_true(all(t$source_commune == FALSE))
  expect_equal(unique(t$source_embedding), "aef")
  expect_gt(t$r2[t$code == "B1"], 0.5)
  expect_equal(t$lecture[t$code == "B1"], "voit")
  expect_lt(t$r2[t$code == "B2"], 0.2)
  expect_equal(t$lecture[t$code == "B2"], "angle_mort")
  tab <- nemetonshiny:::sondes_tableau(t, nemetonshiny:::get_i18n("fr"))
  expect_equal(sub(" - .*$", "", tab[[1]]), c("B1", "B2"))
})

# --- 9. Option AlphaEarth respectee (decision de Pascal du 2026-11-26) -------------------

test_that("option eteinte + cache present : message, aucun appel au coeur, pas de table", {
  appele <- FALSE
  testthat::local_mocked_bindings(
    get_app_options = function() list(language = "fr"),
    project_aef_active = function(metadata) FALSE,
    .package = "nemetonshiny")
  testthat::local_mocked_bindings(
    probe_indicator = function(...) { appele <<- TRUE; .sd_table() }, .package = "nemeton")
  notes <- character(0)
  testthat::local_mocked_bindings(
    showNotification = function(ui, ...) { notes <<- c(notes, as.character(ui)); invisible("id") },
    removeNotification = function(...) invisible(NULL), .package = "shiny")
  i18n <- nemetonshiny:::get_i18n("fr")
  projet <- .sd_projet(TRUE)
  expect_false(is.null(nemetonshiny:::sondes_embedding(projet$indicators_sf, projet$path)))
  shiny::testServer(nemetonshiny:::mod_sondes_embedding_server,
                    args = list(app_state = .sd_state(projet)), {
    expect_match(output$intro$html, "Activez AlphaEarth dans Sources", fixed = TRUE)
    session$setInputs(run = 1L)
    .sd_attendre(session, 5L)
    expect_false(appele)
    expect_true(i18n$t("sd_aef_eteint") %in% notes)
    expect_null(output$tableau)
    expect_equal(session$returned$status(), "initial")
  })
})

test_that("option active + cache present : le coeur est appele", {
  .sd_mocks()   # project_aef_active = TRUE
  appele <- 0L
  testthat::local_mocked_bindings(
    probe_indicator = function(...) { appele <<- appele + 1L; .sd_table() }, .package = "nemeton")
  shiny::testServer(nemetonshiny:::mod_sondes_embedding_server,
                    args = list(app_state = .sd_state(.sd_projet(TRUE))), {
    session$setInputs(run = 1L)
    .sd_attendre(session)
    expect_equal(appele, 1L)
    expect_match(output$tableau, "C2 - ", fixed = TRUE)
  })
})

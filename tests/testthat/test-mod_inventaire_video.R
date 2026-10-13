# Tests du sous-onglet Terrain > Inventaire video (brief groma 2026-10-12).
# Les fonctions coeur sont simulees au niveau des enveloppes `video_*` du
# service : le module se teste meme avec un coeur qui ne les exporte pas encore.

.fixture_video <- function() {
  testthat::test_path("fixtures", "inventaire_video", "inventaire_video.gpkg")
}

.plan_video <- function() {
  sf::st_sf(
    plot_id = c("P001", "P002"),
    geometry = sf::st_sfc(sf::st_point(c(900250, 6500250)),
                          sf::st_point(c(900750, 6500750)), crs = 2154))
}

.etat_video <- function(lang = "fr") {
  shiny::reactiveValues(
    current_project = list(id = "p_video", metadata = list()),
    language = lang)
}

test_that("l'UI expose les trois blocs et leurs commandes", {
  local_mocked_bindings(get_app_options = function() list(language = "fr"))
  html <- as.character(mod_inventaire_video_ui("iv"))
  for (id in c("iv-rayon", "iv-seuil", "iv-unite", "iv-declinaison", "iv-lidar",
               "iv-generer", "iv-actualiser", "iv-gpkg_terrain", "iv-recuperer",
               "iv-gpkg_resultats", "iv-integrer")) {
    expect_match(html, id, fixed = TRUE)
  }
  expect_match(html, "Pr\u00e9parer", fixed = TRUE)
})

test_that("aucun texte litteral : toutes les cles utilisees existent en FR et EN", {
  fichiers <- chemin_source("R", c("mod_inventaire_video.R",
                                   "service_inventaire_video.R"))
  skip_sans_sources(fichiers)
  code <- paste(unlist(lapply(fichiers, readLines)), collapse = "\n")
  cles <- c(regmatches(code, gregexpr('\\$t\\("[a-z0-9_]+"\\)', code))[[1]],
            regmatches(code, gregexpr('"video_[a-z0-9_]+"', code))[[1]])
  cles <- unique(gsub('^\\$t\\("|"\\)$|"', "", cles))
  expect_gt(length(cles), 40)
  expect_length(setdiff(cles, names(TRANSLATIONS)), 0)
  for (k in grep("^video_|^tab_terrain_video$", names(TRANSLATIONS), value = TRUE)) {
    expect_true(nzchar(TRANSLATIONS[[k]]$fr) && nzchar(TRANSLATIONS[[k]]$en), info = k)
    expect_lte(nchar(k), 30)
  }
  # L'UI change de langue avec l'option : aucun libelle code en dur.
  local_mocked_bindings(get_app_options = function() list(language = "en"))
  html_en <- as.character(mod_inventaire_video_ui("iv"))
  expect_match(html_en, "Build the QField project", fixed = TRUE)
  expect_false(grepl("G\u00e9n\u00e9rer", html_en, fixed = TRUE))
})

test_that("video_args_generation transmet les reglages tels quels", {
  pp <- withr::local_tempdir()
  a <- video_args_generation(pp, .plan_video(), c("P002"), rayon_m = 15,
                             seuil_diam_cm = 10, unite_azimut = "gr",
                             declinaison_deg = 1.5, avec_lidar = TRUE,
                             dem = "/x/mnt.tif", chm = "/x/mnh.tif", lang = "en")
  expect_identical(a$plot_ids, "P002")
  expect_identical(a[c("rayon_m", "seuil_diam_cm", "unite_azimut",
                       "declinaison_deg", "avec_lidar", "dem", "chm", "lang")],
                   list(rayon_m = 15, seuil_diam_cm = 10, unite_azimut = "gr",
                        declinaison_deg = 1.5, avec_lidar = TRUE,
                        dem = "/x/mnt.tif", chm = "/x/mnh.tif", lang = "en"))
  expect_identical(a$output_dir, video_dossier(pp))
  # Le coeur resout le MNT manquant depuis le projet.
  expect_identical(a$project_path, pp)
  # Sans MNH : pas de LiDAR, quoi que dise la case.
  b <- video_args_generation(pp, .plan_video(), "P001", declinaison_deg = 0,
                             avec_lidar = TRUE, dem = "/x/mnt.tif", chm = NULL)
  expect_false(b$avec_lidar)
  expect_null(b$dem)
  # Ce qui manque est rendu comme cle i18n.
  expect_identical(video_args_generation(pp, .plan_video(), "P001"),
                   "video_declinaison_requise")
  expect_identical(video_args_generation(pp, NULL, "P001", declinaison_deg = 0),
                   "video_sans_plan")
  expect_identical(video_args_generation(NULL, .plan_video(), "P001"),
                   "field_ingest_no_project")
})

test_that("Preparer : le formulaire arrive a la generation, et le .qgz est servi", {
  pp <- withr::local_tempdir()
  qgz <- file.path(pp, "inventaire_video.qgz")
  writeLines("qgz", qgz)
  recu <- NULL
  local_mocked_bindings(
    get_project_path = function(id) pp,
    .project_chm = function(id) NULL,
    video_placettes_plan = function(project_path) .plan_video(),
    video_coeur_disponible = function() TRUE,
    video_lancer_generation = function(tache, args, dev_path = NULL) recu <<- args
  )
  shiny::testServer(mod_inventaire_video_server, args = list(app_state = .etat_video()), {
    session$setInputs(placettes = c("P001", "P002"), rayon = 18, seuil = 10,
                      unite = "gr", declinaison = -0.4, lidar = TRUE)
    session$setInputs(generer = 1)
    expect_identical(recu$plot_ids, c("P001", "P002"))
    expect_identical(recu$rayon_m, 18)
    expect_identical(recu$seuil_diam_cm, 10)
    expect_identical(recu$unite_azimut, "gr")
    expect_identical(recu$declinaison_deg, -0.4)
    # Pas de MNH dans le projet : la case cochee ne suffit pas.
    expect_false(recu$avec_lidar)

    expect_null(output$telechargement)
    rv <- session$getReturned()
    rv$qgz <- qgz
    session$flushReact()
    expect_match(output$telechargement$html, "Download|T\u00e9l\u00e9charger")
  })
})

test_that("Preparer : sans declinaison, rien n'est lance", {
  pp <- withr::local_tempdir()
  lance <- FALSE
  local_mocked_bindings(
    get_project_path = function(id) pp,
    .project_chm = function(id) NULL,
    video_placettes_plan = function(project_path) .plan_video(),
    video_coeur_disponible = function() TRUE,
    video_lancer_generation = function(...) lance <<- TRUE
  )
  shiny::testServer(mod_inventaire_video_server, args = list(app_state = .etat_video()), {
    session$setInputs(placettes = "P001", declinaison = NA, generer = 1)
    expect_false(lance)
  })
})

test_that("video_tableau_etats croise le serveur et l'etat local", {
  srv <- data.frame(plot_id = c("P001", "P002", "P003"),
                    etat = c("provisoire", "envoye", "definitif"),
                    maj = c("2026-10-01", "2026-10-02", "2026-10-03"),
                    verdict = c(NA, NA, "conforme"))
  local <- c(P001 = "envoye", P004 = "a_faire")
  t <- video_tableau_etats(srv, local)
  expect_named(t, c("plot_id", "etat", "date", "verdict", "n_arbres", "alertes"))
  expect_identical(t$plot_id, c("P001", "P002", "P003", "P004"))
  # Le plus avance des deux gagne.
  expect_identical(t$etat, c("envoye", "envoye", "definitif", "a_faire"))
  expect_identical(t$date[1:3], srv$maj)
  expect_identical(video_tableau_etats(NULL)$plot_id, character(0))
})

test_that("Suivre : le tableau reflete les quatre etats", {
  withr::local_envvar(GROMA_URL = "https://groma.test/", GROMA_JETON = "secret")
  pp <- withr::local_tempdir()
  local_mocked_bindings(
    get_project_path = function(id) pp,
    .project_chm = function(id) NULL,
    video_placettes_plan = function(project_path) NULL,
    video_etat_serveur = function(url, jeton) {
      expect_identical(url, "https://groma.test")
      expect_identical(jeton, "secret")
      data.frame(plot_id = sprintf("P00%d", 1:4),
                 etat = c("a_faire", "provisoire", "envoye", "definitif"),
                 verdict = c(NA, NA, "a_verifier", "conforme"),
                 n_arbres = c(NA, 12L, 14L, 15L),
                 alertes = c(NA, NA, "ecart_dbh", NA))
    }
  )
  shiny::testServer(mod_inventaire_video_server, args = list(app_state = .etat_video()), {
    session$setInputs(actualiser = 1)
    html <- output$etats$html
    for (lib in c("\u00c0 faire", "Provisoire", "Envoy\u00e9e", "D\u00e9finitive",
                  "Conforme", "\u00c0 v\u00e9rifier", "ecart_dbh")) {
      expect_match(html, lib, fixed = TRUE)
    }
    # Le jeton ne s'affiche jamais.
    expect_false(grepl("secret", output$serveur_info$html, fixed = TRUE))
  })
})

test_that("Suivre : une erreur reseau donne un message i18n, pas un crash", {
  withr::local_envvar(GROMA_URL = "https://groma.test", GROMA_JETON = "secret")
  pp <- withr::local_tempdir()
  messages <- character(0)
  local_mocked_bindings(
    showNotification = function(ui, ...) messages <<- c(messages, as.character(ui)),
    .package = "shiny")
  local_mocked_bindings(
    get_project_path = function(id) pp,
    .project_chm = function(id) NULL,
    video_placettes_plan = function(project_path) NULL,
    video_etat_serveur = function(url, jeton) stop("Could not resolve host")
  )
  shiny::testServer(mod_inventaire_video_server, args = list(app_state = .etat_video()), {
    session$setInputs(actualiser = 1)
    expect_null(session$getReturned()$etats)
  })
  expect_true(any(grepl("Serveur Groma injoignable : Could not resolve host",
                        messages, fixed = TRUE)))
})

test_that("Suivre : sans serveur configure, aucun appel", {
  withr::local_envvar(GROMA_URL = "", GROMA_JETON = "")
  pp <- withr::local_tempdir()
  appele <- FALSE
  local_mocked_bindings(
    get_project_path = function(id) pp,
    .project_chm = function(id) NULL,
    video_placettes_plan = function(project_path) NULL,
    video_etat_serveur = function(url, jeton) appele <<- TRUE
  )
  shiny::testServer(mod_inventaire_video_server, args = list(app_state = .etat_video()), {
    session$setInputs(actualiser = 1)
    expect_false(appele)
    expect_match(output$serveur_info$html, "GROMA_URL", fixed = TRUE)
  })
})

test_that("video_config_serveur : URL du projet ou de l'env, jeton de l'env seul", {
  withr::local_envvar(GROMA_URL = "https://env.test", GROMA_JETON = "j")
  expect_identical(video_config_serveur(NULL), list(url = "https://env.test", jeton = "j"))
  cfg <- video_config_serveur(list(groma_url = "https://projet.test/",
                                   groma_jeton = "fuite"))
  expect_identical(cfg, list(url = "https://projet.test", jeton = "j"))
})

test_that("Integrer : NDP du coeur persiste et projet recharge", {
  pp <- withr::local_tempdir()
  maj <- NULL
  recharge <- FALSE
  local_mocked_bindings(
    get_project_path = function(id) pp,
    .project_chm = function(id) NULL,
    video_placettes_plan = function(project_path) NULL,
    deny_if_readonly = function(app_state, ...) FALSE,
    video_integrer = function(gpkg, resultats, project_path) {
      list(validation = list(ok = TRUE, errors = data.frame(), warnings = data.frame()),
           a_completer = data.frame(plot_id = "P001", tree_id = "4"),
           a_verifier = data.frame(),
           numeros_modifies = data.frame(plot_id = "P001", uuid = "u1",
                                         ancien = "5", nouveau = "4"),
           classique_conserve = "P002", ndp = 3L)
    },
    update_project_metadata = function(id, updates, ...) maj <<- updates,
    load_project = function(id, ...) { recharge <<- TRUE; list(id = id) }
  )
  shiny::testServer(mod_inventaire_video_server, args = list(app_state = .etat_video()), {
    session$setInputs(gpkg_terrain = data.frame(
      name = "inventaire_video.gpkg", size = 1, type = "",
      datapath = .fixture_video()))
    session$setInputs(integrer = 1)
    expect_identical(maj$ndp_level, 3L)
    expect_identical(maj$field_data_source, "inventaire_video")
    expect_true(recharge)
    html <- output$bilan$html
    expect_match(html, "Inventaire classique conserv\u00e9 pour 1 placette", fixed = TRUE)
    expect_match(html, "P001 : 5 \u2192 4", fixed = TRUE)
    expect_match(html, "Arbres \u00e0 compl\u00e9ter (1)", fixed = TRUE)
  })
})

test_that("Integrer : coeur absent -> message, aucune ecriture", {
  pp <- withr::local_tempdir()
  maj <- NULL
  local_mocked_bindings(
    get_project_path = function(id) pp,
    .project_chm = function(id) NULL,
    video_placettes_plan = function(project_path) NULL,
    deny_if_readonly = function(app_state, ...) FALSE,
    .video_coeur = function(nom) NULL,
    update_project_metadata = function(id, updates, ...) maj <<- updates
  )
  shiny::testServer(mod_inventaire_video_server, args = list(app_state = .etat_video()), {
    session$setInputs(gpkg_terrain = data.frame(
      name = "inventaire_video.gpkg", size = 1, type = "",
      datapath = .fixture_video()))
    session$setInputs(integrer = 1)
    expect_null(maj)
    expect_false(file.exists(file.path(pp, "data", "field_data.gpkg")))
  })
})

test_that("integration reelle : P002 classique conserve, P001 video ajoute, NDP 3", {
  skip_if_not(video_coeur_disponible(), "nemeton sans inventaire video")
  pp <- withr::local_tempdir()
  dir.create(file.path(pp, "data"))
  fd <- file.path(pp, "data", "field_data.gpkg")
  # Inventaire classique de P002 deja present dans le projet.
  p2 <- sf::st_read(.fixture_video(), layer = "placettes", quiet = TRUE)
  p2 <- p2[p2$plot_id == "P002", c("plot_id", "visit_order", "type")]
  centre <- sf::st_coordinates(p2)[1, ]
  arbres <- sf::st_sf(
    plot_id = "P002", tree_id = c("1", "2", "3"), espece = "essence_chenaie",
    dbh_cm = c(31, 42, 27), h_m = c(21, 24, 19), statut = "vivant",
    geometry = sf::st_sfc(lapply(1:3, function(i)
      sf::st_point(centre + c(i * 3, i * 2))), crs = 2154))
  sf::st_write(p2, fd, layer = "placettes", quiet = TRUE)
  sf::st_write(arbres, fd, layer = "arbres", quiet = TRUE, append = TRUE)

  res <- video_integrer(.fixture_video(), NULL, pp)
  b <- video_bilan(res)
  expect_true(b$validation$ok)
  expect_identical(b$ndp, 3L)

  a <- sf::st_drop_geometry(sf::st_read(fd, layer = "arbres", quiet = TRUE))
  non_video <- if (is.null(a$source)) rep(TRUE, nrow(a))
               else !a$source %in% "inventaire_video"
  classiques <- a[a$plot_id == "P002" & non_video, ]
  expect_setequal(classiques$tree_id, c("1", "2", "3"))
  expect_equal(sort(classiques$dbh_cm), c(27, 31, 42))
  expect_true("P001" %in% a$plot_id)
  # Le GeoPackage integre sert ensuite d'etat local a " Suivre ".
  expect_true("P001" %in% names(video_etat_local(pp)))

  # Renvoi sur le terrain : le coeur refuse s'il n'y a rien a reprendre.
  skip_if_not(video_renvoi_disponible(), "nemeton sans renvoyer_inventaire_video")
  if (!video_a_renvoyer(b)) {
    expect_error(suppressMessages(video_renvoyer(res, pp)), "nothing to send back")
    # L'exemple est complet : on marque un arbre de P001 a completer.
    i <- which(res$arbres$plot_id == "P001")[1]
    res$arbres$etat_detection[i] <- "a_completer"
  }
  qgz <- suppressMessages(video_renvoyer(res, pp))
  expect_true(file.exists(qgz))
  expect_identical(dirname(qgz), normalizePath(video_dossier(pp), winslash = "/"))
  expect_true("P001" %in% attr(qgz, "plot_ids"))
  expect_gt(attr(qgz, "n_arbres"), 0)
})

test_that("video_a_renvoyer : seulement une integration valide avec des arbres a reprendre", {
  b <- list(validation = list(ok = TRUE), a_completer = data.frame(x = 1),
            a_verifier = data.frame())
  expect_true(video_a_renvoyer(b))
  b$validation$ok <- FALSE
  expect_false(video_a_renvoyer(b))
  b$validation$ok <- TRUE
  b$a_completer <- data.frame()
  expect_false(video_a_renvoyer(b))
  expect_false(video_a_renvoyer(NULL))
})

test_that("Renvoyer : le bouton apparait apres integration et sert le .qgz", {
  pp <- withr::local_tempdir()
  qgz <- file.path(pp, "inventaire_video_retour.qgz")
  writeLines("zip", qgz)
  recu <- NULL
  local_mocked_bindings(
    get_project_path = function(id) pp,
    .project_chm = function(id) NULL,
    video_placettes_plan = function(project_path) NULL,
    deny_if_readonly = function(app_state, ...) FALSE,
    video_renvoi_disponible = function() TRUE,
    video_integrer = function(gpkg, resultats, project_path) {
      list(validation = list(ok = TRUE, errors = data.frame(), warnings = data.frame()),
           a_completer = data.frame(plot_id = "P001", tree_id = "4"),
           a_verifier = data.frame(), ndp = NA_integer_, marqueur = "import")
    },
    video_renvoyer = function(import, project_path) {
      recu <<- import$marqueur
      structure(qgz, plot_ids = "P001", n_arbres = 1L)
    },
    update_project_metadata = function(id, updates, ...) NULL,
    load_project = function(id, ...) list(id = id)
  )
  shiny::testServer(mod_inventaire_video_server, args = list(app_state = .etat_video()), {
    expect_null(output$renvoi)
    session$setInputs(gpkg_terrain = data.frame(
      name = "inventaire_video.gpkg", size = 1, type = "",
      datapath = .fixture_video()))
    session$setInputs(integrer = 1)
    expect_match(output$renvoi$html, "Renvoyer sur le terrain", fixed = TRUE)
    session$setInputs(renvoyer = 1)
    expect_identical(recu, "import")
    expect_identical(rv$qgz_retour, qgz)
    expect_match(output$renvoi$html, "projet de retour", fixed = TRUE)
  })
})

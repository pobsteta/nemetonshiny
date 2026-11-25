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

.etat_video <- function(lang = "fr", metadata = list()) {
  shiny::reactiveValues(
    current_project = list(id = "p_video", metadata = metadata),
    language = lang)
}

test_that("l'UI expose les trois blocs et leurs commandes", {
  local_mocked_bindings(get_app_options = function() list(language = "fr"))
  html <- as.character(mod_inventaire_video_ui("iv"))
  # Rayon, diametre, unite et declinaison ont quitte la vue (mission 050).
  for (id in c("iv-params_rappel", "iv-cvat_info", "iv-carte_ui", "iv-lidar",
               "iv-generer", "iv-actualiser", "iv-gpkg_terrain", "iv-recuperer",
               "iv-gpkg_resultats", "iv-integrer")) {
    expect_match(html, id, fixed = TRUE)
  }
  expect_match(html, "Pr\u00e9parer", fixed = TRUE)
  for (id in c("iv-rayon", "iv-seuil", "iv-unite", "iv-declinaison")) {
    expect_false(grepl(sprintf('id="%s"', id), html, fixed = TRUE), info = id)
  }
})

test_that("aucun texte litteral : toutes les cles utilisees existent en FR et EN", {
  fichiers <- chemin_source("R", c("mod_inventaire_video.R",
                                   "service_inventaire_video.R",
                                   "mod_sources_config.R"))
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
  # Declinaison vide : facultative, transmise a NULL (Groma la calcule avec
  # WMM2025 ; le coeur ecrit NA, jamais 0).
  v <- video_args_generation(pp, .plan_video(), "P001")
  expect_true(is.list(v))
  expect_true("declinaison_deg" %in% names(v))
  expect_null(v$declinaison_deg)
  expect_null(video_args_generation(pp, .plan_video(), "P001",
                                    declinaison_deg = NA)$declinaison_deg)
  # Ce qui manque est rendu comme cle i18n.
  expect_identical(video_args_generation(pp, NULL, "P001", declinaison_deg = 0),
                   "video_sans_plan")
  expect_identical(video_args_generation(NULL, .plan_video(), "P001"),
                   "field_ingest_no_project")
})

test_that("le CVAT precalcule du projet est passe au coeur (cvat_path), sinon absent", {
  pp <- withr::local_tempdir()
  # Sans MNT ni CVAT : l'argument n'est pas passe.
  expect_null(.video_cvat_path(pp))
  a <- video_args_generation(pp, .plan_video(), "P001")
  expect_false("cvat_path" %in% names(a))
  # MNT sans CVAT a cote : toujours rien, et rien n'est calcule.
  acc <- file.path(pp, "cache", "accessibility", "emprise_500m")
  dir.create(acc, recursive = TRUE)
  mnt <- file.path(acc, "mnt_highres_1m.tif")
  file.create(mnt)
  expect_null(.video_cvat_path(pp))
  expect_false("cvat_path" %in% names(video_args_generation(pp, .plan_video(), "P001")))
  expect_identical(list.files(acc), "mnt_highres_1m.tif")
  # CVAT Byte a cote du MNT : chemin rendu et transmis tel quel.
  cvat <- file.path(acc, "mnt_highres_1m_CVAT_8bit_foretaccess.tif")
  file.create(cvat)
  expect_identical(.video_cvat_path(pp), cvat)
  b <- video_args_generation(pp, .plan_video(), "P001")
  expect_true("cvat_path" %in% names(b))
  expect_identical(b$cvat_path, cvat)
  # Le coeur installe expose bien l'argument (plancher 2026.11.37).
  skip_if_not("cvat_path" %in% names(formals(nemeton::creer_projet_inventaire_video)))
  expect_true(TRUE)
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
  etat <- .etat_video(metadata = list(video_params = list(
    rayon_m = 18, seuil_diam_cm = 10, unite_azimut = "gr", declinaison_deg = -0.4)))
  shiny::testServer(mod_inventaire_video_server, args = list(app_state = etat), {
    session$setInputs(placettes = c("P001", "P002"), lidar = TRUE)
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

test_that("Preparer : sans declinaison, la generation part (Groma la calcule)", {
  pp <- withr::local_tempdir()
  recu <- NULL
  local_mocked_bindings(
    get_project_path = function(id) pp,
    .project_chm = function(id) NULL,
    video_placettes_plan = function(project_path) .plan_video(),
    video_coeur_disponible = function() TRUE,
    video_lancer_generation = function(tache, args, ...) recu <<- args
  )
  shiny::testServer(mod_inventaire_video_server, args = list(app_state = .etat_video()), {
    session$setInputs(placettes = "P001", generer = 1)
    expect_false(is.null(recu))
    expect_true("declinaison_deg" %in% names(recu))
    expect_null(recu$declinaison_deg)
  })
})

test_that("video_tableau_etats croise le serveur et l'etat local", {
  srv <- data.frame(plot_id = c("P001", "P002", "P003"),
                    etat = c("provisoire", "envoye", "definitif"),
                    maj = c("2026-10-01", "2026-10-02", "2026-10-03"),
                    verdict = c(NA, NA, "conforme"))
  local <- c(P001 = "envoye", P004 = "a_faire")
  t <- video_tableau_etats(srv, local)
  expect_named(t, c("plot_id", "etat", "date", "verdict", "n_arbres", "alertes",
                    "terrain_envoye_le"))
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
    video_integrer = function(gpkg, resultats, project_path, terrain = NULL) {
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
    video_integrer = function(gpkg, resultats, project_path, terrain = NULL) {
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

test_that("le projet QField est livre en zip avec son plugin Groma", {
  d <- withr::local_tempdir()
  qgz <- file.path(d, "inventaire_video.qgz")
  writeLines("qgz", qgz)
  # Coeur < 2026.10.7 : pas de .qml, le .qgz part seul.
  expect_identical(video_fichiers_livraison(qgz), qgz)
  writeLines("qml", file.path(d, "inventaire_video.qml"))
  z <- file.path(d, "sortie.zip")
  video_zip_livraison(qgz, z)
  expect_setequal(utils::unzip(z, list = TRUE)$Name,
                  c("inventaire_video.qgz", "inventaire_video.qml"))
  expect_identical(video_nom_livraison(qgz, "x.qgz"), "inventaire_video.zip")
  expect_identical(video_nom_livraison(NULL, "retour.qgz"), "retour.zip")
  expect_error(video_zip_livraison(file.path(d, "absent.qgz"), z))
})

test_that("le dossier ouvrable du projet est livre a la place du .qgz", {
  d <- withr::local_tempdir()
  qgz <- file.path(d, "inventaire_video.qgz")
  writeLines("qgz", qgz)
  writeLines("qml", file.path(d, "inventaire_video.qml"))
  # Coeur >= 2026.10.12 : dossier <name>/ a cote du .qgz.
  dossier <- file.path(d, "inventaire_video")
  dir.create(file.path(dossier, "rasters"), recursive = TRUE)
  for (f in c("inventaire_video.qgs", "inventaire_video.gpkg",
              "inventaire_video.qml", "rasters/P1_chm.tif")) {
    writeLines("x", file.path(dossier, f))
  }
  wd <- getwd()
  z <- file.path(d, "sortie.zip")
  expect_identical(video_zip_livraison(qgz, z), z)
  expect_identical(getwd(), wd)
  noms <- utils::unzip(z, list = TRUE)$Name
  fichiers <- noms[!grepl("/$", noms)]
  expect_setequal(fichiers, c("inventaire_video/inventaire_video.qgs",
                              "inventaire_video/inventaire_video.gpkg",
                              "inventaire_video/inventaire_video.qml",
                              "inventaire_video/rasters/P1_chm.tif"))
  # Le .qgz seul, decompresse, ouvrirait des couches introuvables.
  expect_false(any(grepl("\\.qgz$", noms)))
})

test_that("les informations de validation ne comptent pas comme avertissements", {
  w <- data.frame(plot_id = c("P1", "P2"), message = c("a", "b"),
                  severity = c("info", "warning"))
  s <- video_separer_infos(w)
  expect_identical(s$avertissements$plot_id, "P2")
  expect_identical(s$infos$plot_id, "P1")
  # Sans colonne severity : tout reste avertissement.
  expect_identical(video_separer_infos(w[, 1:2])$avertissements, w[, 1:2])
  i18n <- get_i18n("fr")
  bilan <- list(validation = list(ok = TRUE, errors = data.frame(),
                                  warnings = w[1, ]),
                numeros_modifies = data.frame(), classique_conserve = 0L,
                ndp = NA)
  html <- as.character(video_bilan_ui(bilan, i18n))
  expect_match(html, "alert alert-success", fixed = TRUE)
  expect_match(html, "Informations", fixed = TRUE)
})

# ---- Envoi au telephone via le serveur Groma (brief 2026-10-17) ------------

# Balise du bouton " Envoyer " seule : le downloadButton voisin porte lui
# aussi une classe `disabled` tant que Shiny ne l'a pas arme.
.bouton_envoyer <- function(html) {
  regmatches(html, regexpr('<button[^>]*id="[^"]*envoyer"[^>]*>', html))
}

.etat_video_serveur <- function() {
  shiny::reactiveValues(
    current_project = list(id = "p_video",
                           metadata = list(groma_url = "https://groma.test/")),
    language = "fr")
}

test_that("Envoyer : bouton absent sans .qgz, desactive sans serveur", {
  pp <- withr::local_tempdir()
  qgz <- file.path(pp, "inventaire_video.qgz"); writeLines("qgz", qgz)
  local_mocked_bindings(
    get_project_path = function(id) pp,
    .project_chm = function(id) NULL,
    video_placettes_plan = function(project_path) .plan_video(),
    video_envoi_disponible = function() TRUE)
  withr::local_envvar(GROMA_URL = "", GROMA_JETON = "")
  shiny::testServer(mod_inventaire_video_server, args = list(app_state = .etat_video()), {
    expect_null(output$telechargement)
    # Premier flush : l'observateur de changement de projet remet rv$qgz a NULL.
    session$flushReact()
    rv <- session$getReturned()
    rv$qgz <- qgz
    session$flushReact()
    html <- output$telechargement$html
    expect_match(html, "Envoyer au t\u00e9l\u00e9phone", fixed = TRUE)
    expect_match(.bouton_envoyer(html), "disabled", fixed = TRUE)
    expect_match(html, "Renseigner l&#39;adresse et le jeton du serveur Groma", fixed = TRUE)
  })
})

test_that("Envoyer : un clic publie le .qgz avec la configuration du serveur", {
  pp <- withr::local_tempdir()
  qgz <- file.path(pp, "inventaire_video.qgz"); writeLines("qgz", qgz)
  recu <- NULL
  local_mocked_bindings(
    get_project_path = function(id) pp,
    .project_chm = function(id) NULL,
    video_placettes_plan = function(project_path) .plan_video(),
    video_envoi_disponible = function() TRUE,
    video_lancer_envoi = function(tache, qgz, cfg, dev_path = NULL)
      recu <<- list(qgz = qgz, cfg = cfg))
  withr::local_envvar(GROMA_URL = "", GROMA_JETON = "secret")
  shiny::testServer(mod_inventaire_video_server,
                    args = list(app_state = .etat_video_serveur()), {
    # Premier flush : l'observateur de changement de projet remet rv$qgz a NULL.
    session$flushReact()
    rv <- session$getReturned()
    rv$qgz <- qgz
    session$flushReact()
    b <- .bouton_envoyer(output$telechargement$html)
    expect_length(b, 1L)
    expect_false(grepl("disabled", b, fixed = TRUE))
    session$setInputs(envoyer = 1)
    expect_identical(recu$qgz, qgz)
    expect_identical(recu$cfg, list(url = "https://groma.test", jeton = "secret"))
  })
})

test_that("Envoyer : sans serveur, le clic n'envoie rien", {
  pp <- withr::local_tempdir()
  qgz <- file.path(pp, "inventaire_video.qgz"); writeLines("qgz", qgz)
  lance <- FALSE
  local_mocked_bindings(
    get_project_path = function(id) pp,
    .project_chm = function(id) NULL,
    video_placettes_plan = function(project_path) .plan_video(),
    video_envoi_disponible = function() TRUE,
    video_lancer_envoi = function(...) lance <<- TRUE)
  withr::local_envvar(GROMA_URL = "", GROMA_JETON = "")
  shiny::testServer(mod_inventaire_video_server, args = list(app_state = .etat_video()), {
    # Premier flush : l'observateur de changement de projet remet rv$qgz a NULL.
    session$flushReact()
    rv <- session$getReturned()
    rv$qgz <- qgz
    session$setInputs(envoyer = 1)
    expect_false(lance)
  })
})

test_that("Envoyer : une erreur au lancement est affichee sans planter", {
  pp <- withr::local_tempdir()
  qgz <- file.path(pp, "inventaire_video.qgz"); writeLines("qgz", qgz)
  local_mocked_bindings(
    get_project_path = function(id) pp,
    .project_chm = function(id) NULL,
    video_placettes_plan = function(project_path) .plan_video(),
    video_envoi_disponible = function() TRUE,
    video_lancer_envoi = function(...) stop("serveur injoignable"))
  withr::local_envvar(GROMA_URL = "", GROMA_JETON = "secret")
  shiny::testServer(mod_inventaire_video_server,
                    args = list(app_state = .etat_video_serveur()), {
    # Premier flush : l'observateur de changement de projet remet rv$qgz a NULL.
    session$flushReact()
    rv <- session$getReturned()
    rv$qgz <- qgz
    expect_no_error(session$setInputs(envoyer = 1))
    expect_null(rv$envoi)
  })
})

test_that("le message d'envoi donne la version, ou la raison de l'echec", {
  i18n <- get_i18n("fr")
  ok <- video_message_envoi(list(projet_id = "p", version = 3L, taille = 10), i18n)
  expect_identical(ok$type, "message")
  expect_match(ok$message, "(version 3)", fixed = TRUE)
  expect_match(ok$message, "Projets du serveur", fixed = TRUE)
  ko <- video_message_envoi(simpleError("jeton refus\u00e9"), i18n)
  expect_identical(ko$type, "error")
  expect_match(ko$message, "jeton refus\u00e9", fixed = TRUE)
  # Chaine d'erreurs rlang : seule la tete est affichee.
  inj <- video_message_envoi(simpleError(
    "Groma server unreachable at <http://x>.\nCaused by error in `curl`:\n! Couldn't connect"), i18n)
  expect_identical(inj$message, "Erreur Serveur Groma injoignable (http://x).")
  # Version absente du serveur (NA) : pas de " NA " dans le message.
  sans <- video_message_envoi(list(projet_id = "p", version = NA, taille = 1), i18n)
  expect_match(sans$message, "(version ?)", fixed = TRUE)
  absent <- tryCatch(video_envoyer_projet("x.qgz", "u", "j"), error = function(e) e)
  if (inherits(absent, "video_coeur_absent")) {
    expect_match(video_message_envoi(absent, i18n)$message, "groma_envoyer_projet",
                 fixed = TRUE)
  }
})

test_that("les erreurs connues du coeur sont traduites, les autres gardees", {
  fr <- get_i18n("fr"); en <- get_i18n("en")
  m <- function(txt, i18n = fr) video_message_envoi(simpleError(txt), i18n)$message
  expect_identical(m("Groma server refused the token (HTTP 403)."),
                   "Erreur Le serveur Groma a refus\u00e9 le jeton (HTTP 403). V\u00e9rifier GROMA_JETON.")
  expect_match(m("Project too large for the Groma server (HTTP 413)."),
               "trop volumineux", fixed = TRUE)
  expect_match(m("A file was altered during the upload: the server's checksum differs (HTTP 422)."),
               "Relancer l'envoi", fixed = TRUE)
  expect_match(m("Groma server unreachable at <http://127.0.0.1:1>.", en),
               "Groma server unreachable (http://127.0.0.1:1).", fixed = TRUE)
  # Message inconnu : remonte tel quel, sans traduction.
  expect_identical(m("HTTP 500 Internal Server Error."),
                   "Erreur HTTP 500 Internal Server Error.")
  # Chaque motif renvoie a une cle existante.
  for (cle in VIDEO_ERREURS_ENVOI) expect_false(identical(fr$t(cle), cle))
})

test_that("le bouton reste inactif tant que le coeur ne sait pas envoyer", {
  local_mocked_bindings(video_envoi_disponible = function() FALSE)
  expect_identical(video_raison_envoi_inactif(list(url = "u", jeton = "j")),
                   "video_envoi_coeur_absent")
  local_mocked_bindings(video_envoi_disponible = function() TRUE)
  expect_identical(video_raison_envoi_inactif(list(url = "u", jeton = "")),
                   "video_envoi_sans_serveur")
  expect_null(video_raison_envoi_inactif(list(url = "u", jeton = "j")))
})

test_that("un changement de projet efface l'etat de l'envoi", {
  pp1 <- withr::local_tempdir(); pp2 <- withr::local_tempdir()
  etat <- .etat_video()
  local_mocked_bindings(
    get_project_path = function(id) if (identical(id, "p2")) pp2 else pp1,
    .project_chm = function(id) NULL,
    video_placettes_plan = function(project_path) NULL)
  shiny::testServer(mod_inventaire_video_server, args = list(app_state = etat), {
    session$flushReact()
    rv <- session$getReturned()
    rv$envoi <- list(version = 1L)
    etat$current_project <- list(id = "p2", metadata = list())
    session$flushReact()
    expect_null(rv$envoi)
  })
})

# ---- Schema Groma 1.6.0 (mission 030, coeur nemeton 2026.11.29) --------------

test_that("le rayon propose par defaut est de 15 m", {
  expect_identical(project_video_params(NULL)$rayon_m, 15)
  expect_false(identical(project_video_params(NULL)$rayon_m, 20))
  pp <- withr::local_tempdir()
  a <- video_args_generation(pp, .plan_video(), "P001")
  expect_identical(a$rayon_m, 15)
})

test_that("sans_paquet est un etat du cycle, entre a_faire et provisoire", {
  expect_identical(.VIDEO_ETATS,
                   c("a_faire", "sans_paquet", "provisoire", "envoye", "definitif"))
  srv <- data.frame(plot_id = c("P1", "P2", "P3"),
                    etat = c("sans_paquet", "sans_paquet", "a_faire"),
                    terrain_envoye_le = c("2026-11-15T13:25:00Z", NA, NA))
  t <- video_tableau_etats(srv, c(P2 = "provisoire", P3 = "a_faire"))
  # Le local plus avance gagne ; sans_paquet bat a_faire.
  expect_identical(t$etat, c("sans_paquet", "provisoire", "a_faire"))
  expect_identical(t$terrain_envoye_le, c("2026-11-15T13:25:00Z", NA, NA))
  # Serveur sans la colonne (serveur anterieur) : NA, pas d'erreur.
  expect_true(all(is.na(video_tableau_etats(srv[, 1:2])$terrain_envoye_le)))
})

test_that("terrain_envoye_le s'affiche en heure de Paris, vide si NA", {
  # Hiver UTC+1, ete UTC+2 : le fuseau est applique, pas un decalage fixe.
  expect_identical(video_terrain_recu("2026-11-15T13:25:00Z"), "15/11/2026 14:25")
  expect_identical(video_terrain_recu("2026-07-01T10:00:00Z"), "01/07/2026 12:00")
  expect_identical(video_terrain_recu("2026-07-01T10:00:00+00:00"), "01/07/2026 12:00")
  expect_identical(video_terrain_recu(c(NA, "n'importe quoi")), c("", ""))
})

test_that("Suivre : saisie recue sans video et date du terrain affichees", {
  withr::local_envvar(GROMA_URL = "https://groma.test/", GROMA_JETON = "secret")
  pp <- withr::local_tempdir()
  local_mocked_bindings(
    get_project_path = function(id) pp,
    .project_chm = function(id) NULL,
    video_placettes_plan = function(project_path) NULL,
    video_etat_serveur = function(url, jeton) {
      data.frame(plot_id = c("P001", "P002"),
                 etat = c("sans_paquet", "a_faire"),
                 terrain_envoye_le = c("2026-11-15T13:25:00Z", NA))
    }
  )
  shiny::testServer(mod_inventaire_video_server, args = list(app_state = .etat_video()), {
    session$setInputs(actualiser = 1)
    html <- output$etats$html
    expect_match(html, "Saisie reçue, sans vidéo", fixed = TRUE)
    expect_match(html, "Terrain reçu le 15/11/2026 14:25", fixed = TRUE)
    # Une seule ligne porte une date de terrain.
    expect_equal(lengths(regmatches(html, gregexpr("Terrain reçu le", html))), 1L)
  })
})

test_that("video_integrer passe terrain au coeur ; la copie QField est facultative", {
  pp <- withr::local_tempdir()
  recu <- NULL
  local_mocked_bindings(.video_appeler = function(nom, ...) {
    recu <<- c(list(nom = nom), list(...))
    list(ndp = 2L)
  })
  zip <- file.path(pp, "P001_terrain.zip")
  res <- video_integrer(NULL, NULL, pp, terrain = zip)
  expect_identical(recu$nom, "importer_inventaire_video")
  expect_identical(recu$terrain, zip)
  expect_true("gpkg" %in% names(recu))
  expect_null(recu$gpkg)
  expect_identical(recu$field_data_gpkg, file.path(pp, "data", "field_data.gpkg"))
  expect_identical(res$field_data_gpkg, recu$field_data_gpkg)
  # Pas de copie de QField : aucun terrain_integre.gpkg fabrique.
  expect_false(file.exists(file.path(video_dossier(pp), "terrain_integre.gpkg")))
  # Avec la copie : elle est gardee comme avant.
  video_integrer(.fixture_video(), NULL, pp, terrain = zip)
  expect_true(file.exists(file.path(video_dossier(pp), "terrain_integre.gpkg")))
  # Sans terrain : l'argument n'est pas transmis (comportement d'avant).
  video_integrer(.fixture_video(), NULL, pp)
  expect_false("terrain" %in% names(recu))
  # Ni copie ni saisie : refus net.
  expect_error(video_integrer(NULL, NULL, pp), "QField copy")
})

test_that("video_telecharger restreint les fichiers seulement sur demande", {
  d <- withr::local_tempdir()
  recu <- NULL
  local_mocked_bindings(.video_appeler = function(nom, ...) {
    recu <<- list(...)
    character(0)
  })
  video_telecharger("u", "j", "P1", file.path(d, "a"))
  expect_false("fichiers" %in% names(recu))
  video_telecharger("u", "j", "P1", file.path(d, "b"), fichiers = "terrain.zip")
  expect_identical(recu$fichiers, "terrain.zip")
})

test_that("Integrer : terrain.zip telecharge puis passe a l'import sans gpkg", {
  withr::local_envvar(GROMA_URL = "https://groma.test/", GROMA_JETON = "secret")
  pp <- withr::local_tempdir()
  appels <- list()
  recu <- NULL
  local_mocked_bindings(
    get_project_path = function(id) pp,
    .project_chm = function(id) NULL,
    video_placettes_plan = function(project_path) NULL,
    deny_if_readonly = function(app_state, ...) FALSE,
    video_etat_serveur = function(url, jeton) {
      data.frame(plot_id = c("P001", "P002", "P003"),
                 etat = c("definitif", "sans_paquet", "a_faire"),
                 terrain_envoye_le = c("2026-11-15T13:25:00Z",
                                       "2026-11-15T14:00:00Z", NA))
    },
    video_telecharger = function(url, jeton, plot_ids, dossier, fichiers = NULL,
                                 etat = NULL) {
      appels[[length(appels) + 1L]] <<- list(plot_ids = plot_ids, fichiers = fichiers,
                                             dossier = dossier, etat = etat)
      f <- file.path(dossier, paste0(plot_ids, "_terrain.zip"))
      dir.create(dossier, showWarnings = FALSE)
      if (is.null(fichiers)) f <- c(f, file.path(dossier, paste0(plot_ids, "_resultat.gpkg")))
      file.create(f)
      f
    },
    video_integrer = function(gpkg, resultats, project_path, terrain = NULL) {
      recu <<- list(gpkg = gpkg, resultats = resultats, terrain = terrain)
      list(validation = list(ok = TRUE, errors = data.frame(), warnings = data.frame()),
           ndp = NA_integer_)
    },
    update_project_metadata = function(id, updates, ...) NULL,
    load_project = function(id, ...) list(id = id)
  )
  shiny::testServer(mod_inventaire_video_server, args = list(app_state = .etat_video()), {
    session$setInputs(recuperer = 1)
    # Definitive : les quatre fichiers ; saisie seule : terrain.zip uniquement.
    expect_length(appels, 2L)
    expect_identical(appels[[1]]$plot_ids, "P001")
    expect_null(appels[[1]]$fichiers)
    expect_identical(appels[[2]]$plot_ids, "P002")
    expect_identical(appels[[2]]$fichiers, "terrain.zip")
    # Dossier stable sous le projet, tableau brut du serveur transmis.
    expect_identical(appels[[1]]$dossier, file.path(pp, "data", "inventaire_video", "serveur"))
    expect_identical(appels[[2]]$dossier, appels[[1]]$dossier)
    expect_identical(appels[[1]]$etat$terrain_envoye_le[1], "2026-11-15T13:25:00Z")
    expect_identical(appels[[2]]$etat$plot_id, c("P001", "P002", "P003"))
    session$setInputs(integrer = 1)
    expect_null(recu$gpkg)
    expect_identical(basename(recu$terrain), c("P001_terrain.zip", "P002_terrain.zip"))
    expect_identical(basename(recu$resultats), "P001_resultat.gpkg")
  })
})

test_that("Integrer : ni copie de QField ni saisie de Groma -> rien n'est lance", {
  pp <- withr::local_tempdir()
  appele <- FALSE
  local_mocked_bindings(
    get_project_path = function(id) pp,
    .project_chm = function(id) NULL,
    video_placettes_plan = function(project_path) NULL,
    deny_if_readonly = function(app_state, ...) FALSE,
    video_integrer = function(...) { appele <<- TRUE; list() }
  )
  shiny::testServer(mod_inventaire_video_server, args = list(app_state = .etat_video()), {
    session$setInputs(integrer = 1)
    expect_false(appele)
  })
})

test_that("les trois motifs d'alerte sont traduits, avec leur detail", {
  al <- data.frame(
    plot_id = c("P1", "P1", "P2", "P3"),
    motif = c("terrain_serveur_prefere", "saisie_qfield_ignoree",
              "centre_telephone", "autre_motif_du_coeur"),
    detail = c(NA, "essence = CHE", "precision_h_m = 6.5", NA))
  fr <- video_alertes_traduites(al, get_i18n("fr"))
  en <- video_alertes_traduites(al, get_i18n("en"))
  expect_named(fr, c("plot_id", "motif", "detail"))
  expect_match(fr$motif[1], "remplacée par la saisie de Groma", fixed = TRUE)
  expect_match(fr$motif[2], "seulement dans QField", fixed = TRUE)
  expect_match(fr$motif[3], "GNSS du téléphone", fixed = TRUE)
  expect_match(en$motif[1], "replaced by the Groma entry", fixed = TRUE)
  expect_match(en$motif[3], "phone GNSS", fixed = TRUE)
  expect_identical(fr$detail, c("", "essence = CHE", "precision_h_m = 6.5", ""))
  # Motif inconnu : etiquette generique + code, jamais perdu ni invente.
  expect_identical(fr$motif[4], "Autre alerte : autre_motif_du_coeur")
  # Coeur anterieur : pas de colonne detail, ni d'alertes du tout.
  expect_identical(video_alertes_traduites(al[, 1:2], get_i18n("fr"))$detail, rep("", 4))
  expect_identical(nrow(video_alertes_traduites(NULL, get_i18n("fr"))), 0L)
})

test_that("le bilan affiche les alertes d'import et leur detail", {
  res <- list(validation = list(ok = TRUE),
              alertes = data.frame(plot_id = "P2", motif = "centre_telephone",
                                   detail = "precision_h_m = 6.5"))
  b <- video_bilan(res)
  html <- as.character(video_bilan_ui(b, get_i18n("fr")))
  expect_match(html, "Alertes d'import (1)", fixed = TRUE)
  expect_match(html, "precision_h_m = 6.5", fixed = TRUE)
  expect_match(html, "GNSS du téléphone", fixed = TRUE)
  expect_false(grepl("centre_telephone", html, fixed = TRUE))
  # Sans alerte : aucun bloc.
  expect_false(grepl("Alertes d'import", as.character(
    video_bilan_ui(video_bilan(list(validation = list(ok = TRUE))), get_i18n("fr"))),
    fixed = TRUE))
})

test_that("un centre telephone est un avertissement, pas une erreur bloquante", {
  w <- data.frame(plot_id = "P2", field = "qualite_gnss",
                  message = "centre mesure au GNSS du telephone")
  b <- video_bilan(list(validation = list(ok = TRUE, warnings = w)))
  html <- as.character(video_bilan_ui(b, get_i18n("fr")))
  expect_match(html, "alert alert-warning", fixed = TRUE)
  expect_false(grepl("alert-danger", html, fixed = TRUE))
  expect_match(html, "qualite_gnss", fixed = TRUE)

  # Dans le module : l'import est persiste malgre l'avertissement.
  pp <- withr::local_tempdir()
  maj <- NULL
  local_mocked_bindings(
    get_project_path = function(id) pp,
    .project_chm = function(id) NULL,
    video_placettes_plan = function(project_path) NULL,
    deny_if_readonly = function(app_state, ...) FALSE,
    video_integrer = function(gpkg, resultats, project_path, terrain = NULL) {
      list(validation = list(ok = TRUE, errors = data.frame(), warnings = w),
           ndp = NA_integer_)
    },
    update_project_metadata = function(id, updates, ...) maj <<- updates,
    load_project = function(id, ...) list(id = id)
  )
  shiny::testServer(mod_inventaire_video_server, args = list(app_state = .etat_video()), {
    session$setInputs(gpkg_terrain = data.frame(
      name = "inventaire_video.gpkg", size = 1, type = "", datapath = .fixture_video()))
    session$setInputs(integrer = 1)
    expect_identical(maj$field_data_source, "inventaire_video")
  })
})

test_that("le coeur installe accepte l'argument terrain (plancher 2026.11.29)", {
  skip_if_not_installed("nemeton")
  skip_if_not(utils::packageVersion("nemeton") >= "2026.11.29",
              "coeur anterieur a 2026.11.29")
  expect_true("terrain" %in% names(formals(nemeton::importer_inventaire_video)))
})

# ---- Reprise de relecture #287 ------------------------------------------------

.ecrire_placettes <- function(chemin, ids, etats) {
  p <- sf::st_sf(plot_id = ids, etat_traitement = etats,
                 geometry = sf::st_sfc(lapply(seq_along(ids), function(i)
                   sf::st_point(c(900000 + i, 6500000))), crs = 2154))
  dir.create(dirname(chemin), recursive = TRUE, showWarnings = FALSE)
  sf::st_write(p, chemin, layer = "placettes", quiet = TRUE)
}

test_that("Suivre : l'etat local vient aussi de field_data.gpkg, en lecture seule", {
  pp <- withr::local_tempdir()
  expect_identical(video_etat_local(pp), character(0))
  # Saisie Groma seule : pas de terrain_integre.gpkg, mais field_data.gpkg.
  fd <- file.path(pp, "data", "field_data.gpkg")
  .ecrire_placettes(fd, c("P1", "P2"), c("definitif", "provisoire"))
  avant <- unname(tools::md5sum(fd))
  expect_identical(video_etat_local(pp), c(P1 = "definitif", P2 = "provisoire"))
  expect_identical(unname(tools::md5sum(fd)), avant)
  expect_false(file.exists(file.path(video_dossier(pp), "terrain_integre.gpkg")))
  # La copie QField integree gagne pour une placette presente des deux cotes.
  .ecrire_placettes(file.path(video_dossier(pp), "terrain_integre.gpkg"),
                    c("P2", "P3"), c("envoye", "provisoire"))
  expect_identical(video_etat_local(pp),
                   c(P2 = "envoye", P3 = "provisoire", P1 = "definitif"))
  # Un field_data.gpkg classique (sans etat_traitement) est ignore.
  fd2 <- withr::local_tempdir()
  p <- sf::st_sf(plot_id = "Z", geometry = sf::st_sfc(sf::st_point(c(0, 0)), crs = 2154))
  dir.create(file.path(fd2, "data"))
  sf::st_write(p, file.path(fd2, "data", "field_data.gpkg"), layer = "placettes", quiet = TRUE)
  expect_identical(video_etat_local(fd2), character(0))
})

test_that("video_telecharger : le repli ne ramasse que les fichiers de l'appel", {
  d <- withr::local_tempdir()
  # Fichiers d'un appel precedent, dans le meme dossier.
  file.create(file.path(d, c("P1_resultat.gpkg", "P1_terrain.zip", "P9_terrain.zip",
                             "P9_resultat.gpkg", "autre.txt")))
  local_mocked_bindings(.video_appeler = function(nom, ...) NULL)
  f <- video_telecharger("u", "j", "P1", d, fichiers = "terrain.zip")
  expect_identical(basename(f), "P1_terrain.zip")
  f <- video_telecharger("u", "j", c("P1", "P9"), d)
  expect_setequal(basename(f), c("P1_resultat.gpkg", "P1_terrain.zip",
                                 "P9_terrain.zip", "P9_resultat.gpkg"))
})

test_that("les motifs sans cle sont regroupes sous 'Autre alerte', jamais bruts", {
  al <- data.frame(plot_id = c("P1", "P1"), motif = c("ecart_dbh", "centre_telephone"),
                   detail = c(NA, "precision_h_m = 4"))
  fr <- video_alertes_traduites(al, get_i18n("fr"))
  en <- video_alertes_traduites(al, get_i18n("en"))
  expect_identical(fr$motif[1], "Autre alerte : ecart_dbh")
  expect_identical(en$motif[1], "Other alert: ecart_dbh")
  expect_match(fr$motif[2], "GNSS", fixed = TRUE)
  # Aucun motif ne s'affiche seul, sans etiquette.
  expect_false(any(fr$motif == "ecart_dbh"))
})

test_that("video_telecharger transmet etat au coeur, et seulement s'il est donne", {
  d <- withr::local_tempdir()
  recu <- NULL
  local_mocked_bindings(.video_appeler = function(nom, ...) {
    recu <<- list(...)
    character(0)
  })
  video_telecharger("u", "j", "P1", file.path(d, "a"))
  expect_false("etat" %in% names(recu))
  et <- data.frame(plot_id = "P1", terrain_envoye_le = "2026-11-15T13:25:00Z")
  video_telecharger("u", "j", "P1", file.path(d, "b"), etat = et)
  expect_identical(recu$etat, et)
})

test_that("video_dossier_serveur : stable sous le projet, temporaire sans projet", {
  pp <- withr::local_tempdir()
  d <- video_dossier_serveur(pp)
  expect_identical(d, file.path(pp, "data", "inventaire_video", "serveur"))
  expect_true(dir.exists(d))
  expect_identical(video_dossier_serveur(pp), d)
  sans <- video_dossier_serveur(NULL)
  expect_false(startsWith(sans, pp))
  expect_false(identical(video_dossier_serveur(NULL), sans))
})

test_that("video_bilan rend dbh_ruban, et l'ecran affiche la part par placette", {
  res <- list(validation = list(ok = TRUE, dbh_ruban = data.frame(
    plot_id = c("P1", "P2"), n_arbres = c(10L, 8L), n_ruban = c(4L, NA),
    part_ruban = c(0.4, NA))))
  b <- video_bilan(res)
  expect_identical(b$dbh_ruban$n_ruban, c(4L, NA))
  html <- as.character(video_bilan_ui(b, get_i18n("fr")))
  expect_match(html, "P1 : 4 arbres sur 10 mesurés au ruban", fixed = TRUE)
  expect_false(grepl("P2 :", html, fixed = TRUE))
  # Projet anterieur au schema 1.4.0 (n_ruban NA) ou element absent : rien.
  expect_false(grepl("ruban", as.character(video_bilan_ui(video_bilan(
    list(validation = list(ok = TRUE))), get_i18n("fr"))), fixed = TRUE))
  na <- video_bilan(list(validation = list(ok = TRUE, dbh_ruban = data.frame(
    plot_id = "P1", n_arbres = 3L, n_ruban = NA_integer_, part_ruban = NA_real_))))
  expect_false(grepl("ruban", as.character(video_bilan_ui(na, get_i18n("fr"))),
                     fixed = TRUE))
})

test_that("coeur reel : deux appels, meme terrain_envoye_le -> une seule requete terrain.zip", {
  skip_if_not_installed("httr2")
  skip_if(!requireNamespace("nemeton", quietly = TRUE))
  skip_if(utils::packageVersion("nemeton") < "2026.11.30",
          "etat= demande nemeton >= 2026.11.30")
  d <- withr::local_tempdir()
  n_req <- 0L
  httr2::local_mocked_responses(function(req) {
    n_req <<- n_req + 1L
    httr2::response(status_code = 200L, body = charToRaw("zip"))
  })
  et <- data.frame(plot_id = "P1", terrain_envoye_le = "2026-11-15T13:25:00Z")
  f1 <- video_telecharger("https://groma.test/", "j", "P1", d,
                          fichiers = "terrain.zip", etat = et)
  f2 <- video_telecharger("https://groma.test/", "j", "P1", d,
                          fichiers = "terrain.zip", etat = et)
  expect_identical(n_req, 1L)
  expect_identical(basename(f1), "P1_terrain.zip")
  expect_identical(f2, f1)
  # Date changee : redemande.
  et$terrain_envoye_le <- "2026-11-16T08:00:00Z"
  video_telecharger("https://groma.test/", "j", "P1", d,
                    fichiers = "terrain.zip", etat = et)
  expect_identical(n_req, 2L)
})

test_that("Recuperer reinterroge le serveur a chaque clic et passe l'etat frais", {
  withr::local_envvar(GROMA_URL = "https://groma.test/", GROMA_JETON = "secret")
  pp <- withr::local_tempdir()
  n_srv <- 0L
  recu <- NULL
  local_mocked_bindings(
    get_project_path = function(id) pp,
    .project_chm = function(id) NULL,
    video_placettes_plan = function(project_path) NULL,
    deny_if_readonly = function(app_state, ...) FALSE,
    video_etat_serveur = function(url, jeton) {
      n_srv <<- n_srv + 1L
      data.frame(plot_id = "P001", etat = "definitif",
                 terrain_envoye_le = paste0("2026-11-15T13:2", n_srv, ":00Z"))
    },
    video_telecharger = function(url, jeton, plot_ids, dossier, fichiers = NULL,
                                 etat = NULL) {
      recu <<- etat
      character(0)
    }
  )
  shiny::testServer(mod_inventaire_video_server, args = list(app_state = .etat_video()), {
    session$setInputs(actualiser = 1)
    expect_identical(n_srv, 1L)
    expect_false(is.null(rv$srv))
    session$setInputs(recuperer = 1)
    expect_identical(n_srv, 2L)
    expect_identical(recu$terrain_envoye_le, "2026-11-15T13:22:00Z")
    expect_identical(rv$srv$terrain_envoye_le, "2026-11-15T13:22:00Z")
  })
})


# ---- Mission 050 : carte, parametres, CVAT ------------------------------------

.plan_video3 <- function() {
  sf::st_sf(
    plot_id = c("P001", "P002", "P003"),
    geometry = sf::st_sfc(sf::st_point(c(900250, 6500250)),
                          sf::st_point(c(900750, 6500750)),
                          sf::st_point(c(901250, 6501250)), crs = 2154))
}

.local_video <- function(pp, plan = .plan_video3(), env = parent.frame(), ...) {
  local_mocked_bindings(
    get_project_path = function(id) pp,
    .project_chm = function(id) NULL,
    video_placettes_plan = function(project_path) plan,
    video_coeur_disponible = function() TRUE,
    .acc_cvat_overlay_raster = function(...) NULL,
    ..., .env = env)
}

test_that("carte : une placette decochee passe en deselectionnee, et la carte suit la liste", {
  pp <- withr::local_tempdir()
  .local_video(pp)
  shiny::testServer(mod_inventaire_video_server, args = list(app_state = .etat_video()), {
    session$setInputs(placettes = c("P001", "P002", "P003"))
    d <- carte_donnees()
    expect_identical(d$plot_id, c("P001", "P002", "P003"))
    expect_true(all(d$selectionnee))
    session$setInputs(placettes = c("P001", "P003"))
    d <- carte_donnees()
    expect_identical(d$selectionnee, c(TRUE, FALSE, TRUE))
    # Tout decocher puis tout cocher : carte et liste d'accord.
    session$setInputs(placettes = NULL)
    expect_false(any(carte_donnees()$selectionnee))
    session$setInputs(placettes = c("P001", "P002", "P003"))
    expect_true(all(carte_donnees()$selectionnee))
  })
})

test_that("carte : un clic sur un marqueur bascule la placette, dans la liste aussi", {
  pp <- withr::local_tempdir()
  .local_video(pp)
  maj <- list()
  local_mocked_bindings(
    updateCheckboxGroupInput = function(session, inputId, ..., selected = NULL) {
      maj[[length(maj) + 1L]] <<- selected
    }, .package = "shiny")
  shiny::testServer(mod_inventaire_video_server, args = list(app_state = .etat_video()), {
    session$setInputs(placettes = c("P001", "P002", "P003"))
    session$setInputs(carte_marker_click = list(id = "P003", .nonce = 1))
    expect_identical(selection(), c("P001", "P002"))
    expect_identical(maj[[1]], c("P001", "P002"))
    expect_false(carte_donnees()$selectionnee[3])
    session$setInputs(carte_marker_click = list(id = "P003", .nonce = 2))
    expect_identical(selection(), c("P001", "P002", "P003"))
    expect_identical(maj[[2]], c("P001", "P002", "P003"))
    # Un identifiant inconnu ne change rien.
    session$setInputs(carte_marker_click = list(id = "ZZZ", .nonce = 3))
    expect_identical(selection(), c("P001", "P002", "P003"))
  })
})

test_that("video_basculer_selection garde l'ordre du plan et ignore l'inconnu", {
  tous <- c("P001", "P002", "P003")
  expect_identical(video_basculer_selection(c("P003", "P001"), "P002", tous), tous)
  expect_identical(video_basculer_selection(tous, "P002", tous), c("P001", "P003"))
  expect_identical(video_basculer_selection(character(0), "P001", tous), "P001")
  expect_identical(video_basculer_selection("P001", "X", tous), "P001")
  expect_identical(video_basculer_selection(NULL, NA, tous), character(0))
})

test_that("carte : plan absent ou vide -> pas de carte, le message existant, sans erreur", {
  pp <- withr::local_tempdir()
  .local_video(pp, plan = NULL)
  shiny::testServer(mod_inventaire_video_server, args = list(app_state = .etat_video()), {
    session$flushReact()
    expect_null(carte_donnees())
    expect_null(output$carte_ui)
    html <- paste(as.character(output$placettes_ui$html), collapse = " ")
    expect_match(html, htmltools::htmlEscape(get_i18n("fr")$t("video_sans_plan")),
                 fixed = TRUE)
  })
  expect_null(video_carte_donnees(NULL, "P001"))
  expect_null(video_carte_donnees(.plan_video3()[0, ], "P001"))
})

test_that("carte : placettes sans geometrie ou sans CRS ecartees, le reste en WGS84 en France", {
  d <- video_carte_donnees(.plan_video3(), "P002")
  expect_identical(sf::st_crs(d)$epsg, 4326L)
  xy <- sf::st_coordinates(d)
  # Lambert-93 (900250, 6500250) tombe en France, longitude puis latitude.
  expect_true(all(xy[, 1] > -5 & xy[, 1] < 10))
  expect_true(all(xy[, 2] > 41 & xy[, 2] < 52))
  expect_identical(d$selectionnee, c(FALSE, TRUE, FALSE))
  # Une placette connue : Paris (Notre-Dame, ~ 652 400 / 6 862 000 en Lambert-93).
  paris <- sf::st_sf(plot_id = "P1", geometry = sf::st_sfc(
    sf::st_point(c(652400, 6862050)), crs = 2154))
  xp <- unname(sf::st_coordinates(video_carte_donnees(paris, "P1")))
  expect_equal(xp[1, 1], 2.35, tolerance = 0.05)
  expect_equal(xp[1, 2], 48.85, tolerance = 0.05)
  # Placette dans un autre CRS : reprojetee au meme endroit.
  wgs <- sf::st_transform(paris, 4326)
  expect_equal(unname(sf::st_coordinates(video_carte_donnees(wgs, "P1"))), xp,
               tolerance = 1e-6)
  # Geometrie vide : ecartee ; toutes vides : pas de carte.
  vide <- .plan_video3()
  sf::st_geometry(vide)[2] <- sf::st_sfc(sf::st_point(), crs = 2154)
  expect_identical(video_carte_donnees(vide, character(0))$plot_id, c("P001", "P003"))
  tout_vide <- vide[2, ]
  expect_null(video_carte_donnees(tout_vide, "P002"))
  # Sans CRS : on ne devine pas la position.
  sans <- .plan_video3(); sf::st_crs(sans) <- NA
  expect_null(video_carte_donnees(sans, "P001"))
})

test_that("parametres : lus des metadonnees par video_args_generation, defauts inchanges", {
  pp <- withr::local_tempdir()
  defaut <- video_args_generation(pp, .plan_video(), "P001")
  expect_identical(defaut$rayon_m, 15)
  expect_identical(defaut$seuil_diam_cm, 7.5)
  expect_identical(defaut$unite_azimut, "deg")
  expect_null(defaut$declinaison_deg)
  md <- list(video_params = list(rayon_m = 20, seuil_diam_cm = 12.5,
                                 unite_azimut = "gr", declinaison_deg = 1.2))
  a <- video_args_generation(pp, .plan_video(), "P001", metadata = md)
  expect_identical(a[c("rayon_m", "seuil_diam_cm", "unite_azimut", "declinaison_deg")],
                   list(rayon_m = 20, seuil_diam_cm = 12.5, unite_azimut = "gr",
                        declinaison_deg = 1.2))
  # Declinaison vide ou illisible -> NULL (Groma la calcule), jamais 0.
  md$video_params$declinaison_deg <- NULL
  expect_null(video_args_generation(pp, .plan_video(), "P001", metadata = md)$declinaison_deg)
  md$video_params$declinaison_deg <- "abc"
  expect_null(video_args_generation(pp, .plan_video(), "P001", metadata = md)$declinaison_deg)
  # Une valeur explicite l'emporte sur les metadonnees.
  expect_identical(video_args_generation(pp, .plan_video(), "P001", metadata = md,
                                         rayon_m = 9)$rayon_m, 9)
  # Hors bornes ou inconnue -> defaut.
  expect_identical(project_video_params(list(video_params = list(
    rayon_m = 500, seuil_diam_cm = -1, unite_azimut = "rad", declinaison_deg = 99))),
    VIDEO_PARAMS_DEFAULT)
})

test_that("parametres : la generation du module utilise ceux du projet", {
  pp <- withr::local_tempdir()
  recu <- NULL
  .local_video(pp, video_lancer_generation = function(tache, args, ...) recu <<- args)
  etat <- .etat_video(metadata = list(video_params = list(rayon_m = 22)))
  shiny::testServer(mod_inventaire_video_server, args = list(app_state = etat), {
    session$setInputs(placettes = c("P001", "P002"))
    session$setInputs(generer = 1)
    expect_identical(recu$rayon_m, 22)
    expect_identical(recu$seuil_diam_cm, 7.5)
    # Seules les placettes selectionnees partent.
    expect_identical(recu$plot_ids, c("P001", "P002"))
  })
})

test_that("parametres : le rappel de la vue donne les valeurs et ou les changer", {
  pp <- withr::local_tempdir()
  .local_video(pp)
  etat <- .etat_video(metadata = list(video_params = list(rayon_m = 22,
                                                          unite_azimut = "gr")))
  shiny::testServer(mod_inventaire_video_server, args = list(app_state = etat), {
    html <- paste(as.character(output$params_rappel$html), collapse = " ")
    expect_match(html, "22 m", fixed = TRUE)
    expect_match(html, "grades", fixed = TRUE)
    expect_match(html, "WMM2025", fixed = TRUE)
    expect_match(html, "Sources &amp; param", fixed = TRUE)
  })
})

test_that("parametres : set_project_video_params persiste, et vider la declinaison l'efface", {
  pp <- withr::local_tempdir()
  writeLines('{"id":"pv","name":"x"}', file.path(pp, "metadata.json"))
  local_mocked_bindings(get_project_path = function(id) pp)
  set_project_video_params("pv", 18, 10, "gr", -0.4)
  md <- jsonlite::read_json(file.path(pp, "metadata.json"))
  expect_identical(project_video_params(md)$declinaison_deg, -0.4)
  expect_identical(project_video_params(md)$unite_azimut, "gr")
  set_project_video_params("pv", 18, 10, "gr", NA)
  md <- jsonlite::read_json(file.path(pp, "metadata.json"))
  expect_true(is.na(project_video_params(md)$declinaison_deg))
  expect_identical(project_video_params(md)$rayon_m, 18)
})

test_that("Sources & parametres : le bloc video s'affiche avec les valeurs du projet et enregistre", {
  pp <- withr::local_tempdir()
  enreg <- NULL
  local_mocked_bindings(
    theia_api_key_configured = function() FALSE,
    set_project_video_params = function(pid, ...) enreg <<- list(pid = pid, ...),
    load_project = function(pid) NULL,
    deny_if_readonly = function(...) FALSE)
  shiny::testServer(mod_sources_config_server, args = list(
    app_state = shiny::reactiveValues(
      language = "fr", project_id = "p1",
      current_project = list(id = "p1", metadata = list(
        video_params = list(rayon_m = 21, seuil_diam_cm = 9))))
  ), {
    session$setInputs(x = 1)
    html <- paste(as.character(output$vp_block), collapse = " ")
    expect_match(html, 'vp_rayon"[^>]*value="21"')
    expect_match(html, 'vp_decl"')
    session$setInputs(vp_rayon = 21, vp_seuil = 9,
                      vp_unite = "deg", vp_decl = NA, vp_save = 1)
    expect_identical(enreg$pid, "p1")
    expect_identical(enreg$rayon_m, 21)
    expect_identical(enreg$unite_azimut, "deg")
  })
})

# ---- CVAT : etat, calcul, trois etats de la ligne -------------------------------

.mnt_projet <- function(pp, nom = "lidar_mnt_mosaic.tif", sous = c("cache", "layers")) {
  d <- do.call(file.path, as.list(c(pp, sous)))
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
  f <- file.path(d, nom)
  file.create(f)
  f
}

test_that("CVAT : MNT LiDAR sans CVAT -> a calculer ; avec CVAT -> joint", {
  pp <- withr::local_tempdir()
  mnt <- .mnt_projet(pp)
  local_mocked_bindings(.acc_rvt_mnt_path = function(...) mnt,
                        .rvt_cvat_available = function() TRUE)
  p <- video_cvat_plan(pp, NULL, 250)
  expect_identical(p$etat, "a_calculer")
  expect_identical(p$mnt, mnt)
  cvat <- file.path(dirname(mnt), "lidar_mnt_mosaic_CVAT_8bit_foretaccess.tif")
  file.create(cvat)
  p2 <- video_cvat_plan(pp, NULL, 250)
  expect_identical(p2$etat, "joint")
  expect_identical(p2$cvat, cvat)
})

test_that("CVAT : MNT WMS, absence de MNT ou foretaccess trop ancien -> jamais de calcul", {
  pp <- withr::local_tempdir()
  wms <- .mnt_projet(pp, "mnt_highres_1m.tif", c("cache", "accessibility", "emprise_250m"))
  local_mocked_bindings(.acc_rvt_mnt_path = function(...) wms,
                        .rvt_cvat_available = function() TRUE)
  p <- video_cvat_plan(pp, NULL, 250)
  expect_identical(c(p$etat, p$raison), c("absent", "video_cvat_mnt_wms"))
  local_mocked_bindings(.acc_rvt_mnt_path = function(...) NULL)
  p <- video_cvat_plan(pp, NULL, 250)
  expect_identical(c(p$etat, p$raison), c("absent", "video_cvat_sans_mnt"))
  lidar <- .mnt_projet(pp)
  local_mocked_bindings(.acc_rvt_mnt_path = function(...) lidar,
                        .rvt_cvat_available = function() FALSE)
  p <- video_cvat_plan(pp, NULL, 250)
  expect_identical(c(p$etat, p$raison), c("absent", "video_cvat_sans_outil"))
})

test_that("CVAT : un echec recent avec les memes parametres n'est pas rejoue", {
  pp <- withr::local_tempdir()
  mnt <- .mnt_projet(pp)
  local_mocked_bindings(.acc_rvt_mnt_path = function(...) mnt,
                        .rvt_cvat_available = function() TRUE,
                        .cvat_buffer_plafonne = function(mnt, aoi, b) b,
                        .cvat_failed_for = function(...) TRUE)
  aoi <- sf::st_sf(geometry = sf::st_sfc(sf::st_point(c(900250, 6500250)), crs = 2154))
  p <- video_cvat_plan(pp, aoi, 250)
  expect_identical(c(p$etat, p$raison), c("absent", "video_cvat_echec"))
  local_mocked_bindings(.cvat_failed_for = function(...) FALSE)
  expect_identical(video_cvat_plan(pp, aoi, 250)$etat, "a_calculer")
})

test_that("video_cvat_calculer delegue a build_cvat_precomputed avec l'AOI et le tampon", {
  recu <- NULL
  local_mocked_bindings(build_cvat_precomputed = function(mnt_path, aoi = NULL,
                                                          buffer_m = 0, ...) {
    recu <<- list(mnt_path, aoi, buffer_m); "/x/cvat.tif" })
  expect_identical(video_cvat_calculer("/x/m.tif", aoi = "AOI", buffer_m = 300), "/x/cvat.tif")
  expect_identical(recu, list("/x/m.tif", "AOI", 300))
})

.cvat_module <- function(pp, mnt, lancer, onglet = TRUE, env = parent.frame(), ...) {
  .local_video(pp, env = env, .acc_rvt_mnt_path = function(...) mnt,
               .rvt_cvat_available = function() TRUE,
               .resolve_project_aoi_2154 = function(...) NULL,
               video_lancer_cvat = lancer, ...)
  etat <- .etat_video()
  etat$active_main_tab <- if (onglet) "terrain" else "home"
  etat$active_terrain_tab <- if (onglet) "inventaire_video" else "inventaire_video"
  etat
}

test_that("CVAT : a l'arrivee sur la vue, MNT LiDAR sans CVAT -> le calcul part une fois", {
  pp <- withr::local_tempdir(); mnt <- .mnt_projet(pp)
  n <- 0L
  etat <- .cvat_module(pp, mnt, function(tache, plan, aoi, dev_path = NULL) n <<- n + 1L)
  shiny::testServer(mod_inventaire_video_server, args = list(app_state = etat), {
    session$flushReact()
    expect_identical(n, 1L)
    expect_true(cvat_en_cours())
    # Revenir sur la vue pendant le calcul n'en relance pas un second.
    app_state$active_terrain_tab <- "accessibility"
    session$flushReact()
    app_state$active_terrain_tab <- "inventaire_video"
    session$flushReact()
    expect_identical(n, 1L)
  })
})

test_that("CVAT : pas de calcul si le CVAT existe, si le MNT est WMS, sans MNT, ou hors de la vue", {
  n <- 0L
  lancer <- function(tache, plan, aoi, dev_path = NULL) n <<- n + 1L
  essayer <- function(pp, mnt, onglet = TRUE) {
    n <<- 0L
    etat <- .cvat_module(pp, mnt, lancer, onglet = onglet)
    if (!onglet) etat$active_main_tab <- "home"
    shiny::testServer(mod_inventaire_video_server, args = list(app_state = etat),
                      session$flushReact())
    n
  }
  # CVAT deja la.
  pp <- withr::local_tempdir(); mnt <- .mnt_projet(pp)
  file.create(file.path(dirname(mnt), "lidar_mnt_mosaic_CVAT_8bit_foretaccess.tif"))
  expect_identical(essayer(pp, mnt), 0L)
  # MNT WMS.
  pp2 <- withr::local_tempdir()
  wms <- .mnt_projet(pp2, "mnt_highres_1m.tif", c("cache", "accessibility", "e"))
  expect_identical(essayer(pp2, wms), 0L)
  # Sans MNT.
  expect_identical(essayer(withr::local_tempdir(), NULL), 0L)
  # Contre-epreuve : MNT LiDAR sans CVAT, mais pas sur la vue -> rien non plus ;
  # sur la vue -> part.
  pp3 <- withr::local_tempdir(); mnt3 <- .mnt_projet(pp3)
  expect_identical(essayer(pp3, mnt3, onglet = FALSE), 0L)
  expect_identical(essayer(pp3, mnt3, onglet = TRUE), 1L)
})

test_that("CVAT : la ligne d'etat dit joint, en cours, ou absent avec la raison", {
  pp <- withr::local_tempdir(); mnt <- .mnt_projet(pp)
  etat <- .cvat_module(pp, mnt, function(...) NULL)
  texte <- function(out) paste(as.character(out$html), collapse = " ")
  shiny::testServer(mod_inventaire_video_server, args = list(app_state = etat), {
    session$flushReact()
    # En cours (le calcul vient de partir) : le projet genere maintenant part sans relief.
    expect_match(texte(output$cvat_info), "calcul en cours", fixed = TRUE)
    expect_false(grepl("joint au projet", texte(output$cvat_info), fixed = TRUE))
    # Fin du calcul sans fichier : absent, avec l'echec.
    .cvat_terminer(echec = FALSE)
    session$flushReact()
    expect_match(texte(output$cvat_info), "absent", fixed = TRUE)
    expect_match(texte(output$cvat_info), "a \u00e9chou\u00e9", fixed = TRUE)
    # Le CVAT apparait : joint.
    file.create(file.path(dirname(mnt), "lidar_mnt_mosaic_CVAT_8bit_foretaccess.tif"))
    .cvat_terminer(echec = FALSE)
    session$flushReact()
    expect_match(texte(output$cvat_info), "joint au projet", fixed = TRUE)
  })
  # Absent : MNT WMS, avec sa raison.
  pp2 <- withr::local_tempdir()
  wms <- .mnt_projet(pp2, "mnt_highres_1m.tif", c("cache", "accessibility", "e"))
  etat2 <- .cvat_module(pp2, wms, function(...) NULL)
  shiny::testServer(mod_inventaire_video_server, args = list(app_state = etat2), {
    session$flushReact()
    expect_match(texte(output$cvat_info), "WMS", fixed = TRUE)
    expect_match(texte(output$cvat_info), "absent", fixed = TRUE)
  })
})

test_that("CVAT : sans CVAT, args n'a pas de cvat_path ; avec, il y pointe", {
  pp <- withr::local_tempdir(); mnt <- .mnt_projet(pp)
  local_mocked_bindings(.acc_rvt_mnt_path = function(...) mnt)
  expect_false("cvat_path" %in% names(video_args_generation(pp, .plan_video(), "P001")))
  cvat <- file.path(dirname(mnt), "lidar_mnt_mosaic_CVAT_8bit_foretaccess.tif")
  file.create(cvat)
  expect_identical(video_args_generation(pp, .plan_video(), "P001")$cvat_path, cvat)
})

# ---- Mission 050, reprise 1 : carte stable, fin de calcul par projet, alertes ---

test_that("carte : la carte n'est rendue qu'une fois, quelles que soient les selections", {
  pp <- withr::local_tempdir()
  .local_video(pp)
  n <- 0L
  local_mocked_bindings(
    leafletOutput = function(...) { n <<- n + 1L; htmltools::div("carte") },
    .package = "leaflet")
  shiny::testServer(mod_inventaire_video_server, args = list(app_state = .etat_video()), {
    session$setInputs(placettes = c("P001", "P002", "P003"))
    output$carte_ui
    session$setInputs(placettes = "P001")
    output$carte_ui
    session$setInputs(placettes = NULL)
    output$carte_ui
    session$setInputs(carte_marker_click = list(id = "P002", .nonce = 1))
    output$carte_ui
    session$setInputs(placettes = c("P002", "P003"))
    output$carte_ui
    expect_identical(n, 1L)
  })
})

test_that("carte : les marqueurs sont peints des le premier rendu, selection initiale comprise", {
  pp <- withr::local_tempdir()
  .local_video(pp)
  shiny::testServer(mod_inventaire_video_server, args = list(app_state = .etat_video()), {
    session$setInputs(placettes = c("P001", "P002", "P003"))
    w <- paste(as.character(output$carte), collapse = "")
    expect_match(w, "addCircleMarkers", fixed = TRUE)
    for (id in c("P001", "P002", "P003")) expect_match(w, id, fixed = TRUE)
  })
})

test_that("CVAT : la fin du calcul d'un projet n'est pas attribuee a un autre", {
  pp <- withr::local_tempdir(); mnt <- .mnt_projet(pp)
  pp2 <- withr::local_tempdir()
  n <- 0L
  etat <- .cvat_module(pp, mnt, function(tache, plan, aoi, dev_path = NULL) n <<- n + 1L)
  local_mocked_bindings(
    get_project_path = function(id) if (identical(id, "p_video")) pp else pp2,
    .acc_rvt_mnt_path = function(p) if (identical(p, pp)) mnt else NULL)
  shiny::testServer(mod_inventaire_video_server, args = list(app_state = etat), {
    session$flushReact()
    expect_identical(n, 1L)
    app_state$current_project <- list(id = "p_autre", metadata = list())
    session$flushReact()
    # Le calcul de A se termine en echec pendant que B est ouvert.
    .cvat_terminer(echec = TRUE)
    session$flushReact()
    expect_false(cvat_echec())
    expect_false(cvat_en_cours())
    expect_false(grepl("a échoué", paste(as.character(output$cvat_info$html), collapse = " ")))
  })
})

test_that("CVAT : apres un echec, quitter puis revenir sur la vue ne relance pas le calcul", {
  pp <- withr::local_tempdir(); mnt <- .mnt_projet(pp)
  n <- 0L
  etat <- .cvat_module(pp, mnt, function(tache, plan, aoi, dev_path = NULL) n <<- n + 1L)
  shiny::testServer(mod_inventaire_video_server, args = list(app_state = etat), {
    session$flushReact()
    expect_identical(n, 1L)
    .cvat_terminer(echec = TRUE)
    session$flushReact()
    app_state$active_terrain_tab <- "accessibility"
    session$flushReact()
    app_state$active_terrain_tab <- "inventaire_video"
    session$flushReact()
    expect_identical(n, 1L)
  })
})

test_that("parametres : video_params_remplaces nomme les valeurs hors bornes", {
  expect_identical(video_params_remplaces(15, 7.5, "deg", NA), character(0))
  expect_identical(video_params_remplaces(80, 7.5, "deg", NA), "video_rayon")
  expect_identical(video_params_remplaces(15, -2, "rad", 99),
                   c("video_seuil_diam", "video_unite_azimut", "video_declinaison"))
  # Une declinaison vide est legitime ; un rayon efface (NA) retombe sur le defaut.
  expect_identical(video_params_remplaces(NA, 7.5, "deg", NULL), "video_rayon")
})

test_that("Sources & parametres : un rayon hors bornes est signale, pas annonce comme enregistre tel quel", {
  notifs <- character(0)
  local_mocked_bindings(
    theia_api_key_configured = function() FALSE,
    set_project_video_params = function(...) invisible(TRUE),
    load_project = function(pid) NULL,
    deny_if_readonly = function(...) FALSE)
  local_mocked_bindings(
    showNotification = function(ui, ..., type = "default") {
      notifs <<- c(notifs, paste(type, paste(as.character(ui), collapse = "")))
    }, .package = "shiny")
  shiny::testServer(mod_sources_config_server, args = list(
    app_state = shiny::reactiveValues(
      language = "fr", project_id = "p1",
      current_project = list(id = "p1", metadata = list()))
  ), {
    session$setInputs(x = 1)
    session$setInputs(vp_rayon = 80, vp_seuil = 7.5, vp_unite = "deg", vp_decl = NA)
    session$setInputs(vp_save = 1)
    expect_true(any(grepl("^warning .*rayon de placette", notifs)))
    notifs <<- character(0)
    session$setInputs(vp_rayon = 15)
    session$setInputs(vp_save = 2)
    expect_false(any(grepl("^warning", notifs)))
  })
})

test_that("carte : un autre projet avec son plan recadre la carte, une selection non", {
  ppA <- withr::local_tempdir(); ppB <- withr::local_tempdir()
  planB <- sf::st_sf(plot_id = c("Q1", "Q2"), geometry = sf::st_sfc(
    sf::st_point(c(652400, 6862050)), sf::st_point(c(652500, 6862150)), crs = 2154))
  local_mocked_bindings(
    get_project_path = function(id) if (identical(id, "p_video")) ppA else ppB,
    .project_chm = function(id) NULL,
    video_placettes_plan = function(project_path)
      if (identical(project_path, ppA)) .plan_video3() else planB,
    video_coeur_disponible = function() TRUE,
    .acc_cvat_overlay_raster = function(...) NULL)
  cadres <- list()
  local_mocked_bindings(
    fitBounds = function(map, lng1, lat1, lng2, lat2, ...) {
      cadres[[length(cadres) + 1L]] <<- c(lng1, lat1); map
    }, .package = "leaflet")
  shiny::testServer(mod_inventaire_video_server, args = list(app_state = .etat_video()), {
    session$setInputs(placettes = c("P001", "P002", "P003"))
    invisible(output$carte)
    expect_length(cadres, 1L)
    # Une selection ne recadre pas.
    session$setInputs(placettes = "P001")
    invisible(output$carte)
    expect_length(cadres, 1L)
    # Un autre projet, avec son plan a Paris : nouveau cadrage sur B.
    app_state$current_project <- list(id = "p_autre", metadata = list())
    session$flushReact()
    invisible(output$carte)
    expect_length(cadres, 2L)
    expect_equal(cadres[[2]][1], 2.35, tolerance = 0.05)
    expect_equal(cadres[[2]][2], 48.85, tolerance = 0.05)
  })
})


# ---- Mission 055 : le coeur refuse le codage du CVAT ----------------------------

.erreur_codage <- function() {
  structure(class = c("nemeton_erreur_cvat_codage", "error", "condition"),
            list(message = "CVAT encoding unknown", call = NULL))
}

# Coeur simule : `reponses` est la suite des comportements (une erreur ou un chemin).
.coeur_simule <- function(reponses, appels) {
  function(nom, ...) {
    appels$liste[[length(appels$liste) + 1L]] <- list(...)
    r <- reponses[[length(appels$liste)]]
    if (inherits(r, "condition")) stop(r)
    r
  }
}

test_that("055 cas 1 : codage refuse -> second appel sans cvat_path, projet rendu, marque", {
  appels <- new.env(); appels$liste <- list()
  local_mocked_bindings(.video_appeler = .coeur_simule(
    list(.erreur_codage(), "/p/inventaire_video.qgz"), appels))
  res <- video_creer_projet(project_path = "/p", cvat_path = "/p/cvat.tif", lang = "fr")
  expect_identical(as.character(res), "/p/inventaire_video.qgz")
  expect_true(isTRUE(attr(res, "cvat_refuse")))
  expect_length(appels$liste, 2L)
  expect_identical(appels$liste[[1]]$cvat_path, "/p/cvat.tif")
  expect_false("cvat_path" %in% names(appels$liste[[2]]))
  # Les autres arguments sont repris tels quels.
  expect_identical(appels$liste[[2]]$project_path, "/p")
  expect_identical(appels$liste[[2]]$lang, "fr")
})

test_that("055 cas 2 : une autre erreur ne declenche aucune reprise", {
  appels <- new.env(); appels$liste <- list()
  autre <- structure(class = c("simpleError", "error", "condition"),
                     list(message = "disque plein", call = NULL))
  local_mocked_bindings(.video_appeler = .coeur_simule(list(autre, "/p/x.qgz"), appels))
  expect_error(video_creer_projet(cvat_path = "/p/cvat.tif"), "disque plein")
  expect_length(appels$liste, 1L)
})

test_that("055 cas 3 : sans erreur, un seul appel avec cvat_path, sans marque", {
  appels <- new.env(); appels$liste <- list()
  local_mocked_bindings(.video_appeler = .coeur_simule(list("/p/x.qgz"), appels))
  res <- video_creer_projet(cvat_path = "/p/cvat.tif")
  expect_identical(res, "/p/x.qgz")
  expect_null(attr(res, "cvat_refuse"))
  expect_length(appels$liste, 1L)
  expect_identical(appels$liste[[1]]$cvat_path, "/p/cvat.tif")
})

test_that("055 cas 4 : le second appel echoue aussi -> son erreur remonte, sans boucle", {
  appels <- new.env(); appels$liste <- list()
  seconde <- structure(class = c("simpleError", "error", "condition"),
                       list(message = "MNT introuvable", call = NULL))
  local_mocked_bindings(.video_appeler = .coeur_simule(
    list(.erreur_codage(), seconde, "/p/jamais.qgz"), appels))
  expect_error(video_creer_projet(cvat_path = "/p/cvat.tif"), "MNT introuvable")
  expect_length(appels$liste, 2L)
})

test_that("055 : sans cvat_path, un refus de codage remonte tel quel (rien a retirer)", {
  appels <- new.env(); appels$liste <- list()
  local_mocked_bindings(.video_appeler = .coeur_simule(
    list(.erreur_codage(), "/p/x.qgz"), appels))
  expect_error(video_creer_projet(project_path = "/p"), class = "nemeton_erreur_cvat_codage")
  expect_length(appels$liste, 1L)
})

test_that("055 cas 5 : la ligne d'etat dit absent, codage non reconnu, et un avertissement part", {
  pp <- withr::local_tempdir(); mnt <- .mnt_projet(pp)
  file.create(file.path(dirname(mnt), "lidar_mnt_mosaic_CVAT_8bit_foretaccess.tif"))
  etat <- .cvat_module(pp, mnt, function(...) NULL)
  notifs <- list()
  local_mocked_bindings(
    showNotification = function(ui, ..., type = "default") {
      notifs[[length(notifs) + 1L]] <<- list(ui = ui, type = type)
    }, .package = "shiny")
  texte <- function(out) paste(as.character(out$html), collapse = " ")
  shiny::testServer(mod_inventaire_video_server, args = list(app_state = etat), {
    session$flushReact()
    expect_match(texte(output$cvat_info), "joint au projet", fixed = TRUE)
    # Un projet genere normalement ne change rien.
    .noter_cvat_refuse("/p/x.qgz")
    session$flushReact()
    expect_match(texte(output$cvat_info), "joint au projet", fixed = TRUE)
    expect_length(notifs, 0L)
    # Le coeur a refuse le codage : absent, avec la raison, et un avertissement.
    refuse <- structure("/p/x.qgz", cvat_refuse = TRUE)
    .noter_cvat_refuse(refuse)
    session$flushReact()
    expect_match(texte(output$cvat_info), "absent", fixed = TRUE)
    expect_match(texte(output$cvat_info), "codage du fichier non reconnu", fixed = TRUE)
    expect_false(grepl("joint au projet", texte(output$cvat_info), fixed = TRUE))
    expect_length(notifs, 1L)
    expect_identical(notifs[[1]]$type, "warning")
    expect_match(notifs[[1]]$ui, "sans relief", fixed = TRUE)
  })
})

test_that("055 : apres un refus, une generation sans refus ou un CVAT recalcule remet « joint »", {
  pp <- withr::local_tempdir(); mnt <- .mnt_projet(pp)
  cvat <- file.path(dirname(mnt), "lidar_mnt_mosaic_CVAT_8bit_foretaccess.tif")
  # Sans CVAT au depart : le calcul de l'app part (cvat_projet est alors pose).
  etat <- .cvat_module(pp, mnt, function(...) NULL)
  local_mocked_bindings(showNotification = function(...) NULL, .package = "shiny")
  texte <- function(out) paste(as.character(out$html), collapse = " ")
  shiny::testServer(mod_inventaire_video_server, args = list(app_state = etat), {
    session$flushReact()
    file.create(cvat)
    .noter_cvat_refuse(structure("/p/x.qgz", cvat_refuse = TRUE))
    session$flushReact()
    expect_match(texte(output$cvat_info), "codage du fichier non reconnu", fixed = TRUE)
    # Generation suivante reussie, CVAT accepte : la ligne revient a joint.
    .noter_cvat_refuse("/p/y.qgz")
    session$flushReact()
    expect_match(texte(output$cvat_info), "joint au projet", fixed = TRUE)
    expect_false(grepl("codage", texte(output$cvat_info), fixed = TRUE))
    # Nouveau refus, puis fin d'un calcul de CVAT reussi par l'app.
    .noter_cvat_refuse(structure("/p/z.qgz", cvat_refuse = TRUE))
    session$flushReact()
    expect_match(texte(output$cvat_info), "codage", fixed = TRUE)
    .cvat_terminer(echec = FALSE)
    session$flushReact()
    expect_match(texte(output$cvat_info), "joint au projet", fixed = TRUE)
  })
})

test_that("055 : les deux cles i18n existent en FR et EN", {
  for (k in c("video_cvat_codage", "video_cvat_codage_avert")) {
    expect_lte(nchar(k), 30L)
    for (l in c("fr", "en")) expect_true(nzchar(get_i18n(l)$t(k)))
    expect_false(identical(get_i18n("fr")$t(k), get_i18n("en")$t(k)))
  }
})

# ---- Mission 058 : progression de la creation du projet QField ------------

.evts_058 <- function() {
  e <- function(etape, i, n) list(etape = etape, i = i, n = n, secondes = 1.5)
  c(list(e("sources", 0, 1), e("sources", 1, 1), e("placettes", 0, 3)),
    lapply(1:3, function(i) e("placettes", i, 3)),
    list(e("houppiers", 0, 4), e("houppiers", 2, 4), e("qgz", 1, 1)))
}

test_that("058 cas 1 : les evenements du coeur arrivent dans le fichier, dans l'ordre", {
  fichier <- withr::local_tempfile(fileext = ".txt")
  vus <- list()
  local_mocked_bindings(
    video_coeur_accepte_progression = function() TRUE,
    .video_appeler = function(nom, ..., progress_callback = NULL) {
      expect_true(is.function(progress_callback))
      for (e in .evts_058()) {
        progress_callback(e)
        vus[[length(vus) + 1L]] <<- video_progression_lire(fichier)
      }
      "/p/x.qgz"
    })
  res <- video_creer_projet(project_path = "/p", progression_fichier = fichier)
  expect_identical(res, "/p/x.qgz")
  expect_identical(vus[[4]]$etape, "placettes")
  expect_identical(c(vus[[4]]$i, vus[[4]]$n), c(1L, 3L))
  expect_identical(vus[[8]]$etape, "houppiers")
  expect_identical(c(vus[[8]]$i, vus[[8]]$n), c(2L, 4L))
  expect_identical(video_progression_lire(fichier)$etape, "qgz")
})

test_that("058 cas 1 : la ligne affichee dit l'etape, le compteur i/n et le temps", {
  tr <- get_i18n("fr")
  l <- video_progression_ligne(list(etape = "houppiers", i = 2L, n = 4L, secondes = 3), 75, tr)
  expect_match(l, "Segmentation des houppiers \\(2/4\\)")
  expect_match(l, "1:15")
  # Une etape a une seule unite n'affiche pas de compteur.
  l2 <- video_progression_ligne(list(etape = "qgz", i = 1L, n = 1L, secondes = 3), 5, tr)
  expect_false(grepl("1/1", l2))
  # Le i = 0 ré-emis au repli sequentiel fait repartir le compteur.
  l3 <- video_progression_ligne(list(etape = "houppiers", i = 0L, n = 4L, secondes = 0), 90, tr)
  expect_match(l3, "\\(0/4\\)")
})

test_that("058 cas 1 : dans le module, la ligne suit le fichier de progression", {
  pp <- withr::local_tempdir()
  recu <- NULL
  local_mocked_bindings(
    get_project_path = function(id) pp,
    .project_chm = function(id) NULL,
    video_placettes_plan = function(project_path) .plan_video(),
    video_coeur_disponible = function() TRUE,
    video_coeur_accepte_progression = function() TRUE,
    video_lancer_generation = function(tache, args, ...) recu <<- args
  )
  shiny::testServer(mod_inventaire_video_server, args = list(app_state = .etat_video()), {
    session$setInputs(placettes = "P001", generer = 1)
    expect_true(nzchar(recu$progression_fichier))
    video_progression_ecrire(recu$progression_fichier,
      list(etape = "placettes", i = 2L, n = 3L, secondes = 1))
    # Le minuteur de 500 ms relit le fichier.
    session$elapse(600)
    expect_match(output$progression_info$html, "2/3")
    expect_match(output$progression_info$html, "Rasters et topographie|Plot rasters")
  })
})

test_that("058 cas 2 : coeur sans progress_callback, rien en trop et generation inchangee", {
  fichier <- withr::local_tempfile(fileext = ".txt")
  appels <- new.env(); appels$liste <- list()
  local_mocked_bindings(
    video_coeur_accepte_progression = function() FALSE,
    .video_appeler = .coeur_simule(list("/p/x.qgz"), appels))
  res <- video_creer_projet(project_path = "/p", progression_fichier = fichier)
  expect_identical(res, "/p/x.qgz")
  expect_identical(names(appels$liste[[1]]), "project_path")
  expect_false(file.exists(fichier))
})

test_that("058 cas 2 : le predicat lit la signature du coeur installe", {
  local_mocked_bindings(.video_coeur = function(nom) function(a, progress_callback = NULL) NULL)
  expect_true(video_coeur_accepte_progression())
  local_mocked_bindings(.video_coeur = function(nom) function(a) NULL)
  expect_false(video_coeur_accepte_progression())
  local_mocked_bindings(.video_coeur = function(nom) NULL)
  expect_false(video_coeur_accepte_progression())
})

test_that("058 cas 2 : coeur sans progression, le module n'affiche aucune ligne d'attente", {
  pp <- withr::local_tempdir()
  local_mocked_bindings(
    get_project_path = function(id) pp,
    .project_chm = function(id) NULL,
    video_placettes_plan = function(project_path) .plan_video(),
    video_coeur_disponible = function() TRUE,
    video_coeur_accepte_progression = function() FALSE,
    video_lancer_generation = function(tache, args, ...) NULL
  )
  shiny::testServer(mod_inventaire_video_server, args = list(app_state = .etat_video()), {
    session$setInputs(placettes = "P001", generer = 1)
    expect_null(output$progression_info)
  })
})

test_that("058 cas 3 : un fichier de progression illisible ne fait pas echouer la generation", {
  # Dossier inexistant : l'ecriture echoue, la generation va au bout.
  fichier <- file.path(withr::local_tempdir(), "absent", "p.txt")
  local_mocked_bindings(
    video_coeur_accepte_progression = function() TRUE,
    .video_appeler = function(nom, ..., progress_callback = NULL) {
      progress_callback(list(etape = "sources", i = 0L, n = 1L, secondes = 0))
      "/p/x.qgz"
    })
  expect_no_error(res <- video_creer_projet(progression_fichier = fichier))
  expect_identical(res, "/p/x.qgz")
  # Lecture : contenu invalide ou absent -> NULL, jamais d'erreur.
  f <- withr::local_tempfile(fileext = ".txt")
  expect_null(video_progression_lire(f))
  writeLines("n'importe quoi", f)
  expect_null(video_progression_lire(f))
  writeLines("houppiers\tx\t4\t1", f)
  expect_null(video_progression_lire(f))
  file.create(f)
  expect_null(video_progression_lire(f))
  expect_null(video_progression_lire(NULL))
})

test_that("058 : apres un refus de codage, la reprise garde le rappel de progression", {
  fichier <- withr::local_tempfile(fileext = ".txt")
  n <- 0L
  local_mocked_bindings(
    video_coeur_accepte_progression = function() TRUE,
    .video_appeler = function(nom, ..., progress_callback = NULL) {
      n <<- n + 1L
      expect_true(is.function(progress_callback))
      if (n == 1L) stop(.erreur_codage())
      "/p/x.qgz"
    })
  res <- video_creer_projet(cvat_path = "/p/cvat.tif", progression_fichier = fichier)
  expect_identical(n, 2L)
  expect_true(isTRUE(attr(res, "cvat_refuse")))
})

test_that("058 cas 4 : tache lancee sans progression recue, la ligne dit l'attente", {
  pp <- withr::local_tempdir()
  local_mocked_bindings(
    get_project_path = function(id) pp,
    .project_chm = function(id) NULL,
    video_placettes_plan = function(project_path) .plan_video(),
    video_coeur_disponible = function() TRUE,
    video_coeur_accepte_progression = function() TRUE,
    video_lancer_generation = function(tache, args, ...) NULL
  )
  shiny::testServer(mod_inventaire_video_server, args = list(app_state = .etat_video()), {
    expect_null(output$progression_info)
    session$setInputs(placettes = "P001", generer = 1)
    expect_match(output$progression_info$html, "processus libre|free process")
  })
})

test_that("058 cas 5 : cles i18n FR/EN des sept etapes, de l'attente et de la ligne", {
  cles <- paste0("video_pg_", c("sources", "placettes", "dalles_mnt", "houppiers",
                                "cvat", "geopackage", "qgz", "attente", "ligne"))
  expect_true(all(nchar(cles) <= 30))
  for (k in cles) {
    expect_true(k %in% names(TRANSLATIONS), info = k)
    expect_true(nzchar(TRANSLATIONS[[k]]$fr) && nzchar(TRANSLATIONS[[k]]$en), info = k)
  }
  for (lang in c("fr", "en")) {
    tr <- get_i18n(lang)
    for (e in c("sources", "placettes", "dalles_mnt", "houppiers", "cvat", "geopackage", "qgz")) {
      l <- video_progression_ligne(list(etape = e, i = 1L, n = 2L, secondes = 0), 1, tr)
      expect_false(grepl("^video_pg_|^\\[", l), info = paste(lang, e))
    }
  }
  # Pas de litteral dans le module : la ligne passe par video_progression_ligne.
  # Sources absentes sous R CMD check (paquet installé) : même garde que
  # test-static_assets.R.
  f_mod <- testthat::test_path("../../R/mod_inventaire_video.R")
  skip_if_not(file.exists(f_mod), "sources R absentes (R CMD check)")
  src <- readLines(f_mod, encoding = "UTF-8")
  expect_false(any(grepl("processus libre|free process", src)))
})

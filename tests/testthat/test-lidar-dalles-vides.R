# Dalles LiDAR HD vides (brief 2026-10-09) : l'IGN publie le nuage de points
# avant les rasters derives, et le WMS sert alors des GeoTIFF valides mais
# 100 % NoData au lieu d'un 404. Ces dalles ne doivent ni etre comptees
# « downloaded », ni rester en cache, ni remplacer le DEM ou servir de CHM.

skip_if_not_installed("terra")
skip_if_not_installed("sf")

# Dalle 0846_6687 (Couchey), 1 km x 1 km en Lambert-93.
.dalle_l93 <- function(x0 = 846000, y0 = 6687000, n = 2000L, valeur = -9999) {
  r <- terra::rast(nrows = n, ncols = n, xmin = x0, xmax = x0 + 1000,
                   ymin = y0, ymax = y0 + 1000, crs = "EPSG:2154")
  terra::values(r) <- valeur
  r
}

.ecrire_dalle <- function(r, f) {
  terra::writeRaster(r, f, overwrite = TRUE, datatype = "FLT4S",
                     NAflag = -9999)
  f
}

# bbox WGS84 interieure a la dalle (marge de 100 m).
.bbox_dalle <- function(x0 = 846000, y0 = 6687000) {
  p <- sf::st_as_sfc(sf::st_bbox(c(xmin = x0 + 100, ymin = y0 + 100,
                                   xmax = x0 + 900, ymax = y0 + 900),
                                 crs = sf::st_crs(2154)))
  as.numeric(sf::st_bbox(sf::st_transform(p, 4326)))
}

.wfs_une_dalle <- function(url) {
  function(...) {
    sf::st_sf(url_telechargement = url,
              geometry = sf::st_sfc(sf::st_point(c(4.9, 47.3)), crs = 4326))
  }
}

test_that(".raster_part_valide distingue une dalle vide d'une dalle pleine", {
  withr::with_tempdir({
    vide <- .ecrire_dalle(.dalle_l93(), "vide.tif")
    pleine <- .ecrire_dalle(.dalle_l93(n = 200L, valeur = 12.5), "pleine.tif")
    expect_equal(nemetonshiny:::.raster_part_valide(vide), 0)
    expect_equal(nemetonshiny:::.raster_part_valide(pleine), 1)
    # -9999 non declare en NoData : compte vide quand meme.
    r <- .dalle_l93(n = 100L)
    expect_equal(nemetonshiny:::.raster_part_valide(r), 0)
    # Fichier illisible : vide.
    writeLines("pas un raster", "faux.tif")
    expect_equal(nemetonshiny:::.raster_part_valide("faux.tif"), 0)
  })
})

test_that("une dalle telechargee 100 % NoData est rejetee et sort du cache", {
  withr::with_tempdir({
    cache_dir <- getwd()
    vide <- .dalle_l93()
    msgs <- character(0)
    res <- withCallingHandlers(
      with_mocked_bindings(
        query_lidar_wfs = .wfs_une_dalle("http://example.com/0846_6687.tif"),
        download_lidar_tile = function(url, dest_file) .ecrire_dalle(vide, dest_file),
        nemetonshiny:::download_ign_lidar_hd(.bbox_dalle(), cache_dir, product = "mnt")
      ),
      message = function(m) {
        msgs <<- c(msgs, conditionMessage(m))
        invokeRestart("muffleMessage")
      }
    )
    expect_null(res)
    expect_false(file.exists(file.path(cache_dir, "lidar_mnt", "0846_6687.tif")))
    expect_false(file.exists(file.path(cache_dir, "lidar_mnt_mosaic.tif")))
    expect_false(any(grepl("downloaded", msgs)))
    expect_true(any(grepl("vide \\(produit non encore publi", msgs)))
  })
})

test_that("une dalle vide deja en cache est purgee puis retelechargee", {
  withr::with_tempdir({
    cache_dir <- getwd()
    dir.create(file.path(cache_dir, "lidar_mnh"))
    tuile <- file.path(cache_dir, "lidar_mnh", "0846_6687.tif")
    .ecrire_dalle(.dalle_l93(), tuile)
    appels <- 0L
    res <- suppressMessages(with_mocked_bindings(
      query_lidar_wfs = .wfs_une_dalle("http://example.com/0846_6687.tif"),
      download_lidar_tile = function(url, dest_file) {
        appels <<- appels + 1L
        .ecrire_dalle(.dalle_l93(n = 200L, valeur = 18), dest_file)
      },
      nemetonshiny:::download_ign_lidar_hd(.bbox_dalle(), cache_dir, product = "mnh")
    ))
    expect_equal(appels, 1L)
    expect_s4_class(res, "SpatRaster")
    expect_equal(nemetonshiny:::.raster_part_valide(tuile), 1)
  })
})

test_that("une mosaique vide en cache est jetee au lieu d'etre reutilisee", {
  withr::with_tempdir({
    cache_dir <- getwd()
    mosaique <- file.path(cache_dir, "lidar_mnt_mosaic.tif")
    .ecrire_dalle(.dalle_l93(n = 200L), mosaique)
    res <- suppressMessages(with_mocked_bindings(
      query_lidar_wfs = function(...) NULL,
      nemetonshiny:::download_ign_lidar_hd(.bbox_dalle(), cache_dir, product = "mnt")
    ))
    expect_null(res)
    expect_false(file.exists(mosaique))
  })
})

test_that("une mosaique qui couvre moins de 90 % de l'emprise n'est pas retenue", {
  withr::with_tempdir({
    cache_dir <- getwd()
    # Moitie ouest valide, moitie est NoData.
    r <- .dalle_l93(n = 200L, valeur = 300)
    v <- matrix(300, 200, 200)
    v[, 101:200] <- -9999
    terra::values(r) <- as.vector(t(v))
    res <- suppressMessages(with_mocked_bindings(
      query_lidar_wfs = .wfs_une_dalle("http://example.com/0846_6687.tif"),
      download_lidar_tile = function(url, dest_file) .ecrire_dalle(r, dest_file),
      nemetonshiny:::download_ign_lidar_hd(.bbox_dalle(), cache_dir, product = "mnt")
    ))
    expect_null(res)
    expect_false(file.exists(file.path(cache_dir, "lidar_mnt_mosaic.tif")))
  })
})

test_that(".raster_utilisable refuse un raster sans pixel sur l'emprise", {
  parcelles <- sf::st_sf(geometry = sf::st_sfc(sf::st_buffer(
    sf::st_point(c(846500, 6687500)), 200), crs = 2154))
  expect_false(suppressMessages(
    nemetonshiny:::.raster_utilisable(.dalle_l93(n = 100L), parcelles)))
  expect_true(nemetonshiny:::.raster_utilisable(
    .dalle_l93(n = 100L, valeur = 20), parcelles))
})

test_that(".cause_sans_raster nomme l'absence de CHM ou de MNT", {
  vals <- rep(NA_real_, 3)
  out <- nemetonshiny:::.cause_sans_raster(
    vals, "indicateur_p1_volume", c("units", "chm"), list(units = 1))
  expect_equal(attr(out, "nemeton_status"), rep("sans_chm", 3))
  expect_equal(attr(out, "nemeton_status_name"), "p1_status")

  out <- nemetonshiny:::.cause_sans_raster(
    vals, "indicateur_w3_humidite", c("units", "dem"), list(units = 1))
  expect_equal(attr(out, "nemeton_status"), rep("sans_mnt", 3))
  expect_equal(attr(out, "nemeton_status_name"), "w3_status")

  # Raster present, valeurs presentes, statut deja pose : rien ne change.
  expect_null(attr(nemetonshiny:::.cause_sans_raster(
    vals, "indicateur_p1_volume", "chm", list(chm = 1)), "nemeton_status"))
  expect_null(attr(nemetonshiny:::.cause_sans_raster(
    c(1, NA, 2), "indicateur_p1_volume", "chm", list()), "nemeton_status"))
  deja <- structure(vals, nemeton_status = rep("sans_age", 3),
                    nemeton_status_name = "p2_status")
  expect_equal(attr(nemetonshiny:::.cause_sans_raster(
    deja, "indicateur_p2_station", "chm", list()), "nemeton_status"),
    rep("sans_age", 3))
})

test_that("la banniere traduit sans_chm par la cle generique", {
  i18n <- nemetonshiny:::get_i18n("fr")
  sf_data <- data.frame(indicateur_p1_volume = c(NA_real_, NA_real_),
                        .p1_status = c("sans_chm", "sans_chm"))
  ban <- nemetonshiny:::indicator_na_banner(sf_data, "indicateur_p1_volume", i18n)
  expect_match(as.character(ban), "CHM", fixed = TRUE)
})

test_that("l'accessibilite ignore une mosaique MNT LiDAR vide", {
  withr::with_tempdir({
    dir.create(file.path("cache", "layers"), recursive = TRUE)
    f <- file.path("cache", "layers", "lidar_mnt_mosaic.tif")
    .ecrire_dalle(.dalle_l93(n = 100L), f)
    expect_null(nemetonshiny:::.lidar_mnt_mosaique_valide(getwd()))
    expect_null(nemetonshiny:::.acc_rvt_mnt_path(getwd()))
    .ecrire_dalle(.dalle_l93(n = 100L, valeur = 480), f)
    expect_equal(nemetonshiny:::.lidar_mnt_mosaique_valide(getwd()),
                 file.path(getwd(), f))
  })
})

# Memo des dalles vides (brief 2026-10-11) -----------------------------------

# n dalles de 1 km sur une ligne, avec les colonnes de la couche de metadonnees.
.wfs_meta <- function(n, date_edition = "2026-09-16", x0 = 846000, y0 = 6687000) {
  noms <- sprintf("LHD_FXX_%04d_%04d_MNH.tif", (x0 / 1000) + seq_len(n) - 1L,
                  y0 / 1000)
  centres <- sf::st_transform(sf::st_sfc(lapply(seq_len(n), function(k)
    sf::st_point(c(x0 + (k - 1) * 1000 + 500, y0 + 500))), crs = 2154), 4326)
  function(...) {
    sf::st_sf(url_mnh = paste0("http://example.com/x?FILENAME=", noms),
              date_edition = date_edition,
              coordonnees_nw = sprintf("%04d-%04d", (x0 / 1000) + seq_len(n) - 1L,
                                       y0 / 1000 + 1),
              geometry = centres)
  }
}

# bbox WGS84 couvrant n dalles alignees.
.bbox_ligne <- function(n, x0 = 846000, y0 = 6687000) {
  p <- sf::st_as_sfc(sf::st_bbox(c(xmin = x0 + 10, ymin = y0 + 10,
                                   xmax = x0 + n * 1000 - 10, ymax = y0 + 990),
                                 crs = sf::st_crs(2154)))
  as.numeric(sf::st_bbox(sf::st_transform(p, 4326)))
}

# Lance download_ign_lidar_hd ; `pleines` = noms de dalles servies pleines.
.lancer_meta <- function(cache_dir, n, pleines = character(0), env, ...) {
  msgs <- character(0)
  res <- withCallingHandlers(
    with_mocked_bindings(
      query_lidar_wfs = .wfs_meta(n, ...),
      download_lidar_tile = function(url, dest_file) {
        env$appels <- c(env$appels, basename(dest_file))
        x0 <- as.numeric(sub(".*LHD_FXX_(\\d{4})_.*", "\\1", dest_file)) * 1000
        pleine <- basename(dest_file) %in% pleines
        .ecrire_dalle(.dalle_l93(x0 = x0, n = 100L,
                                 valeur = if (pleine) 18 else -9999), dest_file)
      },
      nemetonshiny:::download_ign_lidar_hd(.bbox_ligne(n), cache_dir, product = "mnh")
    ),
    message = function(m) {
      msgs <<- c(msgs, conditionMessage(m))
      invokeRestart("muffleMessage")
    }
  )
  env$msgs <- msgs
  res
}

.vieillir_memo <- function(cache_dir, jours) {
  f <- file.path(cache_dir, "lidar_mnh", "vides.json")
  m <- jsonlite::read_json(f)
  for (k in names(m$dalles)) {
    m$dalles[[k]]$constat <- format(Sys.time() - jours * 86400, "%Y-%m-%dT%H:%M:%S")
  }
  jsonlite::write_json(m, f, auto_unbox = TRUE)
}

test_that("une dalle memorisee vide n'est redemandee qu'a l'echeance", {
  withr::local_envvar(NEMETON_LIDAR_VIDE_TTL_JOURS = NA)
  withr::with_tempdir({
    cache_dir <- getwd()
    env <- new.env()
    expect_null(.lancer_meta(cache_dir, 1, env = env))
    expect_length(env$appels, 1)
    m <- jsonlite::read_json(file.path(cache_dir, "lidar_mnh", "vides.json"))
    e <- m$dalles[[1]]
    expect_equal(e$produit, "mnh")
    expect_equal(e$dalle, "0846-6688")
    expect_equal(e$date_edition, "2026-09-16")

    # 6 jours : pas de requete, journal date
    .vieillir_memo(cache_dir, 6)
    expect_null(.lancer_meta(cache_dir, 1, env = env))
    expect_length(env$appels, 1)
    expect_true(any(grepl("m\u00e9mo du .*nouvel essai apr\u00e8s le", env$msgs)))
    expect_true(any(grepl("All 1 LiDAR HD MNH tiles are empty", env$msgs)))

    # 8 jours : redemandee
    .vieillir_memo(cache_dir, 8)
    expect_null(.lancer_meta(cache_dir, 1, env = env))
    expect_length(env$appels, 2)
  })
})

test_that("une date_edition changee declenche le telechargement avant l'echeance", {
  withr::with_tempdir({
    cache_dir <- getwd()
    env <- new.env()
    .lancer_meta(cache_dir, 2, env = env)
    expect_length(env$appels, 2)
    .lancer_meta(cache_dir, 2, env = env)
    expect_length(env$appels, 2)
    res <- .lancer_meta(cache_dir, 2, env = env, date_edition = "2026-10-11",
                        pleines = sprintf("LHD_FXX_%04d_6687_MNH.tif", 846:847))
    expect_length(env$appels, 4)
    expect_s4_class(res, "SpatRaster")
    # Pleines : sorties du memo, qui disparait
    expect_false(file.exists(file.path(cache_dir, "lidar_mnh", "vides.json")))
  })
})

test_that("28 dalles echues : une seule sonde, encore vide, repousse les 27 autres", {
  withr::with_tempdir({
    cache_dir <- getwd()
    env <- new.env()
    .lancer_meta(cache_dir, 28, env = env)
    expect_length(env$appels, 28)
    .vieillir_memo(cache_dir, 8)
    env$appels <- character(0)
    expect_null(.lancer_meta(cache_dir, 28, env = env))
    expect_length(env$appels, 1)
    # La sonde est la dalle centrale (14e ou 15e sur 28)
    expect_true(env$appels %in% sprintf("LHD_FXX_%04d_6687_MNH.tif", 859:860))
    expect_true(any(grepl("27 autres dalles repouss", env$msgs)))
    m <- jsonlite::read_json(file.path(cache_dir, "lidar_mnh", "vides.json"))
    constats <- as.POSIXct(vapply(m$dalles, `[[`, "", "constat"),
                           format = "%Y-%m-%dT%H:%M:%S")
    expect_length(constats, 28)
    expect_true(all(difftime(Sys.time(), constats, units = "hours") < 1))
    # Et le calcul suivant ne fait aucune requete
    .lancer_meta(cache_dir, 28, env = env)
    expect_length(env$appels, 1)
  })
})

test_that("une sonde pleine declenche le lot et les dalles pleines quittent le memo", {
  withr::with_tempdir({
    cache_dir <- getwd()
    env <- new.env()
    .lancer_meta(cache_dir, 4, env = env)
    .vieillir_memo(cache_dir, 8)
    env$appels <- character(0)
    # Trois dalles publiees, une encore vide
    pleines <- sprintf("LHD_FXX_%04d_6687_MNH.tif", 846:848)
    res <- .lancer_meta(cache_dir, 4, pleines = pleines, env = env)
    expect_length(env$appels, 4)
    m <- jsonlite::read_json(file.path(cache_dir, "lidar_mnh", "vides.json"))
    expect_identical(names(m$dalles), "LHD_FXX_0849_6687_MNH.tif")
    expect_true(all(file.exists(file.path(cache_dir, "lidar_mnh", pleines))))
  })
})

test_that("un memo illisible ou d'un autre format est ignore puis reecrit", {
  withr::with_tempdir({
    cache_dir <- getwd()
    dir.create(file.path(cache_dir, "lidar_mnh"))
    f <- file.path(cache_dir, "lidar_mnh", "vides.json")
    env <- new.env()
    writeLines("{ pas du json", f)
    expect_null(.lancer_meta(cache_dir, 1, env = env))
    expect_length(env$appels, 1)
    expect_equal(jsonlite::read_json(f)$format, 1L)
    jsonlite::write_json(list(format = 99, dalles = list()), f, auto_unbox = TRUE)
    .lancer_meta(cache_dir, 1, env = env)
    expect_length(env$appels, 2)
  })
})

test_that(".lidar_vide_ttl_jours : option, puis variable d'environnement, puis 7", {
  f <- nemetonshiny:::.lidar_vide_ttl_jours
  withr::local_options(nemetonshiny.lidar_vide_ttl_jours = NULL)
  withr::local_envvar(NEMETON_LIDAR_VIDE_TTL_JOURS = NA)
  expect_equal(f(), 7)
  withr::local_envvar(NEMETON_LIDAR_VIDE_TTL_JOURS = "0")
  expect_equal(f(), 0)
  withr::local_envvar(NEMETON_LIDAR_VIDE_TTL_JOURS = "abc")
  expect_equal(f(), 7)
  withr::local_options(nemetonshiny.lidar_vide_ttl_jours = 30)
  expect_equal(f(), 30)
})

test_that("NEMETON_LIDAR_VIDE_TTL_JOURS=0 force le nouvel essai", {
  withr::with_tempdir({
    cache_dir <- getwd()
    env <- new.env()
    .lancer_meta(cache_dir, 1, env = env)
    withr::local_envvar(NEMETON_LIDAR_VIDE_TTL_JOURS = "0")
    .lancer_meta(cache_dir, 1, env = env)
    expect_length(env$appels, 2)
  })
})

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

test_that("une dalle vide n'est pas redemandee au recalcul suivant", {
  withr::with_tempdir({
    cache_dir <- getwd()
    appels <- 0L
    msgs <- character(0)
    lancer <- function() withCallingHandlers(
      with_mocked_bindings(
        query_lidar_wfs = .wfs_une_dalle("http://example.com/0846_6687.tif"),
        download_lidar_tile = function(url, dest_file) {
          appels <<- appels + 1L
          .ecrire_dalle(.dalle_l93(), dest_file)
        },
        nemetonshiny:::download_ign_lidar_hd(.bbox_dalle(), cache_dir, product = "mnh")
      ),
      message = function(m) {
        msgs <<- c(msgs, conditionMessage(m))
        invokeRestart("muffleMessage")
      }
    )
    expect_null(lancer())
    marqueur <- file.path(cache_dir, "lidar_mnh", "0846_6687.tif.vide")
    expect_true(file.exists(marqueur))
    expect_equal(appels, 1L)

    # Recalcul : pas de nouvel appel reseau, toujours NULL, message explicite
    expect_null(lancer())
    expect_equal(appels, 1L)
    expect_true(any(grepl("pas redemand", msgs)))
    expect_true(any(grepl("All 1 LiDAR HD MNH tiles are empty", msgs)))

    # Delai depasse : redemandee
    Sys.setFileTime(marqueur, Sys.time() - 8 * 86400)
    expect_null(lancer())
    expect_equal(appels, 2L)
  })
})

test_that("le marqueur saute quand la dalle est enfin publiee", {
  withr::with_tempdir({
    cache_dir <- getwd()
    dir.create(file.path(cache_dir, "lidar_mnh"))
    marqueur <- file.path(cache_dir, "lidar_mnh", "0846_6687.tif.vide")
    writeLines("2026-10-01T00:00:00", marqueur)
    withr::local_options(nemetonshiny.lidar_vide_jours = 0)
    res <- suppressMessages(with_mocked_bindings(
      query_lidar_wfs = .wfs_une_dalle("http://example.com/0846_6687.tif"),
      download_lidar_tile = function(url, dest_file)
        .ecrire_dalle(.dalle_l93(n = 200L, valeur = 18), dest_file),
      nemetonshiny:::download_ign_lidar_hd(.bbox_dalle(), cache_dir, product = "mnh")
    ))
    expect_s4_class(res, "SpatRaster")
    expect_false(file.exists(marqueur))
  })
})

test_that(".lidar_dalle_vide_recente suit l'age du marqueur et l'option", {
  f <- nemetonshiny:::.lidar_dalle_vide_recente
  withr::with_tempdir({
    expect_false(f("absent.vide"))
    writeLines("x", "d.vide")
    expect_true(f("d.vide"))
    expect_false(f("d.vide", maintenant = Sys.time() + 8 * 86400))
    withr::local_options(nemetonshiny.lidar_vide_jours = 30)
    expect_true(f("d.vide", maintenant = Sys.time() + 8 * 86400))
    withr::local_options(nemetonshiny.lidar_vide_jours = "n'importe quoi")
    expect_true(f("d.vide"))
  })
})

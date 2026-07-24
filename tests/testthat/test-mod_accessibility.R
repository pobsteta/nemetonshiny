# Tests testServer du module Accessibilité (ForêtAccess).
# Le run réel tourne dans un worker `future` (non exécuté sous testServer) : on
# valide ici la résolution d'AOI, les gardes, et le rendu à partir d'un résultat
# posé directement dans `rv`.

.acc_units <- function(n = 2) {
  polys <- lapply(seq_len(n), function(i) {
    x0 <- 6 + i / 1000
    sf::st_polygon(list(rbind(
      c(x0, 46), c(x0 + 0.001, 46), c(x0 + 0.001, 46.001),
      c(x0, 46.001), c(x0, 46))))
  })
  sf::st_sf(ug_id = paste0("U", seq_len(n)),
            geometry = sf::st_sfc(polys, crs = 4326))
}

# Raster de classes factice, au format que produisent les moteurs foretaccess :
# catégoriel, colonnes `value` / `classe`, avec une classe `hors_foret`.
.acc_fake_raster <- function(labs, coltab = NULL) {
  skip_if_not_installed("terra")
  n <- length(labs)
  # Emprise petite et réaliste (Lambert-93) : l'étendue par défaut -180..180
  # fait échouer la reprojection Web-Mercator de leaflet (« warp failure »).
  r <- terra::rast(nrows = 4, ncols = n,
                   xmin = 900000, xmax = 900000 + 5 * n,
                   ymin = 6500000, ymax = 6500020,
                   crs = "EPSG:2154",
                   vals = rep(seq_len(n), each = 4))
  levels(r) <- data.frame(value = seq_len(n), classe = labs)
  if (!is.null(coltab)) terra::coltab(r) <- coltab
  r
}

test_that(".acc_mask_hors_foret : les cellules hors forêt passent à NA", {
  skip_if_not_installed("terra")
  labs <- c("parcourable", "accessible", "non_accessible", "hors_foret")
  r <- .acc_fake_raster(labs)
  expect_equal(terra::global(is.na(r), "sum")[[1]], 0)

  out <- nemetonshiny:::.acc_mask_hors_foret(r, seq_along(labs), labs)
  # Un quart des cellules (la colonne « hors_foret ») doit être masqué.
  expect_equal(terra::global(is.na(out), "sum")[[1]], terra::ncell(r) / 4)
  # Le code de hors_foret varie selon le moteur : repérage par LABEL, pas en dur.
  labs6 <- c("inaccessible", "non_defendable_pente", "defendable_c1",
             "defendable_c2", "defendable_c3", "hors_foret")
  r6 <- .acc_fake_raster(labs6)
  out6 <- nemetonshiny:::.acc_mask_hors_foret(r6, seq_along(labs6), labs6)
  expect_equal(terra::global(is.na(out6), "sum")[[1]], terra::ncell(r6) / 6)
})

test_that(".acc_mask_hors_foret : sans classe hors_foret, raster inchangé", {
  skip_if_not_installed("terra")
  labs <- c("accessible_cable", "non_accessible")
  r <- .acc_fake_raster(labs)
  out <- nemetonshiny:::.acc_mask_hors_foret(r, seq_along(labs), labs)
  expect_equal(terra::global(is.na(out), "sum")[[1]], 0)
})

test_that(".acc_level_colors : couleur SÉMANTIQUE, pas positionnelle", {
  skip_if_not_installed("terra")
  # Régression : « inaccessible » est le niveau 1 du raster DFCI et se peignait
  # donc en VERT avec une palette positionnelle — l'inverse du sens voulu.
  labs <- c("inaccessible", "non_defendable_pente", "defendable_c1",
            "defendable_c2", "defendable_c3", "hors_foret")
  r <- .acc_fake_raster(labs)
  cols <- nemetonshiny:::.acc_level_colors(r, seq_along(labs), labs)
  expect_equal(cols[labs == "inaccessible"], unname(nemetonshiny:::.ACC_CLASS_COLORS[["inaccessible"]]))
  expect_false(identical(cols[labs == "inaccessible"], "#2E7D32"))
  # defendable_c1 (le plus proche) est le vert.
  expect_equal(cols[labs == "defendable_c1"], "#2E7D32")
  # Une classe inconnue retombe sur la palette positionnelle, sans NA.
  lu <- c("parcourable", "classe_inconnue", "hors_foret")
  cu <- nemetonshiny:::.acc_level_colors(.acc_fake_raster(lu), seq_along(lu), lu)
  expect_false(any(is.na(cu)))
  expect_equal(cu[1], "#2E7D32")
})

test_that(".acc_level_colors : la coltab du raster prime (rampe Sylvaccess)", {
  skip_if_not_installed("terra")
  labs <- c("0-250", "250-500", "hors_foret")
  ct <- data.frame(value = 1:3,
                   red = c(26L, 154L, 255L), green = c(158L, 205L, 255L),
                   blue = c(143L, 50L, 255L), alpha = c(255L, 255L, 255L))
  cols <- nemetonshiny:::.acc_level_colors(.acc_fake_raster(labs, ct), 1:3, labs)
  expect_equal(toupper(substr(cols[1], 1, 7)), "#1A9E8F")
})

test_that("régression : le raster rendu comporte bien des pixels transparents", {
  skip_if_not_installed("terra")
  skip_if_not_installed("leaflet")
  skip_if_not_installed("png")
  skip_if_not_installed("base64enc")
  # Garde-fou du bug « hors_foret en blanc opaque » : `colorFactor` +
  # `addRasterImage` perdent le canal alpha d'un `#RRGGBBAA`, donc la
  # transparence DOIT venir du masque NA. On vérifie le PNG réellement émis.
  labs <- c("parcourable", "accessible", "non_accessible", "hors_foret")
  r <- .acc_fake_raster(labs)
  codes <- seq_along(labs)
  cols <- nemetonshiny:::.acc_level_colors(r, codes, labs)
  cols[labs == "hors_foret"] <- "#FFFFFF00"
  r <- nemetonshiny:::.acc_mask_hors_foret(r, codes, labs)
  cmap <- leaflet::colorFactor(cols, domain = codes, na.color = "transparent")
  m <- leaflet::addRasterImage(leaflet::leaflet(), r, colors = cmap,
                               opacity = 0.7, method = "ngb")
  uri <- m$x$calls[[1]]$args[[1]]
  img <- png::readPNG(base64enc::base64decode(
    sub("^data:image/png;base64,", "", uri)))
  expect_equal(dim(img)[3], 4L)              # canal alpha présent
  expect_gt(mean(img[, , 4] == 0), 0.1)      # des pixels réellement transparents
})

test_that("AOI résolu depuis indicators_sf (EPSG:2154)", {
  skip_if_not_installed("sf")
  proj <- list(id = "p1", path = withr::local_tempdir(),
               indicators_sf = .acc_units(2))
  as <- shiny::reactiveValues(current_project = proj)
  testthat::local_mocked_bindings(
    get_app_options = function() list(language = "fr"), .package = "nemetonshiny")

  shiny::testServer(
    nemetonshiny:::mod_accessibility_server,
    args = list(app_state = as),
    {
      aoi <- units_sf()
      expect_s3_class(aoi, "sf")
      expect_equal(sf::st_crs(aoi)$epsg, 2154L)
      expect_equal(nrow(aoi), 2L)
    })
})

test_that("cache d'accessibilité chargé PARESSEUSEMENT (onglet actif, 1x/projet)", {
  skip_if_not_installed("sf")
  proj <- list(id = "p1", path = withr::local_tempdir(),
               indicators_sf = .acc_units(1))
  fake <- list(status = "success", engines = "skidder",
               recaps = list(skidder = data.frame(classe = "parcourable",
                                                  surface_ha = 1)),
               raster_paths = list(skidder = "/x/s.tif",
                                   classes_debardage = "/x/c.tif"))
  loaded <- 0L
  testthat::local_mocked_bindings(
    get_app_options = function() list(language = "fr"),
    .load_cached_accessibility = function(path) { loaded <<- loaded + 1L; fake },
    .package = "nemetonshiny")
  as <- shiny::reactiveValues(current_project = NULL,
                              active_main_tab = "selection",
                              active_terrain_tab = NULL)
  shiny::testServer(
    nemetonshiny:::mod_accessibility_server,
    args = list(app_state = as),
    {
      # 1) Projet chargé mais utilisateur sur « Sélection » : PAS de lecture cache.
      as$current_project <- proj
      session$flushReact()
      expect_equal(loaded, 0L)
      expect_null(rv$result)
      # 2) Ouverture de l'onglet Accessibilité : lecture du cache (une fois).
      as$active_main_tab <- "terrain"
      as$active_terrain_tab <- "accessibility"
      session$flushReact()
      expect_equal(loaded, 1L)
      expect_equal(rv$result$status, "success")
      # 3) Aller-retour sur l'onglet : pas de rechargement (déjà chargé ce projet).
      as$active_terrain_tab <- "sampling"; session$flushReact()
      as$active_terrain_tab <- "accessibility"; session$flushReact()
      expect_equal(loaded, 1L)
    })
})

test_that("run sans projet chargé = no-op gardé (aucun worker lancé)", {
  skip_if_not_installed("sf")
  as <- shiny::reactiveValues(current_project = NULL)
  testthat::local_mocked_bindings(
    get_app_options = function() list(language = "fr"), .package = "nemetonshiny")

  shiny::testServer(
    nemetonshiny:::mod_accessibility_server,
    args = list(app_state = as),
    {
      session$setInputs(engines = c("skidder", "porteur"), run = 1)
      expect_false(isTRUE(rv$running))
      expect_null(rv$result)
    })
})

test_that("résultat posé -> sélecteur de couche rendu", {
  skip_if_not_installed("sf")
  proj <- list(id = "p1", path = withr::local_tempdir(),
               indicators_sf = .acc_units(1))
  as <- shiny::reactiveValues(current_project = proj)
  testthat::local_mocked_bindings(
    get_app_options = function() list(language = "fr"), .package = "nemetonshiny")

  shiny::testServer(
    nemetonshiny:::mod_accessibility_server,
    args = list(app_state = as),
    {
      # Laisser l'observer de purge (déféré sur current_project) s'exécuter à
      # l'init AVANT de poser un résultat, sinon il l'écraserait au 1er flush.
      session$flushReact()
      rv$result <- list(
        status = "success", engines = c("skidder", "porteur"),
        recaps = list(
          skidder = data.frame(classe = "parcourable", surface_ha = 4.0),
          porteur = data.frame(classe = "parcourable", surface_ha = 4.0)),
        raster_paths = list(skidder = "/x/acc_skidder.tif",
                            classes_debardage = "/x/acc_classes_debardage.tif",
                            porteur = "/x/acc_porteur.tif"),
        gpkg_path = NULL)
      session$flushReact()
      # Le sélecteur de couche apparaît une fois un résultat présent (une entrée
      # par raster disponible, dont « classes_debardage »).
      expect_false(is.null(output$layer_ui))
    })
})

test_that("ACCESSFOR systématique : libellé combiné + tableau d'accord auto", {
  skip_if_not_installed("sf")
  skip_if_not_installed("terra")
  tmp <- withr::local_tempdir()
  mk <- function(name) {
    r <- terra::rast(nrows = 4, ncols = 4, xmin = 0, xmax = 4, ymin = 0, ymax = 4,
                     crs = "EPSG:2154", vals = rep(1:4, length.out = 16))
    p <- file.path(tmp, name); terra::writeRaster(r, p, overwrite = TRUE); p
  }
  cdb <- mk("acc_classes_debardage.tif"); afp <- mk("accessfor_skidder.tif")
  proj <- list(id = "p1", path = tmp, indicators_sf = .acc_units(1))
  as <- shiny::reactiveValues(current_project = proj, active_main_tab = "terrain",
                              active_terrain_tab = "accessibility")
  testthat::local_mocked_bindings(
    get_app_options = function() list(language = "fr"), .package = "nemetonshiny")
  shiny::testServer(
    nemetonshiny:::mod_accessibility_server,
    args = list(app_state = as),
    {
      session$flushReact()
      rv$result <- list(
        status = "success", engines = "skidder",
        raster_paths = list(classes_debardage = cdb),
        accessfor_raster_path = afp,
        accessfor = list(status = "success", overall_pct = 19.8, n_cells = 100L,
          table = data.frame(accessfor_class = 3L, libelle = "A1",
                             accord_pct = 19.8, stringsAsFactors = FALSE)))
      session$setInputs(layer = "classes_debardage")
      session$flushReact()
      flat <- function(x) paste(unlist(x), collapse = " ")
      # Le sélecteur nomme la couche « …/ACCESSFOR (IGN) » (raster ACCESSFOR présent).
      expect_true(grepl("ACCESSFOR", flat(output$layer_ui)))
      # Le tableau d'accord s'affiche automatiquement (pas de bouton « Comparer »).
      expect_true(grepl("19", flat(output$accessfor_result)))
    })
})

test_that("accessfor_result : pas d'erreur '$ atomic' sur un résultat rechargé du cache", {
  # Régression : `$accessfor` fait du partial matching et attrapait
  # `accessfor_raster_path` (character) quand la clé `accessfor` est absente
  # (résultat du cache) -> « $ operator is invalid for atomic vectors ». Le fix
  # utilise `[["accessfor"]]` (match exact).
  skip_if_not_installed("sf")
  proj <- list(id = "p1", path = withr::local_tempdir(),
               indicators_sf = .acc_units(1))
  as <- shiny::reactiveValues(current_project = proj)
  testthat::local_mocked_bindings(
    get_app_options = function() list(language = "fr"), .package = "nemetonshiny")
  shiny::testServer(
    nemetonshiny:::mod_accessibility_server,
    args = list(app_state = as),
    {
      session$flushReact()
      # Résultat façon CACHE : accessfor_raster_path présent, PAS de clé `accessfor`.
      rv$result <- list(
        status = "success", engines = "skidder",
        raster_paths = list(classes_debardage = "/x/acc_classes_debardage.tif"),
        accessfor_raster_path = "/x/accessfor_skidder.tif")
      session$flushReact()
      # Ne doit PAS lever d'erreur (rendait « $ operator invalid » avant le fix).
      expect_error(output$accessfor_result, NA)
    })
})

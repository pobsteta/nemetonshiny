# test-desserte_visualisation.R - rendre visibles les sorties de l'onglet Desserte.
#
# Trois des cinq actions du panneau produisaient un GeoPackage que rien
# n'affichait ni n'exportait : il fallait connaitre le chemin du cache et ouvrir
# QGIS pour voir son propre resultat. Ces tests couvrent la CHAINE de mise en
# vue - chemin remonte, cache relu, couches fusionnees a l'export, calques
# declares - et la palette, mesuree et non jugee.

.dv_ligne <- function(x0 = 900000, y0 = 6500000, n = 1L, attrs = NULL) {
  g <- lapply(seq_len(n), function(i) sf::st_linestring(
    rbind(c(x0, y0 + 10 * i), c(x0 + 100, y0 + 10 * i))))
  d <- if (is.null(attrs)) data.frame(id = seq_len(n)) else attrs
  sf::st_sf(d, geometry = sf::st_sfc(g, crs = 2154))
}

.dv_cache <- function() {
  d <- withr::local_tempdir(.local_envir = parent.frame())
  sf::st_write(.dv_ligne(), file.path(d, "desserte.gpkg"), layer = "reseau_cree",
               quiet = TRUE)
  d
}

# --- Export : le telechargement emporte tout le travail de l'onglet ----------

test_that("l'export fusionne les couches optionnelles presentes", {
  skip_if_not_installed("sf")
  cd <- .dv_cache()
  sf::st_write(.dv_ligne(attrs = data.frame(type = "primaire")),
               file.path(cd, "typage_glouton.gpkg"), layer = "reseau_type",
               quiet = TRUE)
  sf::st_write(.dv_ligne(attrs = data.frame(highway = "track")),
               file.path(cd, "desserte_osm.gpkg"), layer = "osm_track", quiet = TRUE)
  sf::st_write(.dv_ligne(attrs = data.frame(CLASSE = "piste_forestiere")),
               file.path(cd, "desserte_detectee.gpkg"), layer = "desserte_detectee",
               quiet = TRUE)

  out <- withr::local_tempfile(fileext = ".gpkg")
  res <- list(gpkg_path = file.path(cd, "desserte.gpkg"), engine = "glouton")
  expect_true(nemetonshiny:::export_desserte_geopackage(res, out))

  lyr <- sf::st_layers(out)$name
  # La couche de base ET les trois optionnelles, dans UN seul fichier.
  expect_true(all(c("reseau_cree", "reseau_type", "osm_track",
                    "desserte_detectee") %in% lyr))
  # Les attributs voyagent : une geometrie sans sa classe ne vaut rien ici.
  det <- sf::st_read(out, layer = "desserte_detectee", quiet = TRUE)
  expect_identical(as.character(det$CLASSE[1]), "piste_forestiere")
})

test_that("l'export n'echoue pas quand les actions optionnelles n'ont pas tourne", {
  skip_if_not_installed("sf")
  cd <- .dv_cache()
  out <- withr::local_tempfile(fileext = ".gpkg")
  expect_true(nemetonshiny:::export_desserte_geopackage(
    list(gpkg_path = file.path(cd, "desserte.gpkg"), engine = "glouton"), out))
  expect_identical(sf::st_layers(out)$name, "reseau_cree")
})

test_that("l'export prend le typage DU MOTEUR COURANT, pas le premier venu", {
  skip_if_not_installed("sf")
  cd <- .dv_cache()
  # Deux moteurs cote a cote : exporter le mauvais serait indetectable a la
  # lecture du fichier - le nom de couche est le meme dans les deux.
  sf::st_write(.dv_ligne(attrs = data.frame(moteur = "glouton")),
               file.path(cd, "typage_glouton.gpkg"), layer = "reseau_type", quiet = TRUE)
  sf::st_write(.dv_ligne(attrs = data.frame(moteur = "steiner")),
               file.path(cd, "typage_steiner.gpkg"), layer = "reseau_type", quiet = TRUE)

  out <- withr::local_tempfile(fileext = ".gpkg")
  nemetonshiny:::export_desserte_geopackage(
    list(gpkg_path = file.path(cd, "desserte.gpkg"), engine = "steiner"), out)
  expect_identical(
    as.character(sf::st_read(out, layer = "reseau_type", quiet = TRUE)$moteur[1]),
    "steiner")
})

test_that("l'export echoue proprement sans GeoPackage source", {
  expect_false(nemetonshiny:::export_desserte_geopackage(list(), tempfile()))
  expect_false(nemetonshiny:::export_desserte_geopackage(
    list(gpkg_path = tempfile(fileext = ".gpkg")), tempfile()))
})

# --- Le typage survit au rechargement du projet ------------------------------

test_that("le typage se relit depuis le cache, comme les quatre autres actions", {
  skip_if_not_installed("sf")
  cd <- withr::local_tempdir()
  gp <- file.path(cd, "typage_glouton.gpkg")
  sf::st_write(.dv_ligne(attrs = data.frame(type = "primaire")), gp,
               layer = "reseau_type", quiet = TRUE)
  saveRDS(list(status = "success", engine = "glouton",
               recap = data.frame(type = "primaire", longueur = 1200),
               gpkg_path = gp, seuils = c(a = 1)),
          file.path(cd, "typage.rds"))

  out <- nemetonshiny:::.load_cached_typage(cd)
  expect_identical(out$engine, "glouton")
  expect_identical(out$gpkg_path, gp)

  # Un sidecar peut survivre au cache qu'il designe : la table reste utilisable,
  # seul le calque disparait.
  unlink(gp)
  out2 <- nemetonshiny:::.load_cached_typage(cd)
  expect_null(out2$gpkg_path)
  expect_s3_class(out2$recap, "data.frame")
})

test_that("run_desserte_typage ECRIT le sidecar que relit le chargeur", {
  # Le chargeur ci-dessus lit un `typage.rds` pose a la main : sans ce test, on
  # pourrait supprimer l'ecriture sans qu'aucune assertion ne tombe, et le typage
  # redeviendrait la seule action a ne pas survivre au rechargement.
  skip_if_not_installed("sf")
  skip_if_not_installed("terra")
  skip_if_not_installed("foretaccess")
  skip_if_not_installed("nemeton")
  cd <- withr::local_tempdir()
  r <- terra::rast(nrows = 4, ncols = 4, xmin = 0, xmax = 4, ymin = 0, ymax = 4,
                   crs = "EPSG:2154", vals = 1)
  # `lignes` non vide : sans lui, la fixture decrit un reseau sans AUCUNE route
  # nouvelle a typer, cas que `run_desserte_typage()` distingue desormais par le
  # statut « empty ». Ce test-ci porte sur la persistance du typage.
  saveRDS(list(reseau = terra::wrap(r),
               lignes = sf::st_sf(id = 1L, geometry = sf::st_sfc(
                 sf::st_linestring(rbind(c(0, 0), c(4, 4))), crs = 2154))),
          file.path(cd, "reseau_obj_glouton.rds"))
  parcelles <- sf::st_sf(
    P1 = 120,
    geometry = sf::st_sfc(sf::st_polygon(list(rbind(
      c(0, 0), c(1, 0), c(1, 1), c(0, 1), c(0, 0)))), crs = 2154))
  troncons <- .dv_ligne(attrs = data.frame(type = "primaire"))

  testthat::local_mocked_bindings(
    vectoriser_reseau = function(...) list(troncons = troncons),
    calculer_flux = function(g, ...) g,
    typer_desserte = function(g, ...) list(
      troncons = troncons,
      recap = data.frame(type = "primaire", longueur = 1200)),
    .package = "foretaccess")
  testthat::local_mocked_bindings(
    volume_mobilisable = function(x, ...) { x$volume_mobilisable <- 42; x },
    .package = "nemeton")

  out <- nemetonshiny:::run_desserte_typage(cd, parcelles, taux_prelevement = 0.5,
                                            horizon_ans = 30)
  expect_identical(out$status, "success")
  expect_true(file.exists(file.path(cd, "typage.rds")))
  # Et ce qui est ecrit est exactement ce que le chargeur sait relire.
  relu <- nemetonshiny:::.load_cached_typage(cd)
  expect_identical(relu$engine, out$engine)
  expect_identical(relu$gpkg_path, out$gpkg_path)
})

test_that("un cache de typage absent ou en echec ne rend rien", {
  cd <- withr::local_tempdir()
  expect_null(nemetonshiny:::.load_cached_typage(cd))
  saveRDS(list(status = "error", reason = "boom"), file.path(cd, "typage.rds"))
  expect_null(nemetonshiny:::.load_cached_typage(cd))
})

# --- Le chemin du GeoPackage OSM remonte avec le resultat --------------------

test_that("run_desserte_osm renvoie le chemin du GeoPackage et le persiste", {
  skip_if_not_installed("sf")
  skip_if_not_installed("foretaccess")
  cd <- withr::local_tempdir()
  aoi <- file.path(cd, "aoi_input.gpkg")
  sf::st_write(sf::st_sf(id = 1L, geometry = sf::st_sfc(
    sf::st_polygon(list(rbind(c(900000, 6500000), c(900500, 6500000),
                              c(900500, 6500500), c(900000, 6500500),
                              c(900000, 6500000)))), crs = 2154)),
    aoi, quiet = TRUE)

  osm <- .dv_ligne(n = 3L, attrs = data.frame(highway = c("track", "track", "path")))
  testthat::local_mocked_bindings(
    acquire_desserte_osm = function(...) osm,
    acquire_desserte = function(...) .dv_ligne(attrs = data.frame(classe = "route")),
    comparer_desserte_osm = function(...) list(
      resume = c(osm_km = 0.3, osm_hors_km = 0.1), corridor_m = 15),
    .package = "foretaccess")

  res <- nemetonshiny:::run_desserte_osm(cd, aoi, buffer_m = 0)
  expect_identical(res$status, "success")
  # Sans ce chemin, le module devrait reconstruire la convention de nommage a la
  # main - et le calque disparaissait au rechargement du projet.
  expect_true(file.exists(res$gpkg_path))
  expect_identical(basename(res$gpkg_path), "desserte_osm.gpkg")
  expect_true("osm_track" %in% sf::st_layers(res$gpkg_path)$name)

  # Et il survit au sidecar : c'est LUI que relit `.load_cached_osm()`.
  relu <- readRDS(file.path(cd, "osm.rds"))
  expect_identical(relu$gpkg_path, res$gpkg_path)
})

# --- Les deux calques : declares, peints, eteints au depart ------------------

test_that("les calques OSM et detection ont leur case et partent eteints", {
  f <- testthat::test_path("..", "..", "R", "mod_desserte.R")
  testthat::skip_if_not(file.exists(f), "sources R absentes (package installe)")
  code <- readLines(f, warn = FALSE)
  code <- code[!grepl("^\\s*#", code)]

  # Declares : un groupe peint mais absent du controle n'a pas de case pour
  # l'eteindre (defaut corrige en 0.122.6, re-arme ici).
  ov <- grep("^\\s*overlays <-", code)
  expect_length(ov, 1L)
  bloc <- paste(code[ov:(ov + 5L)], collapse = " ")
  expect_match(bloc, "DESS_GROUPE_OSM", fixed = TRUE)
  expect_match(bloc, "DESS_GROUPE_DETECTEE", fixed = TRUE)

  # Eteints A LA CREATION : ce sont des diagnostics, et 544 troncons OSM
  # noieraient les 39 routes creees.
  expect_true(any(grepl(
    "hideGroup(c(DESS_GROUPE_OSM, DESS_GROUPE_DETECTEE))", code, fixed = TRUE)))

  # Peints depuis les couches ecrites par les services, pas re-calcules.
  expect_true(any(grepl('layer = "osm_track"', code, fixed = TRUE)))
  expect_true(any(grepl('layer = "desserte_detectee"', code, fixed = TRUE)))
  # Et l'etat de la case reste respecte au re-dessin.
  expect_true(any(grepl("hideGroup(proxy, DESS_GROUPE_OSM)", code, fixed = TRUE)))
  expect_true(any(grepl("hideGroup(proxy, DESS_GROUPE_DETECTEE)", code, fixed = TRUE)))
})

test_that("le calque OSM ne se presente pas comme le gisement manquant", {
  # `comparer_desserte_osm()` ne renvoie AUCUNE geometrie : le GeoPackage porte
  # l'acquisition brute, doublons de la BD TOPO compris. Un libelle " pistes
  # absentes de la BD TOPO " serait donc faux.
  i18n <- nemetonshiny:::get_i18n("fr")
  expect_identical(nemetonshiny:::DESS_GROUPE_OSM, "Pistes OSM")
  expect_match(i18n$t("dess_osm_layer_note"), "doublons")
  expect_false(grepl("absentes", nemetonshiny:::DESS_GROUPE_OSM))
})

# --- Popup de detection : la classe seule serait trompeuse -------------------

test_that("le popup porte la confiance, les criteres et le statut du balisage OSM", {
  i18n <- nemetonshiny:::get_i18n("fr")
  d <- data.frame(CLASSE = c("route_forestiere", "layon_parcellaire"),
                  CLASSE_CONF = c(0.33, NA_real_),
                  CLASSE_MOTIF = c("ouvrage; mineral", ""),
                  OSM_TAGS = c("highway=track", NA_character_),
                  stringsAsFactors = FALSE)
  p <- nemetonshiny:::.dess_detect_popup(d, i18n)

  expect_length(p, 2L)
  # Une classe posee sur peu de criteres ne vaut pas une classe posee sur six :
  # la confiance doit se lire troncon par troncon, pas seulement en moyenne.
  expect_match(p[1], "33 %", fixed = TRUE)
  expect_match(p[1], "ouvrage; mineral", fixed = TRUE)
  # Le balisage est une PROPOSITION, jamais un televersement.
  expect_match(p[1], "highway=track", fixed = TRUE)
  expect_match(p[1], i18n$t("dess_detect_popup_osm_note"), fixed = TRUE)

  # Ce qui manque ne s'invente pas : ni confiance ni motif ni balisage.
  expect_false(grepl("NA", p[2], fixed = TRUE))
  expect_false(grepl(i18n$t("dess_detect_popup_osm_note"), p[2], fixed = TRUE))
})

test_that("une classe inconnue du coeur s'affiche telle quelle, sans avertir", {
  i18n <- nemetonshiny:::get_i18n("fr")
  # Le vocabulaire du coeur peut grandir : mapper l'inconnu sur " autre "
  # masquerait l'arrivee d'une classe neuve, et interroger l'i18n a l'aveugle
  # produirait un avertissement par troncon.
  expect_silent(
    lab <- nemetonshiny:::.dess_detect_classe_label(
      c("route_forestiere", "classe_neuve", NA), i18n))
  expect_identical(lab[2], "classe_neuve")
  expect_identical(lab[3], i18n$t("dess_detect_classe_indetermine"))
})

# --- Palette : mesuree, pas jugee -------------------------------------------

test_that("les classes detectees restent separables, y compris en deuteranopie", {
  skip_if_not_installed("colorspace")
  cols <- nemetonshiny:::DESS_DETECT_COLS
  expect_true(all(names(cols) != ""))

  lab <- function(x) as(colorspace::hex2RGB(unname(x)), "LAB")@coords
  dmin <- function(m) {
    n <- nrow(m); v <- Inf
    for (i in seq_len(n - 1L)) for (j in (i + 1L):n) {
      v <- min(v, sqrt(sum((m[i, ] - m[j, ])^2)))
    }
    v
  }
  # Seuil aligne sur `test-acc_palettes.R` : en deca, deux classes se confondent
  # sur un fond satellite.
  expect_gte(dmin(lab(cols)), 20)
  for (f in list(colorspace::deutan, colorspace::protan, colorspace::tritan)) {
    expect_gte(dmin(lab(f(unname(cols)))), 20)
  }

  # Et separables des AUTRES couches de la meme carte : reseau type, lignes
  # creees, desserte existante, raster, parcelles, pistes OSM.
  autres <- c("#C62828", "#FB8C00", "#2E7D32", "#FF6F00", "#37474F", "#B71C1C",
              "#546E7A", "#1f78b4")
  A <- lab(autres); C <- lab(cols)
  for (i in seq_len(nrow(C))) for (j in seq_len(nrow(A))) {
    expect_gte(sqrt(sum((C[i, ] - A[j, ])^2)), 20)
  }
})

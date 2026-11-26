# Source d'embedding AlphaEarth (mission 080) : tout est simule, aucun reseau.
# `alphaearthnemeton::aef_fetch()` est remplace par `.aef_fetch_fn` ; l'embedding
# est un petit raster de 64 bandes fabrique, ecrit en GeoTIFF comme le fait le
# paquet.

# Raster Lambert-93 de 64 bandes (1200 m x 1200 m, pixel 10 m), ecrit dans `dir`.
.aef_faux_raster <- function(dir, nom = "aef_2025_test.tif") {
  set.seed(1)
  r <- terra::rast(nrows = 120, ncols = 120, nlyrs = 64,
                   xmin = 900000, xmax = 901200, ymin = 6800000, ymax = 6801200,
                   crs = "EPSG:2154")
  terra::values(r) <- matrix(stats::rnorm(120 * 120 * 64), ncol = 64)
  names(r) <- sprintf("A%02d", 0:63)
  f <- file.path(dir, nom)
  terra::writeRaster(r, f, overwrite = TRUE)
  r <- terra::rast(f)
  attr(r, "embedding_source") <- "aef"
  attr(r, "annee") <- 2025L
  r
}

# Emprise : 5 x 5 carres de 200 m dans le raster factice.
.aef_unites <- function() {
  g <- lapply(0:24, function(i) {
    x <- 900000 + (i %% 5) * 220 + 20
    y <- 6800000 + (i %/% 5) * 220 + 20
    sf::st_polygon(list(matrix(c(x, y, x + 200, y, x + 200, y + 200, x, y + 200, x, y),
                               ncol = 2, byrow = TRUE)))
  })
  sf::st_sf(nemeton_id = sprintf("u%02d", 1:25), id = sprintf("u%02d", 1:25),
            N1 = 90, geometry = sf::st_sfc(g, crs = 2154))
}

.aef_projet <- function() {
  d <- tempfile("aef_proj_")
  dir.create(d)
  d
}

test_that("project_aef_enabled : eteint par defaut, allume seulement par TRUE", {
  expect_false(nemetonshiny:::project_aef_enabled(NULL))
  expect_false(nemetonshiny:::project_aef_enabled(list()))
  expect_false(nemetonshiny:::project_aef_enabled(list(aef = list(enabled = FALSE))))
  expect_true(nemetonshiny:::project_aef_enabled(list(aef = list(enabled = TRUE))))
})

test_that("sans le paquet ou option eteinte : la liste des indicateurs ne change pas", {
  base <- nemetonshiny:::list_available_indicators(NULL)
  expect_false("indicateur_n5_distance_reference" %in% base)
  meta_on <- list(aef = list(enabled = TRUE))
  with_mocked_bindings(
    aef_disponible = function() FALSE,
    expect_identical(nemetonshiny:::list_available_indicators(meta_on), base)
  )
  with_mocked_bindings(
    aef_disponible = function() TRUE, {
      expect_identical(nemetonshiny:::list_available_indicators(NULL), base)
      avec <- nemetonshiny:::list_available_indicators(meta_on)
      expect_identical(avec, c(base, "indicateur_n5_distance_reference"))
    }
  )
})

test_that("l'etape est sans effet quand l'option est eteinte ou le paquet absent", {
  appels <- 0L
  couches <- structure(list(rasters = list(), vectors = list()),
                       class = "nemeton_layers")
  with_mocked_bindings(
    .aef_fetch_fn = function() function(...) { appels <<- appels + 1L; NULL },
    aef_disponible = function() TRUE, {
      out <- nemetonshiny:::.aef_etape(couches, .aef_unites(), .aef_projet(),
                                       list(aef = list(enabled = FALSE)))
      expect_identical(out$layers, couches)
      expect_length(out$warnings, 0L)
    })
  with_mocked_bindings(
    .aef_fetch_fn = function() function(...) { appels <<- appels + 1L; NULL },
    aef_disponible = function() FALSE, {
      out <- nemetonshiny:::.aef_etape(couches, .aef_unites(), .aef_projet(),
                                       list(aef = list(enabled = TRUE)))
      expect_identical(out$layers, couches)
    })
  expect_identical(appels, 0L)
})

test_that("l'etape pose la couche aef_embedding et la source `aef`", {
  skip_if_not_installed("terra")
  skip_if_not_installed("sf")
  proj <- .aef_projet()
  couches <- structure(list(rasters = list(), vectors = list()),
                       class = "nemeton_layers")
  with_mocked_bindings(
    aef_disponible = function() TRUE,
    .aef_fetch_fn = function() function(aoi, years, crs, cache_dir, ...) {
      expect_null(years)            # l'annee la plus recente disponible
      expect_identical(crs, 2154)
      expect_true(grepl(file.path("cache", "layers", "aef"), cache_dir, fixed = TRUE))
      .aef_faux_raster(cache_dir)
    }, {
      out <- nemetonshiny:::.aef_etape(couches, .aef_unites(), proj,
                                       list(aef = list(enabled = TRUE)))
      expect_s4_class(out$layers$rasters$aef_embedding, "SpatRaster")
      expect_equal(terra::nlyr(out$layers$rasters$aef_embedding), 64)
      expect_identical(out$layers$embedding_source, "aef")
      expect_length(out$warnings, 0L)
    })
})

test_that("cas 4 : le second calcul relit le cache, sans second appel au paquet", {
  skip_if_not_installed("terra")
  skip_if_not_installed("sf")
  proj <- .aef_projet()
  appels <- 0L
  with_mocked_bindings(
    aef_disponible = function() TRUE,
    .aef_fetch_fn = function() function(aoi, years, crs, cache_dir, ...) {
      appels <<- appels + 1L
      .aef_faux_raster(cache_dir)
    }, {
      a <- nemetonshiny:::build_aef_layer(.aef_unites(), proj)
      b <- nemetonshiny:::build_aef_layer(.aef_unites(), proj)
    })
  expect_identical(appels, 1L)
  expect_false(a$depuis_cache)
  expect_true(b$depuis_cache)
  expect_identical(b$annee, 2025L)
  expect_identical(attr(b$raster, "embedding_source"), "aef")
  expect_equal(terra::nlyr(b$raster), 64)
})

test_that("une autre emprise ne reutilise pas le cache", {
  skip_if_not_installed("terra")
  skip_if_not_installed("sf")
  proj <- .aef_projet()
  appels <- 0L
  with_mocked_bindings(
    aef_disponible = function() TRUE,
    .aef_fetch_fn = function() function(aoi, years, crs, cache_dir, ...) {
      appels <<- appels + 1L
      .aef_faux_raster(cache_dir)
    }, {
      u <- .aef_unites()
      nemetonshiny:::build_aef_layer(u, proj)
      decale <- u
      sf::st_geometry(decale) <- sf::st_geometry(u) + c(5000, 0)
      sf::st_crs(decale) <- 2154
      nemetonshiny:::build_aef_layer(decale, proj)
    })
  expect_identical(appels, 2L)
})

test_that("cas 3 : un echec du telechargement ne leve rien et se dit", {
  proj <- .aef_projet()
  couches <- structure(list(rasters = list(), vectors = list()),
                       class = "nemeton_layers")
  for (echec in list(
    function(...) stop("reseau coupe"),
    function(...) "pas un raster",
    function(...) list()
  )) {
    with_mocked_bindings(
      aef_disponible = function() TRUE,
      .aef_fetch_fn = function() echec, {
        out <- suppressWarnings(nemetonshiny:::.aef_etape(
          couches, .aef_unites(), proj, list(aef = list(enabled = TRUE))))
        expect_null(out$layers$rasters$aef_embedding)
        expect_null(out$layers$embedding_source)
        expect_length(out$warnings, 1L)
        w <- out$warnings[[1]]
        expect_identical(w$key, "aef_fetch_echec")
        expect_true(!is.null(nemetonshiny:::TRANSLATIONS[[w$key]]))
        # le message traduit se forme avec l'argument
        txt <- nemetonshiny:::.message_calcul(w, get_i18n("fr"))
        expect_true(nzchar(txt))
      })
  }
})

test_that("cas 1 : avec la couche, N5 est calcule et B4 / L3 passent en embedding", {
  skip_if_not_installed("terra")
  skip_if_not_installed("sf")
  skip_if_not("indicateur_n5_distance_reference" %in% getNamespaceExports("nemeton"))
  d <- tempfile("aef_cas1_")
  dir.create(d)
  emb <- .aef_faux_raster(d)
  unites <- .aef_unites()
  foret <- sf::st_sf(anciennete = 1,
                     geometry = sf::st_sfc(sf::st_make_valid(sf::st_as_sfc(sf::st_bbox(unites))),
                                           crs = 2154))
  couches <- structure(
    list(rasters = list(aef_embedding = emb), vectors = list(foret_ancienne = foret),
         embedding_source = "aef"),
    class = "nemeton_layers")

  n5 <- nemetonshiny:::compute_single_indicator(
    "indicateur_n5_distance_reference", unites, couches)
  expect_identical(unique(attr(n5, "nemeton_status")), "calculated")
  expect_true(any(is.finite(as.numeric(n5))))

  b4 <- nemetonshiny:::compute_single_indicator("indicateur_b4_div_spectrale",
                                                unites, couches)
  expect_identical(unique(attr(b4, "nemeton_status")), "calculated_embedding")
  l3 <- nemetonshiny:::compute_single_indicator("indicateur_l3_het_spectrale",
                                                unites, couches)
  expect_identical(unique(attr(l3, "nemeton_status")), "calculated_embedding")
})

test_that("cas 3 (cote coeur) : sans couche, N5 sort skipped_no_embedding", {
  skip_if_not_installed("sf")
  skip_if_not("indicateur_n5_distance_reference" %in% getNamespaceExports("nemeton"))
  couches <- structure(list(rasters = list(), vectors = list()),
                       class = "nemeton_layers")
  n5 <- suppressMessages(nemetonshiny:::compute_single_indicator(
    "indicateur_n5_distance_reference", .aef_unites(), couches))
  expect_identical(unique(attr(n5, "nemeton_status")), "skipped_no_embedding")
  expect_true(all(is.na(as.numeric(n5))))
})

test_that("N5 lit N1 des unites : .COMPOSANTES_LUES le relie a l'indicateur N1", {
  expect_identical(
    nemetonshiny:::.COMPOSANTES_LUES$indicateur_n5_distance_reference,
    c(N1 = "indicateur_n1_distance"))
  parcels <- .aef_unites()
  parcels$N1 <- NULL
  res <- data.frame(indicateur_n1_distance = seq_len(25) * 1.0)
  out <- nemetonshiny:::.units_for_indicator(
    "indicateur_n5_distance_reference", parcels, res, list())
  expect_identical(out$N1, seq_len(25) * 1.0)
})

test_that("les cles i18n AlphaEarth existent en FR et EN et tiennent en 30 caracteres", {
  cles <- c("aef_section", "aef_enable", "aef_hint", "aef_save", "aef_saved",
            "aef_need_pkg", "aef_active", "aef_none", "aef_fetch_echec",
            "task_aef_embedding", "aef_taille_avertissement",
            "aef_annee_utilisee", "aef_annee_n5", "aef_annee_etiq")
  tr <- nemetonshiny:::TRANSLATIONS
  for (k in cles) {
    expect_lte(nchar(k), 30L)
    expect_true(nzchar(tr[[k]]$fr), info = k)
    expect_true(nzchar(tr[[k]]$en), info = k)
  }
  expect_identical(
    nemetonshiny:::translate_task_message("aef_embedding", get_i18n("en")),
    get_i18n("en")$t("task_aef_embedding"))
})

# ---- start_computation : de bout en bout, tout simule ---------------------

.lance_calcul_aef <- function(actif, fetch, cote = 0.001) {
  temp_dir <- tempfile("nemeton_aef_")
  project_dir <- file.path(temp_dir, "test_project")
  dir.create(file.path(project_dir, "data"), recursive = TRUE)
  geom <- sf::st_sfc(
    sf::st_polygon(list(matrix(c(0, 0, cote, 0, cote, cote, 0, cote, 0, 0),
                               ncol = 2, byrow = TRUE))), crs = 4326)
  sf::st_write(sf::st_sf(id = "p1", geometry = geom),
               file.path(project_dir, "data", "parcels.gpkg"), quiet = TRUE)
  vu <- new.env()
  vu$attr <- "pas_appele"
  vu$maj <- list()
  vu$emb <- "pas_appele"
  vu$indic <- NULL
  vu$taches <- character(0)
  vu$erreurs <- NULL
  with_mocked_bindings(
    get_project_path = function(id) project_dir,
    get_app_options = function() list(project_dir = temp_dir),
    clear_cancel = function(id) invisible(NULL),
    get_computation_progress = function(id) list(computed_indicators = character(0)),
    update_project_status = function(id, status) invisible(NULL),
    project_aef_active = function(metadata) actif,
    aef_disponible = function() TRUE,
    .aef_fetch_fn = function() fetch,
    download_layers_for_parcels = function(parcels, project_path, progress_callback, annule = NULL) {
      structure(list(rasters = list(), vectors = list(), point_clouds = list(),
                     bbox = c(0, 0, 1, 1), crs = sf::st_crs(4326),
                     cache_dir = tempdir(), warnings = list()),
                class = "nemeton_layers")
    },
    is_cancelled = function(id) FALSE,
    compute_all_indicators = function(parcels, layers, indicators, progress_callback, project_id) {
      vu$attr <- attr(parcels, "embedding_source")
      vu$emb <- layers$rasters$aef_embedding
      vu$indic <- indicators
      parcels$indicateur_c1_biomasse <- 75.5
      parcels
    },
    save_indicators = function(project_id, results) TRUE,
    update_project_metadata = function(id, updates, ...) {
      vu$maj <- c(vu$maj, list(updates)); invisible(NULL)
    },
    {
      res <- nemetonshiny:::start_computation(
        "test_project", indicators = "indicateur_c1_biomasse")
      vu$erreurs <- res$state$errors
    })
  vu
}

test_that("start_computation : avec la source, couche, attribut, metadonnees et badge", {
  skip_if_not_installed("sf")
  skip_if_not_installed("terra")
  vu <- .lance_calcul_aef(TRUE, function(aoi, years, crs, cache_dir, ...) {
    .aef_faux_raster(cache_dir)
  })
  expect_s4_class(vu$emb, "SpatRaster")
  expect_identical(vu$attr, "aef")
  maj <- Filter(function(u) "chm_source" %in% names(u), vu$maj)
  expect_identical(maj[[1]]$embedding_source, "aef")
  expect_true("embedding_ml" %in% nemetonshiny:::project_augmented_flags(
    list(metadata = list(embedding_source = maj[[1]]$embedding_source))))
})

test_that("start_computation : fetch en erreur, calcul termine, message, pas de drapeau", {
  skip_if_not_installed("sf")
  vu <- suppressWarnings(.lance_calcul_aef(TRUE, function(...) stop("reseau coupe")))
  expect_null(vu$emb)
  expect_null(vu$attr)
  maj <- Filter(function(u) "chm_source" %in% names(u), vu$maj)
  expect_null(maj[[1]]$embedding_source)
  cles <- vapply(vu$erreurs, function(e) as.character(e$key %||% ""), "")
  expect_true("aef_fetch_echec" %in% cles)
})

test_that("start_computation : source eteinte, rien ne change et le paquet n'est pas appele", {
  skip_if_not_installed("sf")
  appels <- 0L
  vu <- .lance_calcul_aef(FALSE, function(...) { appels <<- appels + 1L; NULL })
  expect_identical(appels, 0L)
  expect_null(vu$emb)
  expect_null(vu$attr)
  expect_length(vu$erreurs, 0L)
})

test_that("start_computation : grande emprise, l'avertissement est dans les messages et le calcul continue", {
  skip_if_not_installed("sf")
  skip_if_not_installed("terra")
  ordre <- character(0)
  vu <- .lance_calcul_aef(TRUE, function(aoi, years, crs, cache_dir, ...) {
    ordre <<- c(ordre, "fetch")
    .aef_faux_raster(cache_dir)
  }, cote = 0.2)   # environ 20 km x 15 km en France : plus de 100 km2
  cles <- vapply(vu$erreurs, function(e) as.character(e$key %||% ""), "")
  expect_true("aef_taille_avertissement" %in% cles)
  expect_identical(ordre, "fetch")
  expect_identical(vu$attr, "aef")
  # Petite emprise : pas d'avertissement.
  vu2 <- .lance_calcul_aef(TRUE, function(aoi, years, crs, cache_dir, ...) {
    .aef_faux_raster(cache_dir)
  })
  expect_false("aef_taille_avertissement" %in%
                 vapply(vu2$erreurs, function(e) as.character(e$key %||% ""), ""))
})

# ---- Relecture : arret, progression, reprise ------------------------------

test_that("arret demande apres les telechargements : AlphaEarth n'est pas appele", {
  skip_if_not_installed("sf")
  appels <- 0L
  temp_dir <- tempfile("nemeton_aef_stop_")
  project_dir <- file.path(temp_dir, "test_project")
  dir.create(file.path(project_dir, "data"), recursive = TRUE)
  geom <- sf::st_sfc(
    sf::st_polygon(list(matrix(c(0, 0, 0.001, 0, 0.001, 0.001, 0, 0.001, 0, 0),
                               ncol = 2, byrow = TRUE))), crs = 4326)
  sf::st_write(sf::st_sf(id = "p1", geometry = geom),
               file.path(project_dir, "data", "parcels.gpkg"), quiet = TRUE)
  vu <- new.env()
  vu$telecharge <- FALSE
  res <- NULL
  with_mocked_bindings(
    get_project_path = function(id) project_dir,
    get_app_options = function() list(project_dir = temp_dir),
    clear_cancel = function(id) invisible(NULL),
    get_computation_progress = function(id) list(computed_indicators = character(0)),
    update_project_status = function(id, status) invisible(NULL),
    project_aef_active = function(metadata) TRUE,
    aef_disponible = function() TRUE,
    .aef_fetch_fn = function() function(...) { appels <<- appels + 1L; NULL },
    download_layers_for_parcels = function(parcels, project_path, progress_callback, annule = NULL) {
      vu$telecharge <- TRUE   # l'arret est demande pendant le telechargement
      structure(list(rasters = list(), vectors = list(), point_clouds = list(),
                     bbox = c(0, 0, 1, 1), crs = sf::st_crs(4326),
                     cache_dir = tempdir(), warnings = list()),
                class = "nemeton_layers")
    },
    is_cancelled = function(id) isTRUE(vu$telecharge),
    update_project_metadata = function(id, updates, ...) invisible(NULL),
    {
      res <- nemetonshiny:::start_computation(
        "test_project", indicators = "indicateur_c1_biomasse")
    })
  expect_identical(appels, 0L)
  expect_true(isTRUE(res$cancelled))
})

test_that("N5 compte dans computed_indicators du fichier de progression", {
  skip_if_not_installed("sf")
  skip_if_not_installed("arrow")
  d <- tempfile("aef_prog_")
  dir.create(file.path(d, "data"), recursive = TRUE)
  g <- sf::st_sfc(
    sf::st_polygon(list(matrix(c(0, 0, 1, 0, 1, 1, 0, 1, 0, 0), ncol = 2, byrow = TRUE))),
    crs = 4326)
  res <- sf::st_sf(id = "p1", indicateur_c1_biomasse = 50,
                   indicateur_n5_distance_reference = 70, geometry = g)
  with_mocked_bindings(
    get_project_path = function(id) d, {
      expect_true(nemetonshiny:::save_indicators_incremental(
        "p", res, "indicateur_n5_distance_reference"))
    })
  prog <- jsonlite::read_json(file.path(d, "data", "compute_progress.json"),
                              simplifyVector = TRUE)
  expect_true("indicateur_n5_distance_reference" %in% prog$computed_indicators)
  expect_true("indicateur_c1_biomasse" %in% prog$computed_indicators)
})

# Reprise : colonnes deja remplies, recalcul demande.
.aef_reprise <- function(etat_stocke, layers) {
  skip_if_not_installed("sf")
  d <- tempfile("aef_reprise_")
  dir.create(file.path(d, "data"), recursive = TRUE)
  if (!is.null(etat_stocke)) {
    jsonlite::write_json(list(etat = etat_stocke),
                         file.path(d, "data", "aef_etat.json"), auto_unbox = TRUE)
  }
  unites <- .aef_unites()
  ind <- c("indicateur_b4_div_spectrale", "indicateur_l3_het_spectrale",
           "indicateur_c2_ndvi")
  stocke <- sf::st_drop_geometry(unites)
  for (i in ind) stocke[[i]] <- 1
  calcules <- character(0)
  with_mocked_bindings(
    get_project_path = function(id) d,
    load_indicators = function(id) stocke,
    is_cancelled = function(id) FALSE,
    compute_single_indicator = function(indicator, parcels, layers) {
      calcules <<- c(calcules, indicator)
      rep(2, nrow(parcels))
    },
    save_indicators_incremental = function(...) TRUE,
    {
      suppressMessages(nemetonshiny:::compute_all_indicators(
        unites, layers, ind, project_id = "p"))
    })
  list(calcules = calcules, etat = jsonlite::read_json(
    file.path(d, "data", "aef_etat.json"), simplifyVector = TRUE)$etat)
}

.aef_couches <- function(avec) {
  l <- structure(list(rasters = list(), vectors = list()), class = "nemeton_layers")
  if (avec) {
    r <- terra::rast(nrows = 2, ncols = 2, nlyrs = 2, crs = "EPSG:2154")
    attr(r, "annee") <- 2025L
    l$rasters$aef_embedding <- r
    l$embedding_source <- "aef"
  }
  l
}

test_that("reprise : activer AlphaEarth sur un projet deja calcule recalcule B4 et L3", {
  skip_if_not_installed("terra")
  r <- .aef_reprise(NULL, .aef_couches(TRUE))   # pas de marqueur = sans source
  expect_setequal(r$calcules, c("indicateur_b4_div_spectrale",
                                "indicateur_l3_het_spectrale"))
  expect_identical(r$etat, "aef:2025")
})

test_that("reprise : couper la source, ou changer d'annee, recalcule aussi", {
  skip_if_not_installed("terra")
  r <- .aef_reprise("aef:2025", .aef_couches(FALSE))
  expect_setequal(r$calcules, c("indicateur_b4_div_spectrale",
                                "indicateur_l3_het_spectrale"))
  expect_identical(r$etat, "none")
  r <- .aef_reprise("aef:2024", .aef_couches(TRUE))
  expect_length(r$calcules, 2L)
})

test_that("reprise : etat inchange, ou projet sans AlphaEarth, rien n'est recalcule", {
  skip_if_not_installed("terra")
  expect_length(.aef_reprise("aef:2025", .aef_couches(TRUE))$calcules, 0L)
  expect_length(.aef_reprise(NULL, .aef_couches(FALSE))$calcules, 0L)
  expect_length(.aef_reprise("none", .aef_couches(FALSE))$calcules, 0L)
})


# ---- Avertissement de taille avant telechargement (decision du 2026-11-26) ---

# Emprise carree de `cote` metres, en Lambert-93.
.aef_unites_cote <- function(cote) {
  g <- sf::st_polygon(list(matrix(
    c(900000, 6800000, 900000 + cote, 6800000, 900000 + cote, 6800000 + cote,
      900000, 6800000 + cote, 900000, 6800000), ncol = 2, byrow = TRUE)))
  sf::st_sf(id = "u1", geometry = sf::st_sfc(g, crs = 2154))
}

test_that("le seuil d'avertissement est une constante nommee de 100 km2", {
  expect_identical(nemetonshiny:::AEF_SEUIL_KM2, 100)
  expect_identical(nemetonshiny:::AEF_MO_PAR_KM2, 1)
})

test_that("emprise au-dela de 100 km2 : l'avertissement part AVANT l'appel au paquet", {
  skip_if_not_installed("terra")
  skip_if_not_installed("sf")
  proj <- .aef_projet()
  ordre <- character(0)
  couches <- structure(list(rasters = list(), vectors = list()),
                       class = "nemeton_layers")
  with_mocked_bindings(
    aef_disponible = function() TRUE,
    .aef_fetch_fn = function() function(aoi, years, crs, cache_dir, ...) {
      ordre <<- c(ordre, "fetch")
      .aef_faux_raster(cache_dir)
    }, {
      out <- nemetonshiny:::.aef_etape(
        couches, .aef_unites_cote(12000), proj,
        list(aef = list(enabled = TRUE)),
        prevenir = function(w) {
          ordre <<- c(ordre, "avertissement")
          expect_identical(w$key, "aef_taille_avertissement")
          expect_identical(w$args[[1]], "144")   # surface en km2
          expect_identical(w$args[[3]], "144")   # ~1 Mo par km2
        })
      expect_identical(ordre, c("avertissement", "fetch"))
      # Le telechargement n'est pas bloque.
      expect_identical(out$layers$embedding_source, "aef")
      # Le texte traduit porte surface, seuil et taille.
      txt <- nemetonshiny:::.message_calcul(
        list(key = "aef_taille_avertissement", args = list("144", "100", "144")),
        get_i18n("fr"))
      expect_match(txt, "144 km", fixed = TRUE)
      expect_match(txt, "100 km", fixed = TRUE)
      expect_match(txt, "144 Mo", fixed = TRUE)
    })
})

test_that("sans `prevenir`, l'avertissement revient dans les warnings de l'etape", {
  skip_if_not_installed("terra")
  skip_if_not_installed("sf")
  couches <- structure(list(rasters = list(), vectors = list()),
                       class = "nemeton_layers")
  with_mocked_bindings(
    aef_disponible = function() TRUE,
    .aef_fetch_fn = function() function(aoi, years, crs, cache_dir, ...) {
      .aef_faux_raster(cache_dir)
    }, {
      out <- nemetonshiny:::.aef_etape(couches, .aef_unites_cote(12000),
                                       .aef_projet(),
                                       list(aef = list(enabled = TRUE)))
      expect_identical(vapply(out$warnings, `[[`, "", "key"),
                       "aef_taille_avertissement")
    })
})

test_that("emprise de 100 km2 ou moins, ou deja en cache : aucun avertissement", {
  skip_if_not_installed("terra")
  skip_if_not_installed("sf")
  # Pile au seuil (10 km x 10 km) : pas d'avertissement (strictement au-dela).
  expect_null(nemetonshiny:::.aef_avertissement_taille(.aef_unites_cote(10000),
                                                       .aef_projet()))
  expect_null(nemetonshiny:::.aef_avertissement_taille(.aef_unites(), .aef_projet()))
  # Grande emprise deja acquise : rien a telecharger, rien a annoncer.
  proj <- .aef_projet()
  vus <- 0L
  with_mocked_bindings(
    aef_disponible = function() TRUE,
    .aef_fetch_fn = function() function(aoi, years, crs, cache_dir, ...) {
      .aef_faux_raster(cache_dir)
    }, {
      nemetonshiny:::build_aef_layer(.aef_unites_cote(12000), proj)
      expect_null(nemetonshiny:::.aef_avertissement_taille(
        .aef_unites_cote(12000), proj))
    })
})

# ---- Annee AlphaEarth affichee (decision du 2026-11-26) ---------------------

test_that("aef_annee : lue dans le cache (courant.json), sinon dans l'attribut", {
  skip_if_not_installed("terra")
  skip_if_not_installed("sf")
  proj <- .aef_projet()
  expect_true(is.na(nemetonshiny:::aef_annee(proj)))
  with_mocked_bindings(
    aef_disponible = function() TRUE,
    .aef_fetch_fn = function() function(aoi, years, crs, cache_dir, ...) {
      .aef_faux_raster(cache_dir)
    }, nemetonshiny:::build_aef_layer(.aef_unites(), proj))
  expect_identical(nemetonshiny:::aef_annee(proj), 2025L)
  # L'attribut du raster prime, et sert sans cache.
  r <- terra::rast(nrows = 2, ncols = 2)
  attr(r, "annee") <- 2023L
  expect_identical(nemetonshiny:::aef_annee(NULL, r), 2023L)
  expect_identical(nemetonshiny:::aef_annee(proj, r), 2023L)
  expect_true(is.na(nemetonshiny:::aef_annee(NULL, NULL)))
})

test_that("l'annee s'affiche a cote de N5 seulement, et seulement si connue", {
  i18n <- get_i18n("fr")
  d <- data.frame(indicateur_n5_distance_reference = c(0.2, NA),
                  indicateur_b4_div_spectrale = c(1, 2))
  b <- nemetonshiny:::n5_annee_banner(d, "indicateur_n5_distance_reference",
                                      2025L, i18n)
  expect_match(as.character(b), "AlphaEarth de l'ann", fixed = TRUE)
  expect_match(as.character(b), "2025", fixed = TRUE)
  expect_null(nemetonshiny:::n5_annee_banner(d, "indicateur_b4_div_spectrale",
                                             2025L, i18n))
  expect_null(nemetonshiny:::n5_annee_banner(d, "indicateur_n5_distance_reference",
                                             NA_integer_, i18n))
  d$indicateur_n5_distance_reference <- NA_real_
  expect_null(nemetonshiny:::n5_annee_banner(d, "indicateur_n5_distance_reference",
                                             2025L, i18n))
  expect_identical(nemetonshiny:::.n5_annee_etiquette(
    "indicateur_n5_distance_reference_norm", 2025L, i18n), "AlphaEarth 2025")
  expect_identical(nemetonshiny:::.n5_annee_etiquette(
    "indicateur_b4_div_spectrale", 2025L, i18n), "")
})

test_that("Sources & parametres dit l'annee utilisee quand elle est connue", {
  skip_if_not_installed("terra")
  skip_if_not_installed("sf")
  proj <- .aef_projet()
  with_mocked_bindings(
    aef_disponible = function() TRUE,
    .aef_fetch_fn = function() function(aoi, years, crs, cache_dir, ...) {
      .aef_faux_raster(cache_dir)
    }, nemetonshiny:::build_aef_layer(.aef_unites(), proj))
  rendre <- function(path, enabled) {
    html <- NULL
    testthat::local_mocked_bindings(aef_disponible = function() TRUE)
    shiny::testServer(nemetonshiny:::mod_sources_config_server, args = list(
      app_state = shiny::reactiveValues(
        language = "fr", project_id = "p1",
        current_project = list(id = "p1", path = path,
                               metadata = list(aef = list(enabled = enabled))))
    ), {
      session$setInputs(x = 1)
      html <<- paste(as.character(output$aef_block), collapse = " ")
    })
    html
  }
  attendu <- sprintf(get_i18n("fr")$t("aef_annee_utilisee"), 2025L)
  expect_true(grepl(attendu, rendre(proj, TRUE), fixed = TRUE))
  # Source eteinte, ou aucune acquisition : pas d'annee affichee.
  expect_false(grepl(attendu, rendre(proj, FALSE), fixed = TRUE))
  expect_false(grepl("Ann\u00e9e AlphaEarth", rendre(.aef_projet(), TRUE), fixed = TRUE))
})

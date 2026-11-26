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
            "task_aef_embedding")
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

.lance_calcul_aef <- function(actif, fetch) {
  temp_dir <- tempfile("nemeton_aef_")
  project_dir <- file.path(temp_dir, "test_project")
  dir.create(file.path(project_dir, "data"), recursive = TRUE)
  geom <- sf::st_sfc(
    sf::st_polygon(list(matrix(c(0, 0, 0.001, 0, 0.001, 0.001, 0, 0.001, 0, 0),
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

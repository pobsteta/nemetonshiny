# Mission 022 : " Aucune alerte dans cette strate " (FAST, FORDEAD, RECONFORT).
#
# Rasters des tests : EPSG:2154, origine (0, 0), ordre row-major depuis le coin
# haut-gauche (comme terra::values). Pixels de 100 m (1 ha) pour que le rappel
# en hectares soit lisible.

.s022_rast <- function(vals, nr = 2, nc = 2, res = 100) {
  terra::rast(nrows = nr, ncols = nc, crs = "EPSG:2154",
              xmin = 0, xmax = nc * res, ymin = 0, ymax = nr * res,
              vals = vals)
}

.s022_html <- function(x) paste(as.character(x$html %||% x %||% ""), collapse = "")

# --- Helper ---------------------------------------------------------------

test_that("etat: les 3 etats pour chacun des 3 moteurs", {
  skip_if_not_installed("terra")
  # tot : 3 pixels en alerte pour FAST / FORDEAD (classes >= 1), 2 pour
  # RECONFORT (classes 2-3 ; la classe 1 est saine).
  cas <- list(
    fast      = list(tot = c(0, 1, 2, 4), sain = 0, alerte = 1, n = 3),
    fordead   = list(tot = c(0, 1, 2, 4), sain = 0, alerte = 3, n = 3),
    reconfort = list(tot = c(1, 2, 3, 1), sain = 1, alerte = 2, n = 2)
  )
  for (m in names(cas)) {
    k <- cas[[m]]
    tot <- .s022_rast(k$tot)
    # Strate = premier pixel seul (les autres NA).
    strate_saine   <- .s022_rast(c(k$sain, NA, NA, NA))
    strate_alerte  <- .s022_rast(c(k$alerte, NA, NA, NA))

    # 1. alertes dans la strate -> rien de nouveau.
    expect_identical(
      nemetonshiny:::.etat_strate_sante(strate_alerte, tot, m)$etat,
      "alertes_strate", info = m)
    # 2. strate vide, _tot en alerte -> nouvel etat + compte et surface.
    e <- nemetonshiny:::.etat_strate_sante(strate_saine, tot, m)
    expect_identical(e$etat, "strate_vide_tot_alerte", info = m)
    expect_equal(e$n_tot, k$n, info = m)
    expect_equal(e$ha_tot, k$n * 1, info = m)   # 1 ha par pixel
    # 3. strate vide, _tot sain aussi -> comportement actuel.
    tot_sain <- .s022_rast(rep(k$sain, 4))
    expect_identical(
      nemetonshiny:::.etat_strate_sante(strate_saine, tot_sain, m)$etat,
      "tout_sain", info = m)
    # Strate = _tot (ou pas de _tot) : r_tot NULL -> inchange.
    expect_identical(
      nemetonshiny:::.etat_strate_sante(strate_saine, NULL, m)$etat,
      "tout_sain", info = m)
    expect_identical(
      nemetonshiny:::.etat_strate_sante(strate_alerte, NULL, m)$etat,
      "alertes_strate", info = m)
  }
})

test_that("etat: RECONFORT ignore la classe 1, FORDEAD compte le sol nu (classe 4)", {
  skip_if_not_installed("terra")
  tot <- .s022_rast(c(1, 1, 1, 1))
  strate <- .s022_rast(c(1, NA, NA, NA))
  expect_identical(
    nemetonshiny:::.etat_strate_sante(strate, tot, "reconfort")$etat, "tout_sain")
  sol_nu <- .s022_rast(c(4, NA, NA, NA))
  expect_identical(
    nemetonshiny:::.etat_strate_sante(sol_nu, tot, "fordead")$etat,
    "alertes_strate")
})

test_that("etat: strate entierement NA ou raster invalide -> comportement actuel", {
  skip_if_not_installed("terra")
  tot <- .s022_rast(c(0, 1, 2, 4))
  vide <- .s022_rast(rep(NA_real_, 4))
  for (m in c("fast", "fordead", "reconfort")) {
    expect_identical(nemetonshiny:::.etat_strate_sante(vide, tot, m)$etat,
                     "tout_sain", info = m)
    expect_identical(nemetonshiny:::.etat_strate_sante(NULL, tot, m)$etat,
                     "tout_sain", info = m)
  }
})

test_that("rappel: pixels et hectares formates par langue", {
  etat <- list(etat = "strate_vide_tot_alerte", n_tot = 1234, ha_tot = 12.34)
  fr <- nemetonshiny:::.sante_rappel_tot(get_i18n("fr"), etat)
  expect_match(fr, "1 234 pixels", fixed = TRUE)
  expect_match(fr, "12,3 ha", fixed = TRUE)
  en <- nemetonshiny:::.sante_rappel_tot(get_i18n("en"), etat)
  expect_match(en, "1,234 alert pixels", fixed = TRUE)
  expect_match(en, "12.3 ha", fixed = TRUE)
})

test_that("cles i18n de la mission 022: FR/EN, <= 30 caracteres", {
  for (k in c("sante_strate_sans_alerte_titre", "sante_strate_rappel_tot")) {
    expect_lte(nchar(k), 30L)
    expect_false(identical(get_i18n("fr")$t(k), k))
    expect_false(identical(get_i18n("en")$t(k), k))
  }
})

# --- FAST -----------------------------------------------------------------

.s022_fast_project <- function(env = parent.frame()) {
  proj <- list(path = withr::local_tempdir(.local_envir = env))
  cd <- file.path(proj$path, "cache", "layers", "sentinel2")
  dir.create(cd, recursive = TRUE)
  sid <- "S2A_MSIL2A_20250610T103021_T31TGM"
  dir.create(file.path(cd, sid))
  file.create(file.path(cd, sid, "B05.tif"))
  proj
}

# Masque 2x2 de 200 m : TL=v[1], TR=v[2], BL=v[3], BR=v[4]. La strate (id 6)
# ne couvre que le pixel haut-gauche.
.s022_fast_harness <- function(vals, env = parent.frame()) {
  mask_tif <- withr::local_tempfile(fileext = ".tif", .local_envir = env)
  terra::writeRaster(.s022_rast(vals), mask_tif)
  aoi <- sf::st_sf(zone_id = 6L, geometry = sf::st_sfc(
    sf::st_polygon(list(rbind(c(10, 110), c(90, 110), c(90, 190), c(10, 190),
                              c(10, 110)))), crs = 2154))
  testthat::local_mocked_bindings(
    get_monitoring_db_connection   = function(...) "CON",
    close_monitoring_db_connection = function(...) invisible(NULL),
    get_monitoring_zone_aoi        = function(...) aoi,
    .compute_fast_mask             = function(...) mask_tif,
    .env = env
  )
}

.s022_fast_args <- function(zone, run_zone, env = parent.frame()) {
  args <- list(
    app_state = shiny::reactiveValues(
      language = "fr", active_main_tab = "monitoring",
      current_project = .s022_fast_project(env)),
    zone_id_r = zone,
    date_range_r = shiny::reactive(as.Date(c("2017-01-01", "2025-12-31"))),
    thresholds_r = shiny::reactive(list(ndvi = 0.6, nbr = 0.5, ndmi = 0.2,
                                        window_days = 30L))
  )
  if (!is.null(run_zone)) args$run_zone_id_r <- run_zone
  args
}

test_that("FAST: strate sans alerte mais _tot en alerte -> bandeau info + rappel", {
  skip_if_not_installed("shiny"); skip_if_not_installed("terra")
  .s022_fast_harness(c(0, 1, 2, 4))
  shiny::testServer(
    nemetonshiny:::mod_monitoring_fast_alerts_server,
    args = .s022_fast_args(shiny::reactive("6"), shiny::reactive("5")),
    {
      session$setInputs(mode = "count", index = "NDVI")
      session$elapse(500); session$flushReact()
      i18n <- get_i18n("fr")
      html <- .s022_html(output$banner)
      expect_match(html, "alert-info", fixed = TRUE)
      expect_match(html, "info-circle", fixed = TRUE)
      expect_false(grepl("alert-success", html, fixed = TRUE))
      expect_match(html, i18n$t("sante_strate_sans_alerte_titre"), fixed = TRUE)
      expect_match(html, "3 pixels", fixed = TRUE)
      expect_match(html, "3,0 ha", fixed = TRUE)
    }
  )
})

test_that("FAST: strate = _tot sans alerte -> bandeau vert inchange", {
  skip_if_not_installed("shiny"); skip_if_not_installed("terra")
  .s022_fast_harness(c(0, 0, 0, 0))
  shiny::testServer(
    nemetonshiny:::mod_monitoring_fast_alerts_server,
    args = .s022_fast_args(shiny::reactive("5"), shiny::reactive("5")),
    {
      session$setInputs(mode = "count", index = "NDVI")
      session$elapse(500); session$flushReact()
      html <- .s022_html(output$banner)
      expect_match(html, "alert-success", fixed = TRUE)
      expect_match(html, get_i18n("fr")$t("monitoring_fast_alerts_empty_title"),
                   fixed = TRUE)
      expect_false(grepl("alert-info", html, fixed = TRUE))
    }
  )
})

test_that("FAST: strate vide ET _tot vide -> bandeau vert; sans _tot -> vert", {
  skip_if_not_installed("shiny"); skip_if_not_installed("terra")
  .s022_fast_harness(c(0, 0, 0, 0))
  shiny::testServer(
    nemetonshiny:::mod_monitoring_fast_alerts_server,
    args = .s022_fast_args(shiny::reactive("6"), shiny::reactive("5")),
    {
      session$setInputs(mode = "count", index = "NDVI")
      session$elapse(500); session$flushReact()
      expect_match(.s022_html(output$banner), "alert-success", fixed = TRUE)
    }
  )
  .s022_fast_harness(c(0, 0, 0, 0))
  shiny::testServer(
    nemetonshiny:::mod_monitoring_fast_alerts_server,
    args = .s022_fast_args(shiny::reactive("6"), NULL),   # pas de _tot
    {
      session$setInputs(mode = "count", index = "NDVI")
      session$elapse(500); session$flushReact()
      expect_match(.s022_html(output$banner), "alert-success", fixed = TRUE)
    }
  )
})

# --- FORDEAD --------------------------------------------------------------

# Masque 4x4 de 10 m : alerte (classe 3) dans le coin haut-gauche seulement.
.s022_fd_raster <- function(vals = NULL) {
  r <- terra::rast(nrows = 4, ncols = 4, xmin = 0, xmax = 40, ymin = 0,
                   ymax = 40, crs = "EPSG:2154")
  v <- rep(0, 16)
  if (!is.null(vals)) v[vals] <- 3
  terra::values(r) <- v
  r
}

# Strate (id 11) : moitie basse du raster (aucune alerte si l'alerte est en haut).
.s022_fd_aoi <- function(con, zone_id) {
  sf::st_sf(zone_id = 11L, geometry = sf::st_sfc(
    sf::st_polygon(list(rbind(c(0, 0), c(40, 0), c(40, 20), c(0, 20),
                              c(0, 0)))), crs = 2154))
}

.s022_fd_run <- function(zone, alertes, code) {
  withr::with_tempdir({
    proj_path <- getwd()
    dir.create(file.path(proj_path, "cache", "layers", "fordead"),
               recursive = TRUE)
    testthat::with_mocked_bindings(
      get_monitoring_db_connection   = function(...) "fake-con",
      close_monitoring_db_connection = function(con) invisible(TRUE),
      get_monitoring_zone_aoi        = .s022_fd_aoi,
      {
        testthat::local_mocked_bindings(
          find_zones_by_project = function(con, project_uuid)
            data.frame(id = c(10L, 11L), name = c("p_tot", "p_res")),
          read_fordead_dieback_mask = function(...) .s022_fd_raster(alertes),
          .package = "nemeton"
        )
        shiny::testServer(
          nemetonshiny:::mod_monitoring_fordead_map_server,
          args = list(
            app_state = shiny::reactiveValues(
              language = "fr", current_project = list(id = "x", path = proj_path)),
            zone_id_r = shiny::reactive(zone)),
          {
            session$flushReact()
            code(.s022_html(output$overlay), mask_tot_r())
          }
        )
      }
    )
  })
}

test_that("FORDEAD: strate sans alerte, _tot en alerte -> carte info + rappel", {
  skip_if_not_installed("shiny"); skip_if_not_installed("terra")
  .s022_fd_run("11", alertes = 1, function(html, tot) {
    expect_match(html, "border-info", fixed = TRUE)
    expect_match(html, "text-info", fixed = TRUE)
    expect_false(grepl("border-success|check-circle-fill", html))
    expect_match(html, get_i18n("fr")$t("sante_strate_sans_alerte_titre"),
                 fixed = TRUE)
    # 1 pixel de 10 m x 10 m = 0,01 ha.
    expect_match(html, "1 pixels", fixed = TRUE)
    expect_s4_class(tot, "SpatRaster")      # masque _tot complet conserve
  })
})

test_that("FORDEAD: strate = _tot sans alerte -> « zone saine » inchangee", {
  skip_if_not_installed("shiny"); skip_if_not_installed("terra")
  .s022_fd_run("10", alertes = NULL, function(html, tot) {
    expect_match(html, "check-circle-fill", fixed = TRUE)
    expect_match(html, "border-success", fixed = TRUE)
    expect_false(grepl("border-info", html))
    expect_null(tot)                        # strate = _tot : pas de rappel
  })
})

test_that("FORDEAD: strate vide ET _tot sain -> « zone saine » inchangee", {
  skip_if_not_installed("shiny"); skip_if_not_installed("terra")
  .s022_fd_run("11", alertes = NULL, function(html, tot) {
    expect_match(html, "border-success", fixed = TRUE)
    expect_false(grepl("border-info", html))
  })
})

# --- RECONFORT ------------------------------------------------------------

# Classification 4x4 de 10 m : classe 3 en haut-gauche (ou rien), reste 1 (sain).
# Strate (id 7) = moitie basse ; `_tot` = id 5 (emprise complete).
.s022_rc_run <- function(zone, run_zone, tot_alert, layer, code,
                         env = parent.frame()) {
  pp <- withr::local_tempdir(.local_envir = env)
  cd <- file.path(pp, "cache", "layers", "reconfort", "zone_5")
  dir.create(cd, recursive = TRUE)
  tif <- file.path(cd, "reconfort_mask_x.tif")
  base <- terra::rast(nrows = 4, ncols = 4, xmin = 0, xmax = 40, ymin = 0,
                      ymax = 40, crs = "EPSG:2154")
  v <- rep(1, 16); if (tot_alert) v[1] <- 3
  terra::values(base) <- v
  terra::writeRaster(base, tif, overwrite = TRUE)
  man <- data.frame(
    id = c("classification", "score"),
    label_key = c("reconfort_couche_classes", "reconfort_couche_classes"),
    type = "raster", role = c("classification", "score"),
    path = tif, categorical = c(TRUE, FALSE),
    palette = NA_character_, reverse = FALSE, vmin = NA_real_, vmax = NA_real_,
    default_visible = c(TRUE, FALSE), default_opacity = 0.8,
    n_features = NA_integer_, stringsAsFactors = FALSE)
  log <- new.env(); log$reads <- list()
  aoi_for <- function(con, zone_id) {
    box <- if (identical(as.integer(zone_id), 5L))
      rbind(c(0, 0), c(40, 0), c(40, 40), c(0, 40), c(0, 0))
    else rbind(c(0, 0), c(40, 0), c(40, 20), c(0, 20), c(0, 0))
    sf::st_sf(zone_id = zone_id, geometry = sf::st_sfc(
      sf::st_polygon(list(box)), crs = 2154))
  }
  testthat::local_mocked_bindings(
    get_monitoring_db_connection   = function(...) "CON",
    close_monitoring_db_connection = function(...) invisible(NULL),
    get_monitoring_zone_aoi        = aoi_for,
    .env = env
  )
  testthat::local_mocked_bindings(
    reconfort_cache_manifest = function(...) man,
    read_reconfort_layer = function(layer, con = NULL, zone_id = NULL,
                                    apply_zone_mask = TRUE, mask_polygon = NULL) {
      log$reads[[length(log$reads) + 1L]] <- list(
        id = layer$id, apply = apply_zone_mask,
        box = if (is.null(mask_polygon)) NULL else
          unname(sf::st_bbox(mask_polygon)[["ymax"]]))
      r <- terra::rast(layer$path)
      if (!is.null(mask_polygon))
        r <- terra::mask(r, terra::vect(mask_polygon))
      r
    },
    .package = "nemeton", .env = env
  )
  shiny::testServer(
    nemetonshiny:::mod_monitoring_reconfort_map_server,
    args = list(id = "rc",
                app_state = shiny::reactiveValues(
                  language = "fr", current_project = list(id = "p", path = pp)),
                zone_id_r = shiny::reactive(zone),
                result_r = shiny::reactive(NULL),
                run_zone_id_r = shiny::reactive(run_zone)),
    {
      session$setInputs(layers = layer, opacity = 0.8)
      session$flushReact()
      code(.s022_html(output$overlay), log)
    }
  )
}

test_that("RECONFORT: strate sans pixel 2/3, _tot en alerte -> carte info + rappel", {
  skip_if_not_installed("shiny"); skip_if_not_installed("terra")
  skip_if_not(exists("reconfort_cache_manifest",
                     where = asNamespace("nemeton"), inherits = FALSE))
  .s022_rc_run("7", "5", tot_alert = TRUE, layer = "classification",
               function(html, log) {
    expect_match(html, "border-info", fixed = TRUE)
    expect_match(html, "text-info", fixed = TRUE)
    expect_match(html, get_i18n("fr")$t("sante_strate_sans_alerte_titre"),
                 fixed = TRUE)
    expect_match(html, "1 pixels", fixed = TRUE)
    # Relecture _tot : mask_polygon = AOI de _tot (ymax 40), jamais
    # apply_zone_mask = FALSE.
    tot_reads <- Filter(function(x) identical(x$box, 40), log$reads)
    expect_length(tot_reads, 1L)
    expect_true(all(vapply(log$reads, function(x) isTRUE(x$apply), logical(1))))
  })
})

test_that("RECONFORT: _tot relu une seule fois, pas a chaque rendu", {
  skip_if_not_installed("shiny"); skip_if_not_installed("terra")
  skip_if_not(exists("reconfort_cache_manifest",
                     where = asNamespace("nemeton"), inherits = FALSE))
  .s022_rc_run("7", "5", tot_alert = TRUE, layer = "classification",
               function(html, log) {
    n0 <- length(Filter(function(x) identical(x$box, 40), log$reads))
    expect_identical(n0, 1L)
  })
})

test_that("RECONFORT: strate = _tot, ou sans _tot (\"NA\") -> aucun message nouveau", {
  skip_if_not_installed("shiny"); skip_if_not_installed("terra")
  skip_if_not(exists("reconfort_cache_manifest",
                     where = asNamespace("nemeton"), inherits = FALSE))
  .s022_rc_run("5", "5", tot_alert = FALSE, layer = "classification",
               function(html, log) {
    expect_false(grepl("border-info", html, fixed = TRUE))
    expect_length(Filter(function(x) identical(x$box, 40), log$reads), 1L)
  })
  # Sans _tot : run_zone_id_r() vaut la chaine "NA".
  .s022_rc_run("7", "NA", tot_alert = TRUE, layer = "classification",
               function(html, log) {
    expect_false(grepl("border-info", html, fixed = TRUE))
  })
})

test_that("RECONFORT: couche score -> rien ne change", {
  skip_if_not_installed("shiny"); skip_if_not_installed("terra")
  skip_if_not(exists("reconfort_cache_manifest",
                     where = asNamespace("nemeton"), inherits = FALSE))
  .s022_rc_run("7", "5", tot_alert = TRUE, layer = "score",
               function(html, log) {
    expect_false(grepl("border-info", html, fixed = TRUE))
  })
})

test_that("RECONFORT: strate ET _tot sans alerte -> aucun message (pas de vert)", {
  skip_if_not_installed("shiny"); skip_if_not_installed("terra")
  skip_if_not(exists("reconfort_cache_manifest",
                     where = asNamespace("nemeton"), inherits = FALSE))
  .s022_rc_run("7", "5", tot_alert = FALSE, layer = "classification",
               function(html, log) {
    # _tot sans alerte non plus : aucun message (pas d'etat vert RECONFORT).
    expect_false(grepl("border-info", html, fixed = TRUE))
  })
})

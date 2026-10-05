# Audit 1.0, lot 6 : bugs divers

# ---- #53 langue par session -------------------------------------------------

test_that(".langue_requete reads ?lang= from a query string or a request", {
  expect_identical(.langue_requete("?lang=en&project=x"), "en")
  expect_identical(.langue_requete(list(QUERY_STRING = "?lang=fr")), "fr")
  expect_null(.langue_requete("?lang=de"))
  expect_null(.langue_requete(""))
  expect_null(.langue_requete(NULL))
})

test_that("get_app_options honours the session language, not another session's", {
  withr::local_options(nemeton.app_options = list(language = "fr"))
  s_en <- shiny::MockShinySession$new()
  s_fr <- shiny::MockShinySession$new()
  s_en$userData$langue <- "en"
  expect_identical(shiny::withReactiveDomain(s_en, get_app_options()$language), "en")
  expect_identical(shiny::withReactiveDomain(s_fr, get_app_options()$language), "fr")
  expect_identical(get_app_options()$language, "fr")
  # L'option globale n'a pas bouge
  expect_identical(getOption("nemeton.app_options")$language, "fr")
})

test_that("app_ui builds in the requested language without changing the global option", {
  withr::local_options(nemeton.app_options = list(language = "fr"))
  ui <- as.character(app_ui(list(QUERY_STRING = "?lang=en")))
  i18n_en <- get_i18n("en")
  expect_true(grepl(i18n_en$t("tab_synthesis"), ui, fixed = TRUE))
  expect_identical(getOption("nemeton.app_options")$language, "fr")
})

test_that("switching language reloads with ?lang= and leaves the process option alone", {
  withr::local_options(nemeton.app_options = list(language = "fr"))
  messages <- list()
  serveur <- function(input, output, session) {
    app_state <- shiny::reactiveValues(language = .langue_session_init(session))
    shiny::observeEvent(input$app_language, {
      new_lang <- input$app_language
      if (!.langue_valide(new_lang)) return()
      if (identical(new_lang, shiny::isolate(app_state$language))) return()
      app_state$language <- new_lang
      session$userData$langue <- new_lang
      session$sendCustomMessage("nemetonSetLang", list(lang = new_lang))
    })
  }
  shiny::testServer(serveur, {
    session$sendCustomMessage <- function(type, message) messages[[type]] <<- message
    session$setInputs(app_language = "en")
    expect_identical(session$userData$langue, "en")
  })
  expect_identical(getOption("nemeton.app_options")$language, "fr")
})

# ---- #55 cartes de fin de calcul ---------------------------------------------

test_that("reset_tracking clears the previous completion card by default", {
  st <- shiny::reactiveValues(language = "fr")
  shiny::testServer(mod_progress_server,
    args = list(compute_state = shiny::reactive(NULL), app_state = st), {
      rv$show_complete <- TRUE
      rv$show_error <- TRUE
      reset_tracking(cartes = FALSE)
      expect_true(rv$show_complete)
      reset_tracking()
      expect_false(rv$show_complete)
      expect_false(rv$show_error)
    })
})

# ---- #37 calcul sans aucun indicateur ---------------------------------------

test_that(".au_moins_un_indicateur needs at least one finite indicator value", {
  expect_false(.au_moins_un_indicateur(NULL))
  expect_false(.au_moins_un_indicateur(data.frame(ug_id = 1:2,
    indicateur_c1_biomasse = NA_real_, indicateur_b1_protection = NA_real_)))
  expect_false(.au_moins_un_indicateur(data.frame(ug_id = 1, indicateur_c1_biomasse_norm = 50)))
  expect_true(.au_moins_un_indicateur(data.frame(ug_id = 1:2,
    indicateur_c1_biomasse = c(NA, 12))))
})

# ---- #41 contenance NA -------------------------------------------------------

test_that("UGF migration falls back to the geometric area when contenance is missing", {
  sq <- function(x) sf::st_polygon(list(rbind(c(x, 0), c(x + 100, 0), c(x + 100, 100), c(x, 100), c(x, 0))))
  parcels <- sf::st_sf(id = c("a", "b"), contenance = c(NA, 0),
                       geometry = sf::st_sfc(sq(800000), sq(800200), crs = 2154))
  p <- ug_init_default(list(parcels = parcels, metadata = list(id = "p")))
  expect_equal(p$tenements$surface_m2, c(10000, 10000))
})

# ---- #48 import de tenements --------------------------------------------------

test_that(".tenement_guess_crs tells degrees from Lambert-93 metres", {
  deg <- sf::st_sf(geometry = sf::st_sfc(sf::st_point(c(5.4, 47.1))))
  l93 <- sf::st_sf(geometry = sf::st_sfc(sf::st_point(c(850000, 6650000))))
  expect_identical(.tenement_guess_crs(deg), 4326L)
  expect_identical(.tenement_guess_crs(l93), 2154L)
})

# ---- #38 ecritures atomiques --------------------------------------------------

test_that(".write_raster_atomic leaves no temporary file and replaces the target", {
  d <- withr::local_tempdir()
  f <- file.path(d, "r.tif")
  .write_raster_atomic(terra::rast(nrows = 2, ncols = 2, vals = 1), f)
  .write_raster_atomic(terra::rast(nrows = 2, ncols = 2, vals = 2), f)
  expect_equal(unique(as.numeric(terra::values(terra::rast(f)))), 2)
  expect_identical(list.files(d), "r.tif")
})

test_that("a failed LiDAR mosaic returns NULL, not the first tile alone", {
  d <- withr::local_tempdir()
  t1 <- file.path(d, "t1.tif")
  terra::writeRaster(terra::rast(nrows = 2, ncols = 2, vals = 1, crs = "EPSG:2154"), t1)
  local_mocked_bindings(.write_raster_atomic = function(...) stop("disque plein"))
  expect_warning(r <- mosaic_lidar_tiles(c(t1, t1), file.path(d, "m.tif")), "mosaic")
  expect_null(r)
})

test_that("a LiDAR tile download is renamed only when complete", {
  d <- withr::local_tempdir()
  dest <- file.path(d, "tuile.laz")
  local_mocked_bindings(
    request = function(url) list(url = url),
    req_timeout = function(req, ...) req,
    req_error = function(req, ...) req,
    req_perform = function(req, path) { writeLines(strrep("x", 200), path); structure(list(), class = "httr2_response") },
    resp_status = function(resp) 200L,
    .package = "httr2")
  expect_identical(download_lidar_tile("http://x", dest), dest)
  expect_true(file.exists(dest))
  expect_false(file.exists(paste0(dest, ".part")))
})

# ---- #43 score global NA ------------------------------------------------------

test_that("an unavailable global score renders 'no data' instead of crashing", {
  local_mocked_bindings(
    project_family_scores = function(project) data.frame(ug_id = "u1", famille_carbone = NA_real_),
    project_global_index = function(...) list(score = NA_real_),
    project_ndp_level = function(...) 0L)
  st <- shiny::reactiveValues(language = "fr", current_project = list(id = "p"))
  shiny::testServer(mod_synthesis_server, args = list(app_state = st), {
    session$setInputs(x = 1)
    html <- paste(as.character(output$global_score), collapse = " ")
    expect_true(grepl(get_i18n("fr")$t("no_data"), html, fixed = TRUE))
  })
})

# ---- #42 echec d'enregistrement des UG ---------------------------------------

test_that("a failed UG save neither crashes the session nor shows the rename as done", {
  sq <- sf::st_polygon(list(rbind(c(0, 0), c(100, 0), c(100, 100), c(0, 100), c(0, 0))))
  parcels <- sf::st_sf(id = "a", geometry = sf::st_sfc(sq, crs = 2154))
  projet <- ug_init_default(list(parcels = parcels))
  projet$metadata <- list(id = "p1")
  local_mocked_bindings(
    get_app_options = function() list(language = "fr"),
    save_ug_data = function(...) stop("disque plein"),
    deny_if_readonly = function(...) FALSE)
  st <- shiny::reactiveValues(language = "fr", current_project = NULL)
  shiny::testServer(mod_ug_server, args = list(app_state = st), {
    session$setInputs(x = 1)
    rv$projet_ug <- projet
    session$flushReact()
    ancien <- rv$projet_ug$ugs$label
    session$setInputs(ug_table_rows_selected = 1L, rename_label = "Nouveau nom")
    session$setInputs(confirm_rename = 1L)
    expect_identical(rv$projet_ug$ugs$label, ancien)
  })
})

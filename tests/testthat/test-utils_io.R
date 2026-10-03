# Ecritures atomiques des fichiers de projet (R/utils_io.R).

test_that("un echec d'ecriture JSON laisse le fichier existant intact", {
  withr::with_tempdir({
    jsonlite::write_json(list(name = "ancien"), "metadata.json", auto_unbox = TRUE)
    # Ecriture qui echoue en cours de route (disque plein, processus tue).
    local_mocked_bindings(write_json = function(x, path, ...) {
      writeLines("{ tronq", path); stop("disque plein")
    }, .package = "jsonlite")
    expect_error(nemetonshiny:::.write_json_atomic(list(name = "x"), "metadata.json"),
                 "disque plein")
    expect_equal(jsonlite::read_json("metadata.json")$name, "ancien")
    expect_length(list.files(all.files = TRUE, no.. = TRUE), 1L)  # pas de fichier temporaire

  })
})

test_that("une ecriture JSON reussie remplace le fichier", {
  withr::with_tempdir({
    jsonlite::write_json(list(name = "ancien"), "metadata.json", auto_unbox = TRUE)
    nemetonshiny:::.write_json_atomic(list(name = "nouveau"), "metadata.json",
                                      auto_unbox = TRUE)
    expect_equal(jsonlite::read_json("metadata.json")$name, "nouveau")
  })
})

test_that("un echec d'ecriture GeoPackage ne supprime plus l'ancien fichier", {
  withr::with_tempdir({
    pts <- sf::st_sf(id = 1:2, geometry = sf::st_sfc(
      sf::st_point(c(0, 0)), sf::st_point(c(1, 1)), crs = 2154))
    nemetonshiny:::.st_write_atomic(pts, "parcels.gpkg")
    expect_equal(nrow(sf::st_read("parcels.gpkg", quiet = TRUE)), 2L)

    # L'ancien schema `unlink(); st_write()` laissait le projet sans
    # parcelles quand l'ecriture echouait.
    expect_error(nemetonshiny:::.st_write_atomic("pas un sf", "parcels.gpkg"))
    expect_equal(nrow(sf::st_read("parcels.gpkg", quiet = TRUE)), 2L)
  })
})

test_that("des fichiers UGF illisibles sont mis de cote avant la migration", {
  withr::with_tempdir({
    racine <- file.path(getwd(), "projects")
    data_dir <- file.path(racine, "p1", "data")
    dir.create(data_dir, recursive = TRUE)
    jsonlite::write_json(list(id = "p1"), file.path(racine, "p1", "metadata.json"),
                         auto_unbox = TRUE)
    # Decoupage present mais `ugs.json` tronque (ecriture interrompue).
    writeLines("{ tronque", file.path(data_dir, "ugs.json"))
    writeLines("gpkg", file.path(data_dir, "tenements.gpkg"))
    local_mocked_bindings(get_projects_root = function() racine)

    dest <- suppressWarnings(nemetonshiny:::.mettre_de_cote_ug("p1"))
    expect_true(dir.exists(dest))
    expect_setequal(list.files(dest), c("ugs.json", "tenements.gpkg"))
    expect_false(file.exists(file.path(data_dir, "ugs.json")))
    meta <- jsonlite::read_json(file.path(racine, "p1", "metadata.json"))
    expect_equal(meta$ug_sauvegarde, basename(dest))

    # Rien a mettre de cote : rien ne se passe.
    expect_null(nemetonshiny:::.mettre_de_cote_ug("p1"))
  })
})

test_that("un plan d'actions illisible est copie avant d'etre remplace", {
  withr::with_tempdir({
    racine <- file.path(getwd(), "projects")
    dir.create(file.path(racine, "p1", "data"), recursive = TRUE)
    local_mocked_bindings(get_projects_root = function() racine)
    chemin <- nemetonshiny:::get_action_plan_path("p1")
    writeLines("{ abime", chemin)

    plan <- suppressWarnings(nemetonshiny:::load_action_plan("p1"))
    expect_length(plan$actions, 0L)
    copies <- list.files(dirname(chemin), pattern = "^action_plan\\.illisible-")
    expect_length(copies, 1L)
    expect_equal(readLines(file.path(dirname(chemin), copies)), "{ abime")
  })
})


test_that("modifier le decoupage UGF invalide les indicateurs, renommer non", {
  withr::with_tempdir({
    racine <- file.path(getwd(), "projects")
    dir.create(file.path(racine, "p1", "data"), recursive = TRUE)
    jsonlite::write_json(list(id = "p1", indicators_computed = TRUE, status = "completed"),
                         file.path(racine, "p1", "metadata.json"), auto_unbox = TRUE)
    local_mocked_bindings(get_projects_root = function() racine)
    carre <- function(x) sf::st_polygon(list(rbind(c(x, 0), c(x + 1, 0), c(x + 1, 1),
                                                   c(x, 1), c(x, 0))))
    projet <- list(
      tenements = sf::st_sf(tenement_id = c("t1", "t2"), parent_parcelle_id = c("a", "b"),
                            ug_id = c("u1", "u2"), surface_m2 = c(1, 1),
                            geometry = sf::st_sfc(carre(0), carre(2), crs = 2154)),
      ugs = data.frame(ug_id = c("u1", "u2"), label = c("A", "B"), groupe = NA_character_))
    indic <- file.path(racine, "p1", "data", "indicators.parquet")

    # Premiere ecriture : rien a invalider.
    expect_false(isTRUE(attr(nemetonshiny:::save_ug_data("p1", projet), "indicateurs_invalides")))

    # Renommer une UGF : memes ug_id, les indicateurs restent valables.
    writeLines("x", indic)
    projet$ugs$label[1] <- "A bis"
    expect_false(isTRUE(attr(nemetonshiny:::save_ug_data("p1", projet), "indicateurs_invalides")))
    expect_true(file.exists(indic))

    # Fusionner : t2 rejoint u1, l'ancienne UGF u2 disparait.
    projet$tenements$ug_id <- c("u1", "u1")
    projet$ugs <- projet$ugs[1, ]
    res <- suppressMessages(nemetonshiny:::save_ug_data("p1", projet))
    expect_true(isTRUE(attr(res, "indicateurs_invalides")))
    expect_false(file.exists(indic))
    meta <- jsonlite::read_json(file.path(racine, "p1", "metadata.json"))
    expect_false(isTRUE(meta$indicators_computed))
  })
})


test_that("changer les parcelles d'un projet met le decoupage UGF de cote", {
  withr::with_tempdir({
    racine <- file.path(getwd(), "projects")
    local_mocked_bindings(get_projects_root = function() racine)
    carre <- function(x) sf::st_polygon(list(rbind(c(x, 0), c(x + 1, 0), c(x + 1, 1),
                                                   c(x, 1), c(x, 0))))
    parcelles <- sf::st_sf(id = c("A", "B"),
                           geometry = sf::st_sfc(carre(0), carre(2), crs = 4326))
    projet <- suppressMessages(nemetonshiny:::create_project("P", parcels = parcelles))
    data_dir <- file.path(racine, projet$id, "data")
    writeLines("{}", file.path(data_dir, "ugs.json"))
    writeLines("x", file.path(data_dir, "indicators.parquet"))

    # Memes parcelles : rien ne bouge.
    p <- suppressMessages(nemetonshiny:::update_project(projet$id, "P", parcels = parcelles))
    expect_false(isTRUE(p$ugf_reinitialisees))
    expect_true(file.exists(file.path(data_dir, "ugs.json")))

    # Une parcelle de plus : decoupage mis de cote, indicateurs invalides.
    plus <- rbind(parcelles, sf::st_sf(id = "C", geometry = sf::st_sfc(carre(4), crs = 4326)))
    p <- suppressWarnings(suppressMessages(
      nemetonshiny:::update_project(projet$id, "P", parcels = plus)))
    expect_true(isTRUE(p$ugf_reinitialisees))
    expect_false(file.exists(file.path(data_dir, "ugs.json")))
    expect_false(file.exists(file.path(data_dir, "indicators.parquet")))
    expect_length(list.files(data_dir, pattern = "^ug_sauvegarde_"), 1L)
    expect_false(isTRUE(p$metadata$indicators_computed))
  })
})

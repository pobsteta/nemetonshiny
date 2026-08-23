# test-service_marculus.R - export vers Marculus (martelage sur telephone)
#
# Ce qui se teste ici est un CONTRAT AVEC UN AUTRE LOGICIEL, dont la
# specification vit dans `marculus/docs/specs/couches-gpkg.md` et dont
# l'implementation vit en Kotlin. Les valeurs figees ci-dessous ne sont donc
# pas des choix de l'app : ce sont les seules que la partie d'en face lit.

test_that("les separateurs d'essences sont ceux de MartelageRepository.kt", {
  e <- nemetonshiny:::.marculus_encode_essences(c("Hetre", "Chene"))
  octets <- utf8ToInt(e)
  # US (0x1F) entre nom / fond / texte, RS (0x1E) entre enregistrements.
  expect_true(0x1FL %in% octets)
  expect_true(0x1EL %in% octets)
  expect_equal(sum(octets == 0x1EL), 1L)   # deux essences -> un seul RS
  expect_equal(sum(octets == 0x1FL), 4L)   # deux champs de couleur par essence

  # Vide est LEGITIME : la feuille arrive sans colonne, l'operateur les ajoute.
  # Le contraire - inventer des essences - remplirait la matrice de colonnes
  # que personne n'a demandees.
  expect_identical(nemetonshiny:::.marculus_encode_essences(character(0)), "")
  expect_identical(nemetonshiny:::.marculus_encode_essences(c(NA, "")), "")
})

test_that("le contexte porte TOUS les champs non optionnels de versContexte()", {
  # Cote Kotlin ces champs se lisent avec `getString`/`getInt`/`getBoolean`/
  # `getLong` : un seul manquant fait echouer l'import de tout le fichier.
  a <- list(id = "a1", ug_id = "ug_2", type = "eclaircie",
            annee_cible = 2028, statut = "validee")
  p <- list(id = "p1", metadata = list(name = "Dabo", owner = "Etat"))
  ctx <- nemetonshiny:::marculus_context_from_action(a, p)

  for (champ in c("id", "nom", "mode", "classeMin", "classeMax", "classePas",
                  "increment", "exporte", "dateCreation")) {
    expect_false(is.null(ctx[[champ]]), info = champ)
  }
  expect_true(is.logical(ctx$exporte))
  expect_true(ctx$mode %in% c("DIAMETRE", "CIRCONFERENCE"))

  # `cheminGpkg` est un chemin DANS le stockage prive du telephone : cette
  # machine ne peut pas le connaitre, et en inventer un ferait pointer le
  # contexte sur un fichier absent.
  expect_null(ctx$cheminGpkg)
})

test_that("les cinq statuts Kanban se transposent tels quels", {
  p <- list(id = "p", metadata = list(name = "F"))
  for (s in names(nemetonshiny:::MARCULUS_STATUTS)) {
    ctx <- nemetonshiny:::marculus_context_from_action(
      list(id = "x", ug_id = "u", type = "eclaircie", statut = s), p)
    expect_identical(ctx$statut, toupper(s), info = s)
  }
  # Statut inconnu : on ne devine pas, on retombe sur l'entree du Kanban.
  ctx <- nemetonshiny:::marculus_context_from_action(
    list(id = "x", ug_id = "u", type = "eclaircie", statut = "farfelu"), p)
  expect_identical(ctx$statut, "PROPOSEE")
})

test_that("le JSON est celui de la FUSION, pas celui de la restauration", {
  p <- list(id = "p", metadata = list(name = "F"))
  ctx <- nemetonshiny:::marculus_context_from_action(
    list(id = "x", ug_id = "u", type = "eclaircie"), p)
  j <- nemetonshiny:::marculus_sync_json(list(ctx))
  parsed <- jsonlite::fromJSON(j, simplifyVector = FALSE)

  expect_identical(parsed$version, 1L)
  expect_length(parsed$contextes, 1L)
  # `tiges` et `configs` VIDES et presents : la fusion est une union par UUID,
  # donc une liste vide n'ajoute ni ne retire rien.
  expect_length(parsed$tiges, 0L)
  expect_length(parsed$configs, 0L)
  # `referentiels` ABSENT : seul `importerJson()` le lirait, et celui-la EFFACE
  # contextes, tiges et configs avant d'inserer. Ce fichier ne doit jamais
  # ressembler a une sauvegarde complete.
  expect_null(parsed$referentiels)
})

test_that("seules les actions qui designent des tiges deviennent des contextes", {
  plan <- list(actions = list(
    list(id = "1", type = "eclaircie"), list(id = "2", type = "coupe_rase"),
    list(id = "3", type = "depressage"), list(id = "4", type = "observation"),
    list(id = "5", type = "plantation"), list(id = "6", type = "desserte"),
    list(id = "7", type = "protection"), list(id = "8", type = "entretien")))
  el <- nemetonshiny:::marculus_eligible_actions(plan)
  expect_identical(vapply(el, function(a) a$id, ""), c("1", "2", "3", "4"))
})

test_that("la couche de parcelles porte les noms de colonnes que le telephone cherche", {
  skip_if_not_installed("sf")
  poly <- function(dx) sf::st_polygon(list(rbind(
    c(dx, 0), c(dx + 1, 0), c(dx + 1, 1), c(dx, 1), c(dx, 0))))

  parcels <- sf::st_sf(
    id = c("p1", "p2"), section = c("AY", "AY"), numero = c("12", "13"),
    commune = c("48042", "48042"),
    geometry = sf::st_sfc(poly(0), poly(2), crs = 4326))
  tenements <- sf::st_sf(
    tenement_id = c("t1", "t2"), parent_parcelle_id = c("p1", "p2"),
    ug_id = c("ug_1", "ug_2"),
    geom = sf::st_sfc(poly(0), poly(2), crs = 4326))
  commune_geometry <- sf::st_sf(
    code = "48042", nom = "Chastel-Nouvel",
    geom = sf::st_sfc(poly(0), crs = 4326))

  projet <- list(id = "p", metadata = list(name = "ForetAccess", owner = "Privé"),
                 parcels = parcels, tenements = tenements,
                 commune_geometry = commune_geometry)

  out <- nemetonshiny:::.marculus_parcelles(projet, "ug_1")
  expect_s3_class(out, "sf")
  # Le perimetre est celui de l'UGF de l'action, pas celui du projet : un
  # contexte est UN chantier.
  expect_equal(nrow(out), 1L)
  for (col in c("proprietaire", "foret", "commune", "section", "numero")) {
    expect_true(col %in% names(out), info = col)
  }
  # Le code INSEE ne dit rien a un marteleur : c'est le nom qui s'affiche.
  expect_identical(out$commune, "Chastel-Nouvel")
  expect_identical(out$section, "AY")
  # Aucune surface : Marculus la calcule depuis la geometrie et n'en lit aucune.
  expect_false(any(grepl("surface|contenance", names(out))))

  # Sans commune_geometry (projet ancien), le code vaut mieux que rien.
  projet$commune_geometry <- NULL
  expect_identical(nemetonshiny:::.marculus_parcelles(projet, "ug_1")$commune,
                   "48042")

  # UGF inconnue : NULL, et surtout pas le projet entier.
  expect_null(nemetonshiny:::.marculus_parcelles(projet, "ug_inexistante"))
})

test_that("le GeoPackage ne porte QUE les noms de couches reconnus", {
  skip_if_not_installed("sf")
  poly <- sf::st_polygon(list(rbind(c(0, 0), c(1, 0), c(1, 1), c(0, 1), c(0, 0))))
  projet <- list(
    id = "p", metadata = list(name = "F", owner = "O"),
    parcels = sf::st_sf(id = "p1", section = "A", numero = "1", commune = "1",
                        geometry = sf::st_sfc(poly, crs = 4326)),
    tenements = sf::st_sf(tenement_id = "t1", parent_parcelle_id = "p1",
                          ug_id = "ug_1", geom = sf::st_sfc(poly, crs = 4326)))
  desserte <- sf::st_sf(
    nom = "RF du Haut", type = "existante",
    geometry = sf::st_sfc(sf::st_linestring(rbind(c(0, 0), c(1, 1))), crs = 4326))

  withr::with_tempdir({
    f <- "ctx.gpkg"
    ok <- nemetonshiny:::marculus_write_action_gpkg(
      projet, list(ug_id = "ug_1"), f, desserte = desserte)
    expect_true(ok)

    couches <- sf::st_layers(f)$name
    # `desserte` est un nom RESERVE ; tout autre nom aurait fait de ces lignes
    # une couche de parcelles, et chaque troncon serait devenu candidat au
    # rattachement spatial des tiges.
    expect_setequal(couches, c("parcelle", "desserte"))
    # Pas de table de tuiles : l'ortho du projet pese des gigaoctets, et un
    # fond de carte qui ne tient pas sur le telephone n'est pas un fond.
    expect_false(any(grepl("^gpkg_|_wm$", couches)))
  })
})

test_that("sans parcelle a envoyer, rien n'est ecrit", {
  skip_if_not_installed("sf")
  projet <- list(id = "p", metadata = list(name = "F"), tenements = NULL)
  withr::with_tempdir({
    expect_false(nemetonshiny:::marculus_write_action_gpkg(
      projet, list(ug_id = "ug_1"), "vide.gpkg"))
    expect_false(file.exists("vide.gpkg"))
  })
})


# ---------------------------------------------------------------------------
# Pre-remplissage de la feuille de martelage
# ---------------------------------------------------------------------------

test_that("les essences viennent du profil de groupe du projet", {
  # ONF, CRPF et OFB ne martelent pas les memes essences : le profil de groupe
  # est ce que le projet porte de plus proche d'un contexte sylvicole.
  onf  <- nemetonshiny:::get_groupes_essences("onf")
  crpf <- nemetonshiny:::get_groupes_essences("crpf")
  ofb  <- nemetonshiny:::get_groupes_essences("ofb")

  expect_gt(length(onf), 0L)
  expect_false(identical(onf, crpf))
  # Le peuplier est une essence de plantation privee ; il n'a rien a faire en
  # tete d'une feuille ONF, ni l'aulne glutineux hors zone humide.
  expect_true("Peuplier" %in% crpf)
  expect_false("Peuplier" %in% onf)
  expect_true("Aulne glutineux" %in% ofb)

  # Profil inconnu : on retombe sur le defaut, pas sur une feuille vide.
  expect_gt(length(nemetonshiny:::get_groupes_essences("profil-inexistant")), 0L)
})

test_that("les libelles d'essence sont ceux que Marculus sait apparier", {
  # Le telephone normalise (minuscules, accents otes) avant de chercher son
  # coefficient de cubage : « Hetre » et « hetre » sont la meme cle. On ecrit
  # donc la forme LISIBLE, sans sacrifier l'appariement.
  toutes <- unique(unlist(lapply(c("onf", "crpf", "ofb", "generic"),
                                 nemetonshiny:::get_groupes_essences)))
  expect_true(all(nzchar(toutes)))
  # Aucun code a trois lettres : c'est le telephone qui derive « HET » de
  # « Hetre », pas l'inverse.
  expect_false(any(grepl("^[A-Z]{3}$", toutes)))
  # Les separateurs du format ne doivent JAMAIS apparaitre dans un libelle :
  # ils casseraient l'encodage de la matrice entiere.
  expect_false(any(grepl("[\u001E\u001F]", toutes)))
})

test_that("une liste explicite prime sur le profil, et le vide reste possible", {
  projet <- list(metadata = list(groupes_profile = "onf"))
  ctx <- nemetonshiny:::marculus_context_from_action(
    list(id = "x", ug_id = "u", type = "eclaircie"), projet,
    essences = c("M\u00e9l\u00e8ze"))
  expect_true(grepl("M\u00e9l\u00e8ze", ctx$essences))

  vide <- nemetonshiny:::marculus_context_from_action(
    list(id = "x", ug_id = "u", type = "eclaircie"), projet,
    essences = character(0))
  expect_identical(vide$essences, "")
})


# ---------------------------------------------------------------------------
# `gpkgNom` : l'appariement contexte <-> fichier dans le lot
#
# Sans lui, receptionner treize chantiers demande treize rattachements manuels
# depuis un selecteur de fichiers, sur des noms qui se ressemblent. Une erreur
# d'appariement ne se voit pas : la carte affiche une parcelle - la mauvaise -
# et les tiges se rattachent a un perimetre qui n'est pas le leur.
# Cf. `specs/BRIEF-marculus-import-zip.md`.
# ---------------------------------------------------------------------------

test_that("le contexte nomme le fichier de son GeoPackage, sans chemin", {
  p <- list(id = "p", metadata = list(name = "F"))
  a <- list(id = "x", ug_id = "u", type = "eclaircie")

  ctx <- nemetonshiny:::marculus_context_from_action(a, p, gpkg_nom = "chantier.gpkg")
  expect_identical(ctx$gpkgNom, "chantier.gpkg")

  # Un nom NU, jamais un chemin : le lot est a plat, et un chemin relatif
  # ouvrirait la porte au zip-slip cote telephone.
  ctx <- nemetonshiny:::marculus_context_from_action(
    a, p, gpkg_nom = "../../evasion.gpkg")
  expect_identical(ctx$gpkgNom, "evasion.gpkg")

  # Absent par defaut : le champ ne doit pas apparaitre vide.
  expect_null(nemetonshiny:::marculus_context_from_action(a, p)$gpkgNom)
})

test_that("chaque contexte du lot designe un fichier qui existe", {
  skip_if_not_installed("sf")
  skip_if_not_installed("jsonlite")
  poly <- function(dx) sf::st_polygon(list(rbind(
    c(dx, 0), c(dx + 1, 0), c(dx + 1, 1), c(dx, 1), c(dx, 0))))

  withr::with_tempdir({
    racine <- getwd()
    with_mocked_bindings(
      get_app_options = function() list(project_dir = racine),
      {
        parcels <- sf::st_sf(
          id = "p1", section = "A", numero = "1", commune = "1",
          contenance = 1e4, geometry = sf::st_sfc(poly(0), crs = 4326))
        pid <- nemetonshiny:::create_project(name = "Lot", parcels = parcels)$id

        # Deux actions eligibles sur la meme UGF, pour que les noms different
        # bien par l'action et pas seulement par l'UGF.
        chemin <- nemetonshiny:::get_project_path(pid)
        jsonlite::write_json(list(
          version = 1L, project_id = pid, horizon_annees = 20L,
          actions = list(
            list(id = "a1", ug_id = "ug_1", type = "eclaircie",
                 annee_cible = 2028L, priorite = "haute", statut = "validee"),
            list(id = "a2", ug_id = "ug_1", type = "coupe_rase",
                 annee_cible = 2030L, priorite = "basse", statut = "proposee")),
          audit = list()),
          file.path(chemin, "data", "action_plan.json"), auto_unbox = TRUE)

        z <- file.path(racine, "lot.zip")
        res <- nemetonshiny:::marculus_export_bundle(pid, z)
        expect_equal(res$n_contexts, 2L)

        d <- file.path(racine, "ouvert")
        dir.create(d)
        utils::unzip(z, exdir = d)

        ms <- list.files(d, pattern = "[.]marsync$", full.names = TRUE)
        expect_length(ms, 1L)
        j <- jsonlite::fromJSON(ms, simplifyVector = FALSE)

        for (ctx in j$contextes) {
          expect_false(is.null(ctx$gpkgNom), info = ctx$id)
          # LE point du brief : le nom annonce designe un fichier PRESENT.
          expect_true(file.exists(file.path(d, ctx$gpkgNom)), info = ctx$gpkgNom)
          # ASCII seul : le nom doit traverser un ZIP et un systeme de fichiers
          # Android sans surprise.
          expect_false(grepl("[^A-Za-z0-9_.-]", ctx$gpkgNom), info = ctx$gpkgNom)
        }
        # Deux actions, deux fichiers distincts : sans cela le second ecraserait
        # le premier et un contexte pointerait sur le chantier de l'autre.
        noms <- vapply(j$contextes, function(c) c$gpkgNom, "")
        expect_length(unique(noms), 2L)
      }
    )
  })
})

# Mission 009 - tests d'acceptation du critere 4 (spec pilotage, P4, ADR 0002) :
# tout outil MCP qui modifie un projet existant fonctionne en deux temps.
#   1er appel, sans `jeton` : n'ecrit RIEN sur disque ; rend l'erreur
#     `confirmation_requise` (isError) de forme {ok, classe, code, langue,
#     erreur, action} PLUS `jeton`, `apercu`, `expire_a` ;
#   2e appel avec `jeton`   : ecrit, pour le MEME outil, le MEME projet et les
#     MEMES parametres (fichier + son md5, `remplacer`, `purger` resolu) ;
#   jeton perime, rejoue, inconnu, d'un autre projet / outil / parametres :
#     refuse (classe `jeton_invalide`), sans rien ecrire.
#
# Ecrit a partir de la spec, des fiches 009/010 (contrat du 2026-11-10) et de
# l'ADR 0002, SANS lire l'implementation. Perimetre : appliquer_ugf et
# croiser_onf (quelle que soit la valeur de `purger`) ; lecture, calcul
# (lancer_calcul, annuler_calcul) et exports restent libres.
#
# PAS DE TEST EN BASE : aucun outil MCP n'ecrit en base aujourd'hui (decision de
# Pascal, 2026-11-10). A reecrire (NEMETON_DB_URL_TEST, comptes de lignes avant /
# apres l'apercu) le jour ou un outil MCP ecrira en base.
#
# Contrat : le parametre et le champ s'appellent `jeton` ; la duree de 10
# minutes (UNE constante, `.cf_duree`) est PROVISOIRE (fiche 009, confirmee
# provisoirement par Pascal le 2026-11-10, a revoir apres l'essai VICTOR) et
# mesuree par base::Sys.time() (horloge simulee ci-dessous). Cles de l'apercu
# FIXEES par Pascal (2026-11-10) : appliquer_ugf -> n_ugf_remplacees,
# n_ugf_nouvelles ; croiser_onf -> ecartees (liste d'IDU), n_ecartees,
# n_retenues. Tout jeton CONNU presente est CONSOMME, meme refuse (refus de lien,
# garde rejouee) : un second essai avec les bons parametres est jeton_invalide.
# Langue (arbitrage de 003) : seuls resume_projet et generer_rapport acceptent
# `langue` ; les erreurs de jeton sont en FR par defaut.
# Rouge tant que la mission 010 n'est pas fusionnee.

# Duree de validite d'un jeton (provisoire, ADR 0002) : seule definition du fichier.
.cf_duree <- 10 * 60

.cf_sq <- function(x) {
  sf::st_polygon(list(rbind(c(x, 0), c(x + 100, 0), c(x + 100, 100),
                            c(x, 100), c(x, 0))))
}
.cf_rect <- function(x0, x1) {
  sf::st_polygon(list(rbind(c(x0, 0), c(x1, 0), c(x1, 100), c(x0, 100), c(x0, 0))))
}
.cf_env <- function(env = parent.frame()) {
  root <- withr::local_tempdir(.local_envir = env)
  withr::local_options(nemeton.app_options = list(project_dir = root),
                       .local_envir = env)
  local_mocked_bindings(
    add_r5_to_indicators = function(base_sf, project) base_sf,
    add_regen_r_indicators = function(base_sf, project) base_sf,
    is_db_configured = function() FALSE,
    lock_status = function(pid) NULL,
    .env = env
  )
  root
}
.cf_projet <- function(nom = "Forêt de Dabo") {
  parcelles <- sf::st_sf(
    id = c("21001000AA0001", "21001000AA0002"), section = "AA",
    numero = c("0001", "0002"), contenance = c(1e4, 1e4),
    commune = "Test", code_insee = "21001",
    geometry = sf::st_sfc(.cf_sq(800000), .cf_sq(800100), crs = 2154))
  suppressMessages(projet_creer(nom, parcelles))
}
# Fichier d'UGF ONF valide pour les deux parcelles (cf. test-service_mcp.R).
# `f` : chemin cible (ecrase s'il existe) ; `part` : change le CONTENU du fichier
# sans changer son nom ni sa validite.
.cf_fichier_ugf <- function(f = tempfile(fileext = ".gpkg"), part = 0.9) {
  unlink(f)
  x <- sf::st_sf(idu = c("21001000AA0001", "21001000AA0001", "21001000AA0002"),
                 label_ugf = c("F1-1", "F1-2", "F1-2"),
                 onf_foret_id = "F1", onf_foret_nom = "Forêt test",
                 onf_parcelle = c("1", "2", "2"),
                 onf_domaniale = FALSE, onf_part = part,
                 geometry = sf::st_sfc(list(.cf_rect(800000, 800050),
                                            .cf_rect(800050, 800100),
                                            .cf_sq(800100)), crs = 2154))
  sf::st_write(x, f, quiet = TRUE)
  f
}
# Coeur ONF simule : AA0001 retenue (F1-1), AA0002 ecartee (hors ONF).
.cf_mock_onf <- function(env = parent.frame()) {
  local_mocked_bindings(
    .onf_construire = function(cad, onf, selection, cfg) {
      x <- sf::st_sf(idu = "21001000AA0001", ugf_id = "F1-1",
                     nom_ugf = "Forêt test - parcelle 1", foret_id = "F1",
                     foret_nom = "Forêt test", parcelle = "1",
                     domaniale = FALSE, part_onf = 0.9, surface_m2 = 1e4,
                     geometry = sf::st_sfc(.cf_sq(800000), crs = 2154))
      attr(x, "parcelles") <- data.frame(
        idu = c("21001000AA0001", "21001000AA0002"), retenue = c(TRUE, FALSE),
        raison = c(NA, "hors ONF"), proprietaire = c("COMMUNE", "X"),
        couverture_onf = c(1, 0))
      x
    },
    onf_load_parcelles = function(...) list(status = "ok", parcelles = NULL),
    .env = env)
}

# Empreinte de tout le dossier du projet : noms + md5 (fichiers caches inclus).
.cf_empreinte <- function(id) {
  d <- get_project_path(id)
  f <- sort(list.files(d, recursive = TRUE, all.files = TRUE, full.names = TRUE))
  stats::setNames(unname(tools::md5sum(f)), sub(paste0(d, "/"), "", f, fixed = TRUE))
}
.cf_ugf_etat <- function(id) {
  ug <- load_ug_data(id)
  sort(paste(ug$tenements$parent_parcelle_id,
             ug$ugs$label[match(ug$tenements$ug_id, ug$ugs$ug_id)]))
}
.cf_idus <- function(id) as.character(load_parcels(id)$id)

# Reglage `purger` du projet (parametres ONF), persiste dans le projet.
.cf_reglage_purger <- function(id, valeur) {
  suppressMessages(set_project_onf_params(id, purger = valeur))
}

.cf_prefixe <- "Tool calling failed with error "

# Appelle l'outil comme mcptools et rend ce que voit le client. Une erreur R
# (ex. argument `jeton` inconnu) est consignee et declaree comme echec, pas
# avalee. Le tryCatch ne contient que l'appel, aucune expectation.
.cf_appel <- function(nom, ...) {
  skip_if_not_installed("mcptools")
  skip_if_not_installed("ellmer")
  skip_if_not_installed("jsonlite")
  outils <- mcp_tools()
  noms <- vapply(outils, function(t) t@name, character(1))
  expect_true(nom %in% noms, info = paste("outil absent :", nom))
  err <- NULL
  res <- tryCatch(outils[[match(nom, noms)]](...),
                  error = function(e) { err <<- conditionMessage(e); NULL })
  expect_null(err, info = paste(nom, ": erreur R a l'appel"))
  if (!is.null(err)) return(list(is_error = NA, texte = "", json = NULL))
  rep <- getFromNamespace("as_tool_call_result", "mcptools")(list(id = 1L), res)
  texte <- paste(vapply(rep$result$content, function(x) x$text %||% "",
                        character(1)), collapse = "\n")
  brut <- if (startsWith(texte, .cf_prefixe)) substring(texte, nchar(.cf_prefixe) + 1L) else texte
  j <- tryCatch(jsonlite::fromJSON(brut, simplifyVector = TRUE), error = function(e) NULL)
  list(is_error = isTRUE(rep$result$isError), texte = texte, json = j)
}

# Erreur classee : isError, prefixe, `code` ET `classe` exacts (pas de sous-chaine).
.cf_attend_erreur <- function(r, code, info = code) {
  expect_true(r$is_error, info = info)
  expect_true(startsWith(r$texte, .cf_prefixe), info = info)
  expect_false(is.null(r$json), info = paste(info, ": JSON illisible"))
  expect_identical(r$json$code, code, info = info)
  expect_identical(r$json$classe, paste0("nemetonshiny_", code), info = info)
  expect_true(is.character(r$json$action) && any(nzchar(r$json$action)),
              info = paste(info, ": action proposee absente"))
  invisible(r$json)
}

# Premier appel : UNIQUEMENT l'erreur `confirmation_requise`, de forme commune
# {ok, classe, code, langue, erreur, action} + jeton, apercu, expire_a.
# Rend le JSON (ou NULL si illisible) ; `.cf_jeton()` en extrait le jeton.
.cf_premier <- function(r, info = "") {
  expect_false(is.null(r$json), info = paste(info, ": JSON illisible"))
  if (is.null(r$json)) return(NULL)
  j <- .cf_attend_erreur(r, "confirmation_requise", info)
  expect_false(isTRUE(j$ok), info = paste(info, ": ok doit etre FALSE"))
  expect_identical(j$langue, "fr", info = info)
  expect_true(is.character(j$erreur) && any(nzchar(j$erreur)), info = info)
  for (champ in c("ok", "classe", "code", "langue", "erreur", "action",
                  "jeton", "apercu", "expire_a")) {
    expect_true(champ %in% names(j), info = paste(info, ": champ absent :", champ))
  }
  jeton <- j$jeton
  expect_true(is.character(jeton) && length(jeton) == 1L && nzchar(jeton),
              info = paste(info, ": `jeton` (chaine non vide) absent"))
  expect_true(length(unlist(j$apercu)) > 0L,
              info = paste(info, ": `apercu` vide"))
  .cf_expire_ok(j$expire_a, info)
  j
}
.cf_jeton <- function(j) {
  if (is.null(j) || !is.character(j$jeton) || length(j$jeton) != 1L) return(NULL)
  j$jeton
}
# expire_a : ISO 8601 avec fuseau (spec 2), dans le futur, a au plus `.cf_duree`
# (+ marge d'1 min) : duree PROVISOIRE, voir l'en-tete.
.cf_expire_ok <- function(x, info = "") {
  ok <- is.character(x) && length(x) == 1L &&
    grepl("^\\d{4}-\\d{2}-\\d{2}T\\d{2}:\\d{2}:\\d{2}(\\.\\d+)?(Z|[+-]\\d{2}(:?\\d{2})?)$", x)
  expect_true(ok, info = paste(info, ": expire_a n'est pas ISO 8601 avec fuseau :", paste(x, collapse = "")))
  if (!ok) return(invisible())
  y <- sub("\\.\\d+", "", x)
  y <- sub("Z$", "+0000", y)
  y <- sub("([+-]\\d{2}):(\\d{2})$", "\\1\\2", y)
  y <- sub("([+-]\\d{2})$", "\\100", y)
  t <- as.POSIXct(y, format = "%Y-%m-%dT%H:%M:%S%z", tz = "UTC")
  dt <- as.numeric(difftime(t, Sys.time(), units = "secs"))
  expect_true(!is.na(dt) && dt > 0 && dt <= .cf_duree + 60,
              info = paste(info, ": expire_a hors de (0, duree + 1 min] :", x))
}

# Cas de croiser_onf : valeur de `purger` passee, reglage du projet, valeur
# resolue. Un `purger` explicite est teste avec le reglage OPPOSE (l'explicite
# l'emporte) ; NULL est teste avec les deux reglages (le defaut du projet est TRUE).
.cf_cas_croiser <- list(
  list(lib = "purger = TRUE (reglage projet FALSE)", purger = TRUE,
       reglage = FALSE, resolu = TRUE),
  list(lib = "purger = FALSE (reglage projet TRUE)", purger = FALSE,
       reglage = TRUE, resolu = FALSE),
  list(lib = "purger = NULL (reglage projet TRUE)", purger = NULL,
       reglage = TRUE, resolu = TRUE),
  list(lib = "purger = NULL (reglage projet FALSE)", purger = NULL,
       reglage = FALSE, resolu = FALSE))
.cf_croiser <- function(id, purger, ...) {
  if (is.null(purger)) .cf_appel("croiser_onf", projet = id, ...)
  else .cf_appel("croiser_onf", projet = id, purger = purger, ...)
}
.cf_idus_attendus <- function(resolu) {
  if (resolu) "21001000AA0001" else c("21001000AA0001", "21001000AA0002")
}
# Un jeton deja presente (refuse ou non) est consomme : le presenter de nouveau,
# avec les BONS parametres, est refuse et n'ecrit rien.
.cf_expect_consomme <- function(r, id, avant, info) {
  .cf_attend_erreur(r, "jeton_invalide", paste(info, ": jeton non consomme"))
  expect_identical(.cf_empreinte(id), avant, info = info)
}

# --------------------------------------------- 1. sans jeton : rien n'est ecrit

test_that("appliquer_ugf sans jeton : rien d'ecrit, apercu = cles fixees avec leurs valeurs", {
  skip_if_not_installed("arrow")
  .cf_env()
  id <- .cf_projet()
  avant <- .cf_empreinte(id)
  etat <- .cf_ugf_etat(id)
  n_remplacees <- nrow(load_ug_data(id)$ugs)
  r <- .cf_appel("appliquer_ugf", projet = id, fichier = .cf_fichier_ugf())
  j <- .cf_premier(r, "appliquer_ugf")
  expect_identical(.cf_empreinte(id), avant)   # disque : octet pour octet
  expect_identical(.cf_ugf_etat(id), etat)     # et vu par l'application
  # spec P4 + cles fixees : UGF actuelles du projet (remplacees) et UGF du fichier.
  expect_identical(j$apercu$n_ugf_remplacees, n_remplacees)
  expect_identical(j$apercu$n_ugf_nouvelles, 2L)
})

for (cas in .cf_cas_croiser) {
  local({
    cas <- cas
    test_that(paste0("croiser_onf (", cas$lib, ") sans jeton : rien d'ecrit, apercu exact"), {
      skip_if_not_installed("arrow")
      .cf_env()
      id <- .cf_projet()
      .cf_mock_onf()
      .cf_reglage_purger(id, cas$reglage)
      avant <- .cf_empreinte(id)
      j <- .cf_premier(.cf_croiser(id, cas$purger), cas$lib)
      expect_identical(.cf_empreinte(id), avant)
      expect_identical(.cf_idus(id), c("21001000AA0001", "21001000AA0002"))
      expect_true(all(c("ecartees", "n_ecartees", "n_retenues") %in% names(j$apercu)),
                  info = "apercu : cles ecartees / n_ecartees / n_retenues")
      # regle de somme : toute parcelle du projet est ecartee OU retenue.
      expect_identical(j$apercu$n_ecartees + j$apercu$n_retenues,
                       length(.cf_idus(id)))
      if (cas$resolu) {
        # purger resolu TRUE (spec P4) : "n parcelles retirees" + leurs IDU
        expect_identical(j$apercu$ecartees, "21001000AA0002")
        expect_identical(j$apercu$n_ecartees, 1L)
        expect_identical(j$apercu$n_retenues, 1L)
      } else {
        # purger resolu FALSE (arbitrage de Pascal, 2026-11-10) : l'apercu dit ce
        # qui va changer, donc rien n'est ecarte et toutes les parcelles restent.
        expect_length(unlist(j$apercu$ecartees), 0L)
        expect_identical(j$apercu$n_ecartees, 0L)
        expect_identical(j$apercu$n_retenues, 2L)
      }
    })
  })
}

test_that("deux appels sans jeton n'ecrivent rien et rendent deux jetons distincts", {
  skip_if_not_installed("arrow")
  .cf_env()
  id <- .cf_projet()
  f <- .cf_fichier_ugf()
  avant <- .cf_empreinte(id)
  j1 <- .cf_jeton(.cf_premier(.cf_appel("appliquer_ugf", projet = id, fichier = f), "1er"))
  j2 <- .cf_jeton(.cf_premier(.cf_appel("appliquer_ugf", projet = id, fichier = f), "2e"))
  expect_identical(.cf_empreinte(id), avant)
  if (!is.null(j1) && !is.null(j2)) expect_false(identical(j1, j2))
})

# ------------------------------------------------- 2. jeton valide : l'ecriture

test_that("appliquer_ugf avec le jeton du premier appel : l'ecriture a lieu", {
  skip_if_not_installed("arrow")
  .cf_env()
  id <- .cf_projet()
  f <- .cf_fichier_ugf()
  jeton <- .cf_jeton(.cf_premier(.cf_appel("appliquer_ugf", projet = id, fichier = f)))
  expect_false(is.null(jeton), info = "pas de jeton au premier appel")
  if (is.null(jeton)) return(invisible())
  r <- .cf_appel("appliquer_ugf", projet = id, fichier = f, jeton = jeton)
  expect_false(r$is_error)
  expect_true(isTRUE(r$json$ok))
  expect_identical(r$json$n_ugf, 2L)
  expect_identical(r$json$n_tenements, 3L)
  expect_setequal(load_ug_data(id)$ugs$onf_parcelle, c("1", "2"))
})

test_that("appliquer_ugf remplacer = FALSE : le jeton ecrit avec les memes parametres", {
  skip_if_not_installed("arrow")
  .cf_env()
  id <- .cf_projet()
  f <- .cf_fichier_ugf()
  avant <- .cf_empreinte(id)
  jeton <- .cf_jeton(.cf_premier(
    .cf_appel("appliquer_ugf", projet = id, fichier = f, remplacer = FALSE)))
  expect_false(is.null(jeton), info = "pas de jeton au premier appel")
  if (is.null(jeton)) return(invisible())
  expect_identical(.cf_empreinte(id), avant)    # l'apercu n'a rien ecrit
  r <- .cf_appel("appliquer_ugf", projet = id, fichier = f, remplacer = FALSE,
                 jeton = jeton)
  expect_false(r$is_error)
  expect_true(isTRUE(r$json$ok))
  # disque : le fichier couvre les deux parcelles, donc meme resultat que le
  # remplacement complet ; le projet a bien change.
  expect_false(identical(.cf_empreinte(id), avant))
  expect_identical(r$json$n_ugf, 2L)
  expect_identical(r$json$n_tenements, 3L)
  expect_setequal(load_ug_data(id)$ugs$onf_parcelle, c("1", "2"))
  expect_identical(.cf_idus(id), c("21001000AA0001", "21001000AA0002"))
})

for (cas in .cf_cas_croiser) {
  local({
    cas <- cas
    test_that(paste0("croiser_onf (", cas$lib, ") avec le jeton : l'ecriture a lieu"), {
      skip_if_not_installed("arrow")
      .cf_env()
      id <- .cf_projet()
      .cf_mock_onf()
      .cf_reglage_purger(id, cas$reglage)
      avant <- .cf_empreinte(id)
      jeton <- .cf_jeton(.cf_premier(.cf_croiser(id, cas$purger), cas$lib))
      expect_false(is.null(jeton), info = "pas de jeton au premier appel")
      if (is.null(jeton)) return(invisible())
      r <- .cf_croiser(id, cas$purger, jeton = jeton)
      expect_false(r$is_error)
      expect_true(isTRUE(r$json$ok))
      # le projet a change sur disque (UGF ONF ecrites) ; parcelles selon le
      # `purger` RESOLU (explicite, sinon reglage du projet).
      expect_false(identical(.cf_empreinte(id), avant))
      expect_identical(.cf_idus(id), .cf_idus_attendus(cas$resolu))
    })
  })
}

# ------------------------------------------------------ 3. jeton refuse, rien d'ecrit

test_that("jeton inconnu : jeton_invalide, rien d'ecrit (les deux outils)", {
  skip_if_not_installed("arrow")
  .cf_env()
  id <- .cf_projet()
  .cf_mock_onf()
  avant <- .cf_empreinte(id)
  r <- .cf_appel("appliquer_ugf", projet = id, fichier = .cf_fichier_ugf(),
                 jeton = "bidon")
  .cf_attend_erreur(r, "jeton_invalide", "appliquer_ugf")
  r <- .cf_appel("croiser_onf", projet = id, purger = TRUE, jeton = "bidon")
  .cf_attend_erreur(r, "jeton_invalide", "croiser_onf")
  expect_identical(.cf_empreinte(id), avant)
})

test_that("appliquer_ugf rejoue : le second usage est refuse, l'etat ecrit reste", {
  skip_if_not_installed("arrow")
  .cf_env()
  id <- .cf_projet()
  f <- .cf_fichier_ugf()
  jeton <- .cf_jeton(.cf_premier(.cf_appel("appliquer_ugf", projet = id, fichier = f)))
  expect_false(is.null(jeton), info = "pas de jeton au premier appel")
  if (is.null(jeton)) return(invisible())
  expect_true(isTRUE(.cf_appel("appliquer_ugf", projet = id, fichier = f,
                               jeton = jeton)$json$ok))
  apres <- .cf_empreinte(id)
  etat <- .cf_ugf_etat(id)
  r <- .cf_appel("appliquer_ugf", projet = id, fichier = f, jeton = jeton)
  .cf_attend_erreur(r, "jeton_invalide", "rejeu")
  expect_identical(.cf_empreinte(id), apres)
  expect_identical(.cf_ugf_etat(id), etat)
})

test_that("croiser_onf rejoue : refuse, et le disque reste dans l'etat de la 1re ecriture", {
  skip_if_not_installed("arrow")
  .cf_env()
  id <- .cf_projet()
  .cf_mock_onf()
  jeton <- .cf_jeton(.cf_premier(.cf_appel("croiser_onf", projet = id, purger = TRUE)))
  expect_false(is.null(jeton), info = "pas de jeton au premier appel")
  if (is.null(jeton)) return(invisible())
  expect_true(isTRUE(.cf_appel("croiser_onf", projet = id, purger = TRUE,
                               jeton = jeton)$json$ok))
  apres <- .cf_empreinte(id)
  expect_identical(.cf_idus(id), "21001000AA0001")   # purge faite une fois
  r <- .cf_appel("croiser_onf", projet = id, purger = TRUE, jeton = jeton)
  .cf_attend_erreur(r, "jeton_invalide", "rejeu croiser_onf")
  expect_identical(.cf_empreinte(id), apres)
  expect_identical(.cf_idus(id), "21001000AA0001")
})

test_that("jeton d'un autre projet : refuse, rien d'ecrit, et le jeton est consomme", {
  skip_if_not_installed("arrow")
  .cf_env()
  a <- .cf_projet("Projet A")
  b <- .cf_projet("Projet B")
  f <- .cf_fichier_ugf()
  jeton_a <- .cf_jeton(.cf_premier(.cf_appel("appliquer_ugf", projet = a, fichier = f)))
  expect_false(is.null(jeton_a), info = "pas de jeton au premier appel")
  if (is.null(jeton_a)) return(invisible())
  avant_a <- .cf_empreinte(a)
  avant_b <- .cf_empreinte(b)
  r <- .cf_appel("appliquer_ugf", projet = b, fichier = f, jeton = jeton_a)
  .cf_attend_erreur(r, "jeton_invalide", "jeton de A sur B")
  expect_identical(.cf_empreinte(b), avant_b)
  expect_identical(.cf_empreinte(a), avant_a)
  # consomme : meme sur son projet d'origine, avec les bons parametres
  r <- .cf_appel("appliquer_ugf", projet = a, fichier = f, jeton = jeton_a)
  .cf_expect_consomme(r, a, avant_a, "A apres refus sur B")
  expect_identical(.cf_empreinte(b), avant_b)
})

# --- lien jeton <-> outil et parametres (contrat du 2026-11-10) ; chaque refus
# --- consomme le jeton (arbitrage du 2026-11-10)

test_that("jeton d'appliquer_ugf rejoue sur croiser_onf : jeton_invalide, rien d'ecrit, consomme", {
  skip_if_not_installed("arrow")
  .cf_env()
  id <- .cf_projet()
  .cf_mock_onf()
  f <- .cf_fichier_ugf()
  jeton <- .cf_jeton(.cf_premier(.cf_appel("appliquer_ugf", projet = id, fichier = f)))
  expect_false(is.null(jeton), info = "pas de jeton au premier appel")
  if (is.null(jeton)) return(invisible())
  avant <- .cf_empreinte(id)
  r <- .cf_appel("croiser_onf", projet = id, purger = TRUE, jeton = jeton)
  .cf_attend_erreur(r, "jeton_invalide", "autre outil")
  expect_identical(.cf_empreinte(id), avant)
  # consomme : sur son propre outil, avec ses propres parametres
  r <- .cf_appel("appliquer_ugf", projet = id, fichier = f, jeton = jeton)
  .cf_expect_consomme(r, id, avant, "apres refus sur croiser_onf")
})

test_that("jeton de croiser_onf rejoue sur appliquer_ugf : jeton_invalide, rien d'ecrit, consomme", {
  skip_if_not_installed("arrow")
  .cf_env()
  id <- .cf_projet()
  .cf_mock_onf()
  jeton <- .cf_jeton(.cf_premier(.cf_appel("croiser_onf", projet = id, purger = TRUE)))
  expect_false(is.null(jeton), info = "pas de jeton au premier appel")
  if (is.null(jeton)) return(invisible())
  avant <- .cf_empreinte(id)
  r <- .cf_appel("appliquer_ugf", projet = id, fichier = .cf_fichier_ugf(), jeton = jeton)
  .cf_attend_erreur(r, "jeton_invalide", "autre outil (sens inverse)")
  expect_identical(.cf_empreinte(id), avant)
  r <- .cf_appel("croiser_onf", projet = id, purger = TRUE, jeton = jeton)
  .cf_expect_consomme(r, id, avant, "apres refus sur appliquer_ugf")
  expect_identical(.cf_idus(id), c("21001000AA0001", "21001000AA0002"))
})

test_that("croiser_onf : `purger` change entre les deux appels -> jeton_invalide, consomme", {
  skip_if_not_installed("arrow")
  .cf_env()
  id <- .cf_projet()
  .cf_mock_onf()
  jeton <- .cf_jeton(.cf_premier(.cf_appel("croiser_onf", projet = id, purger = FALSE)))
  expect_false(is.null(jeton), info = "pas de jeton au premier appel")
  if (is.null(jeton)) return(invisible())
  avant <- .cf_empreinte(id)
  # le jeton a ete delivre pour purger = FALSE : TRUE (qui retirerait AA0002) est refuse
  r <- .cf_appel("croiser_onf", projet = id, purger = TRUE, jeton = jeton)
  .cf_attend_erreur(r, "jeton_invalide", "purger FALSE -> TRUE")
  expect_identical(.cf_empreinte(id), avant)
  expect_identical(.cf_idus(id), c("21001000AA0001", "21001000AA0002"))
  # consomme : avec le bon `purger` (FALSE), le jeton ne sert plus
  r <- .cf_appel("croiser_onf", projet = id, purger = FALSE, jeton = jeton)
  .cf_expect_consomme(r, id, avant, "apres refus de purger")
})

test_that("appliquer_ugf : `remplacer` change entre les deux appels -> jeton_invalide, consomme", {
  skip_if_not_installed("arrow")
  .cf_env()
  id <- .cf_projet()
  f <- .cf_fichier_ugf()
  jeton <- .cf_jeton(.cf_premier(
    .cf_appel("appliquer_ugf", projet = id, fichier = f, remplacer = TRUE)))
  expect_false(is.null(jeton), info = "pas de jeton au premier appel")
  if (is.null(jeton)) return(invisible())
  avant <- .cf_empreinte(id)
  r <- .cf_appel("appliquer_ugf", projet = id, fichier = f, remplacer = FALSE,
                 jeton = jeton)
  .cf_attend_erreur(r, "jeton_invalide", "remplacer TRUE -> FALSE")
  expect_identical(.cf_empreinte(id), avant)
  r <- .cf_appel("appliquer_ugf", projet = id, fichier = f, remplacer = TRUE,
                 jeton = jeton)
  .cf_expect_consomme(r, id, avant, "apres refus de remplacer")
})

test_that("appliquer_ugf : meme nom de fichier, autre contenu -> jeton_invalide (md5), consomme", {
  skip_if_not_installed("arrow")
  .cf_env()
  id <- .cf_projet()
  f <- .cf_fichier_ugf(part = 0.9)
  jeton <- .cf_jeton(.cf_premier(.cf_appel("appliquer_ugf", projet = id, fichier = f)))
  expect_false(is.null(jeton), info = "pas de jeton au premier appel")
  if (is.null(jeton)) return(invisible())
  md5_avant <- unname(tools::md5sum(f))
  orig <- withr::local_tempfile(fileext = ".gpkg")
  expect_true(file.copy(f, orig))                  # octets d'origine, tels quels
  .cf_fichier_ugf(f = f, part = 0.5)               # meme chemin, autre contenu
  expect_false(identical(unname(tools::md5sum(f)), md5_avant))  # garde du test
  avant <- .cf_empreinte(id)
  r <- .cf_appel("appliquer_ugf", projet = id, fichier = f, jeton = jeton)
  .cf_attend_erreur(r, "jeton_invalide", "fichier modifie")
  expect_identical(.cf_empreinte(id), avant)
  # consomme : le fichier d'origine restitue ne ressuscite pas le jeton
  expect_true(file.copy(orig, f, overwrite = TRUE))
  expect_identical(unname(tools::md5sum(f)), md5_avant)         # garde du test
  r <- .cf_appel("appliquer_ugf", projet = id, fichier = f, jeton = jeton)
  .cf_expect_consomme(r, id, avant, "fichier restitue apres refus")
})

# --- `purger` RESOLU : le lien porte sur la valeur effective, pas sur l'argument

test_that("jeton obtenu avec purger = NULL, presente avec purger = TRUE explicite (reglage TRUE) : accepte", {
  skip_if_not_installed("arrow")
  .cf_env()
  id <- .cf_projet()
  .cf_mock_onf()
  .cf_reglage_purger(id, TRUE)
  jeton <- .cf_jeton(.cf_premier(.cf_appel("croiser_onf", projet = id)))
  expect_false(is.null(jeton), info = "pas de jeton au premier appel")
  if (is.null(jeton)) return(invisible())
  r <- .cf_appel("croiser_onf", projet = id, purger = TRUE, jeton = jeton)
  expect_false(r$is_error)
  expect_true(isTRUE(r$json$ok))
  expect_identical(.cf_idus(id), "21001000AA0001")
})

test_that("reglage `purger` du projet modifie entre les deux appels (purger = NULL) : jeton_invalide, consomme", {
  skip_if_not_installed("arrow")
  .cf_env()
  id <- .cf_projet()
  .cf_mock_onf()
  .cf_reglage_purger(id, TRUE)
  jeton <- .cf_jeton(.cf_premier(.cf_appel("croiser_onf", projet = id)))
  expect_false(is.null(jeton), info = "pas de jeton au premier appel")
  if (is.null(jeton)) return(invisible())
  .cf_reglage_purger(id, FALSE)                 # le purger resolu passe de TRUE a FALSE
  avant <- .cf_empreinte(id)
  r <- .cf_appel("croiser_onf", projet = id, jeton = jeton)
  .cf_attend_erreur(r, "jeton_invalide", "reglage modifie")
  expect_identical(.cf_empreinte(id), avant)
  expect_identical(.cf_idus(id), c("21001000AA0001", "21001000AA0002"))
  # consomme : meme reglage remis comme au premier appel
  .cf_reglage_purger(id, TRUE)
  avant <- .cf_empreinte(id)
  r <- .cf_appel("croiser_onf", projet = id, jeton = jeton)
  .cf_expect_consomme(r, id, avant, "reglage restitue apres refus")
  expect_identical(.cf_idus(id), c("21001000AA0001", "21001000AA0002"))
})

# --- liens sur TOUS les reglages ONF resolus (arbitrage de Pascal, 2026-11-10)

for (modif in list(list(lib = "seuil_couverture", args = list(seuil_couverture = 0.8)),
                   list(lib = "domanialite", args = list(domanialite = "domaniale")))) {
  local({
    modif <- modif
    test_that(paste0("reglage ONF `", modif$lib, "` modifie entre apercu et confirmation : jeton_invalide, rien d'ecrit, consomme"), {
      skip_if_not_installed("arrow")
      .cf_env()
      id <- .cf_projet()
      .cf_mock_onf()
      .cf_reglage_purger(id, TRUE)
      jeton <- .cf_jeton(.cf_premier(.cf_appel("croiser_onf", projet = id)))
      expect_false(is.null(jeton), info = "pas de jeton au premier appel")
      if (is.null(jeton)) return(invisible())
      avant_reglage <- project_onf_params(load_project_metadata(id))
      suppressMessages(do.call(set_project_onf_params, c(list(id, purger = TRUE), modif$args)))
      expect_false(identical(project_onf_params(load_project_metadata(id)),
                             avant_reglage),
                   info = "garde du test : le reglage a bien change")
      avant <- .cf_empreinte(id)
      r <- .cf_appel("croiser_onf", projet = id, jeton = jeton)
      .cf_attend_erreur(r, "jeton_invalide", paste("reglage modifie :", modif$lib))
      expect_identical(.cf_empreinte(id), avant)
      expect_identical(.cf_idus(id), c("21001000AA0001", "21001000AA0002"))
      # consomme : reglage remis comme au premier appel, le jeton reste mort
      suppressMessages(set_project_onf_params(id, purger = TRUE))
      expect_identical(project_onf_params(load_project_metadata(id)), avant_reglage)
      avant <- .cf_empreinte(id)
      r <- .cf_appel("croiser_onf", projet = id, jeton = jeton)
      .cf_expect_consomme(r, id, avant, "reglage restitue apres refus")
    })
  })
}

# --- duree : `.cf_duree` (PROVISOIRE, fiche 009). Horloge simulee via base::Sys.time.
.cf_horloge <- function(decalage_s, env = parent.frame()) {
  reel <- base::Sys.time
  local_mocked_bindings(Sys.time = function() reel() + decalage_s,
                        .package = "base", .env = env)
}

test_that("jeton perime (plus de la duree) : refuse, rien d'ecrit", {
  skip_if_not_installed("arrow")
  .cf_env()
  id <- .cf_projet()
  f <- .cf_fichier_ugf()
  jeton <- .cf_jeton(.cf_premier(.cf_appel("appliquer_ugf", projet = id, fichier = f)))
  expect_false(is.null(jeton), info = "pas de jeton au premier appel")
  if (is.null(jeton)) return(invisible())
  avant <- .cf_empreinte(id)
  .cf_horloge(.cf_duree + 1)
  r <- .cf_appel("appliquer_ugf", projet = id, fichier = f, jeton = jeton)
  .cf_attend_erreur(r, "jeton_invalide", "perime")
  expect_identical(.cf_empreinte(id), avant)
})

test_that("jeton a duree - 1 min : encore valable, l'ecriture a lieu (borne de la duree)", {
  skip_if_not_installed("arrow")
  .cf_env()
  id <- .cf_projet()
  f <- .cf_fichier_ugf()
  jeton <- .cf_jeton(.cf_premier(.cf_appel("appliquer_ugf", projet = id, fichier = f)))
  expect_false(is.null(jeton), info = "pas de jeton au premier appel")
  if (is.null(jeton)) return(invisible())
  .cf_horloge(.cf_duree - 60)
  r <- .cf_appel("appliquer_ugf", projet = id, fichier = f, jeton = jeton)
  expect_false(r$is_error)
  expect_true(isTRUE(r$json$ok))
})

# ------------------------------------------------------------ 4. cas du terrain

# Ordre des erreurs (contrat du 2026-11-10) : erreurs metier et gardes AVANT
# l'apercu : pas de confirmation_requise, pas de jeton, rien d'ecrit.
.cf_attend_sans_jeton <- function(r, code, id, avant, info = code) {
  j <- .cf_attend_erreur(r, code, info)
  expect_false("jeton" %in% names(j), info = paste(info, ": jeton rendu malgre l'erreur"))
  expect_false("apercu" %in% names(j), info = paste(info, ": apercu rendu malgre l'erreur"))
  expect_identical(.cf_empreinte(id), avant)
}

test_that("fichier absent au premier appel : fichier_invalide (pas d'apercu, pas de jeton)", {
  skip_if_not_installed("arrow")
  .cf_env()
  id <- .cf_projet()
  avant <- .cf_empreinte(id)
  r <- .cf_appel("appliquer_ugf", projet = id,
                 fichier = file.path(tempdir(), "absent-009.gpkg"))
  .cf_attend_sans_jeton(r, "fichier_invalide", id, avant)
})

test_that("idu_inconnu et pavage_invalide au premier appel : erreur metier, pas de jeton", {
  skip_if_not_installed("arrow")
  .cf_env()
  id <- .cf_projet()
  avant <- .cf_empreinte(id)
  f <- tempfile(fileext = ".gpkg")
  x <- sf::st_sf(idu = c("21001000AA0001", "21001000ZZ0009"),
                 label_ugf = c("F1-1", "F1-2"), onf_foret_id = "F1",
                 onf_foret_nom = "Forêt test", onf_parcelle = c("1", "2"),
                 onf_domaniale = FALSE, onf_part = 0.9,
                 geometry = sf::st_sfc(list(.cf_sq(800000), .cf_sq(900000)), crs = 2154))
  sf::st_write(x, f, quiet = TRUE)
  .cf_attend_sans_jeton(.cf_appel("appliquer_ugf", projet = id, fichier = f),
                        "idu_inconnu", id, avant)
  # pavage inexact : AA0001 couverte a moitie, AA0002 entiere
  g <- tempfile(fileext = ".gpkg")
  y <- sf::st_sf(idu = c("21001000AA0001", "21001000AA0002"),
                 label_ugf = c("F1-1", "F1-2"), onf_foret_id = "F1",
                 onf_foret_nom = "Forêt test", onf_parcelle = c("1", "2"),
                 onf_domaniale = FALSE, onf_part = 0.9,
                 geometry = sf::st_sfc(list(.cf_rect(800000, 800050), .cf_sq(800100)),
                                       crs = 2154))
  sf::st_write(y, g, quiet = TRUE)
  .cf_attend_sans_jeton(.cf_appel("appliquer_ugf", projet = id, fichier = g),
                        "pavage_invalide", id, avant)
})

test_that("croiser_onf : source ONF injoignable au premier appel -> onf_echec, pas de jeton", {
  skip_if_not_installed("arrow")
  .cf_env()
  id <- .cf_projet()
  local_mocked_bindings(.onf_construire = function(...) NULL,
                        onf_load_parcelles = function(...) list(status = "ok", parcelles = NULL))
  avant <- .cf_empreinte(id)
  .cf_attend_sans_jeton(.cf_appel("croiser_onf", projet = id, purger = TRUE),
                        "onf_echec", id, avant)
})

test_that("projet verrouille au premier appel : projet_verrouille, pas de jeton (les deux outils)", {
  skip_if_not_installed("arrow")
  .cf_env()
  id <- .cf_projet()
  .cf_mock_onf()
  local_mocked_bindings(lock_status = function(pid) list(holder_label = "Pascal", stale = FALSE))
  avant <- .cf_empreinte(id)
  .cf_attend_sans_jeton(
    .cf_appel("appliquer_ugf", projet = id, fichier = .cf_fichier_ugf()),
    "projet_verrouille", id, avant, "appliquer_ugf")
  .cf_attend_sans_jeton(.cf_appel("croiser_onf", projet = id, purger = TRUE),
                        "projet_verrouille", id, avant, "croiser_onf")
})

test_that("calcul en cours au premier appel : calcul_en_cours, pas de jeton (les deux outils)", {
  skip_if_not_installed("arrow")
  .cf_env()
  id <- .cf_projet()
  .cf_mock_onf()
  local_mocked_bindings(progress_state_age_sec = function(id) 5)
  avant <- .cf_empreinte(id)
  .cf_attend_sans_jeton(
    .cf_appel("appliquer_ugf", projet = id, fichier = .cf_fichier_ugf()),
    "calcul_en_cours", id, avant, "appliquer_ugf")
  .cf_attend_sans_jeton(.cf_appel("croiser_onf", projet = id, purger = TRUE),
                        "calcul_en_cours", id, avant, "croiser_onf")
})

# Les gardes sont rejouees a la confirmation : un jeton valide ne les contourne
# pas, et la garde qui refuse CONSOMME le jeton (la garde levee, il ne revit pas).
test_that("un jeton valide ne contourne pas le verrou pris entre-temps ; le jeton est consomme", {
  skip_if_not_installed("arrow")
  .cf_env()
  id <- .cf_projet()
  f <- .cf_fichier_ugf()
  jeton <- .cf_jeton(.cf_premier(.cf_appel("appliquer_ugf", projet = id, fichier = f)))
  expect_false(is.null(jeton), info = "pas de jeton au premier appel")
  if (is.null(jeton)) return(invisible())
  avant <- .cf_empreinte(id)
  with_mocked_bindings(
    {
      r <- .cf_appel("appliquer_ugf", projet = id, fichier = f, jeton = jeton)
      .cf_attend_erreur(r, "projet_verrouille", "verrou apres jeton")
      expect_identical(.cf_empreinte(id), avant)
    },
    lock_status = function(pid) list(holder_label = "Pascal", stale = FALSE))
  # verrou leve, memes parametres : le jeton est mort
  r <- .cf_appel("appliquer_ugf", projet = id, fichier = f, jeton = jeton)
  .cf_expect_consomme(r, id, avant, "verrou leve")
})

test_that("un jeton valide ne contourne pas un calcul lance entre-temps (croiser_onf) ; consomme", {
  skip_if_not_installed("arrow")
  .cf_env()
  id <- .cf_projet()
  .cf_mock_onf()
  jeton <- .cf_jeton(.cf_premier(.cf_appel("croiser_onf", projet = id, purger = TRUE)))
  expect_false(is.null(jeton), info = "pas de jeton au premier appel")
  if (is.null(jeton)) return(invisible())
  avant <- .cf_empreinte(id)
  with_mocked_bindings(
    {
      r <- .cf_appel("croiser_onf", projet = id, purger = TRUE, jeton = jeton)
      .cf_attend_erreur(r, "calcul_en_cours", "calcul apres jeton")
      expect_identical(.cf_empreinte(id), avant)
    },
    progress_state_age_sec = function(id) 5)
  # calcul termine, memes parametres : le jeton est mort
  r <- .cf_appel("croiser_onf", projet = id, purger = TRUE, jeton = jeton)
  .cf_expect_consomme(r, id, avant, "calcul termine")
  expect_identical(.cf_idus(id), c("21001000AA0001", "21001000AA0002"))
})

# Langue (arbitrage de 003) : seuls resume_projet et generer_rapport acceptent
# `langue` ; les erreurs de jeton sont en FR par defaut.
test_that("jeton_invalide : langue FR par defaut (appliquer_ugf n'accepte pas `langue`)", {
  skip_if_not_installed("arrow")
  .cf_env()
  id <- .cf_projet()
  r <- .cf_appel("appliquer_ugf", projet = id, fichier = .cf_fichier_ugf(),
                 jeton = "bidon")
  j <- .cf_attend_erreur(r, "jeton_invalide", "defaut")
  expect_identical(j$langue, "fr")
  expect_true(is.character(j$erreur) && any(nzchar(j$erreur)))
})

# ----------------------------------------- 5. perimetre : les outils libres restent libres

test_that("lecture, calcul et exports n'exigent aucun jeton (ADR 0002, perimetre)", {
  skip_if_not_installed("arrow")
  .cf_env()
  id <- .cf_projet()
  ugs <- load_ug_data(id)$ugs
  suppressMessages(save_indicators(id, data.frame(
    ug_id = ugs$ug_id, indicateur_b1_protection = c(40, 60),
    indicateur_c1_biomasse = c(70, 90))))
  local_mocked_bindings(
    generate_report_pdf = function(project, family_scores, output_file, ...) {
      writeLines("pdf", output_file); output_file
    },
    .mcp_spawn = function(...) invisible(TRUE))
  libres <- list(
    lister_projets = .cf_appel("lister_projets"),
    resume_projet = .cf_appel("resume_projet", projet = id),
    etat_calcul = .cf_appel("etat_calcul", projet = id),
    url_app = .cf_appel("url_app", projet = id),
    exporter_gpkg = suppressMessages(.cf_appel("exporter_gpkg", projet = id)),
    generer_rapport = .cf_appel("generer_rapport", projet = id, synthese = "Texte."),
    annuler_calcul = .cf_appel("annuler_calcul", projet = id),
    lancer_calcul = .cf_appel("lancer_calcul", projet = id))
  for (nom in names(libres)) {
    r <- libres[[nom]]
    expect_false(r$is_error, info = nom)
    expect_false(identical(r$json$code, "confirmation_requise"), info = nom)
    expect_false("jeton" %in% names(r$json), info = paste(nom, ": jeton inattendu"))
  }
  expect_true(isTRUE(libres$exporter_gpkg$json$ok))
  expect_true(isTRUE(libres$generer_rapport$json$ok))
  expect_true(isTRUE(libres$annuler_calcul$json$ok))
})

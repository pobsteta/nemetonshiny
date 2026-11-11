# Mission 012 - tests d'acceptation : `generer_rapport` et `croiser_onf`
# detaches sur le modele `lancer_calcul` / `etat_calcul` (spec, P3 ; ADR 0004 ;
# decisions de Pascal du 2026-11-10 et du 2026-11-11).
#
# Ecrit a partir de la fiche 012, de la spec, des fiches 003/010/016 et de
# l'interface PUBLIQUE existante (mcp_*, forme JSON des erreurs), SANS lire
# l'implementation a venir. Ce que les tests fixent :
#   - l'appel rend TOUT DE SUITE un `job_id` (ok TRUE), sans attendre le travail ;
#   - `etat_calcul` rend `statut` en_cours / termine / echec, `job_id`, et, en
#     echec, une erreur MCP classee {ok, classe, code, langue, erreur, action,
#     candidats?} (mission 003) dont le `code` appartient a .MCP_ERREURS ;
#   - croiser_onf : 1er appel (apercu) synchrone et inchange ; 2e appel : jeton
#     consomme puis `job_id` immediat ; jeton invalide / rejoue : `jeton_invalide`
#     et AUCUN job ; projet introuvable / ambigu : erreur SANS consommer le jeton.
#
# Choix : ces tests ECHOUENT aujourd'hui (pas de skip) : tant que la mission
# n'est pas faite, l'appel est synchrone et ne rend pas de `job_id` ; un skip
# cacherait ce rouge.
#
# LIMITES (dites) :
#  - `etat_calcul` existe avec le parametre `projet` (pas `job_id`) : les tests
#    suivent le job par `etat_calcul(projet)` et verifient que `job_id` rendu
#    est celui de l'appel. Si le dev ajoute un parametre `job_id`, les tests
#    restent valables.
#  - Le travail detache tourne probablement dans un Rscript separe, que
#    `local_mocked_bindings` n'atteint pas. Le RETOUR RAPIDE n'en depend pas
#    (la fonction lente simulee ne bloque que si l'implementation reste
#    synchrone) ; le reseau est coupe pour l'enfant par un proxy mort herite de
#    l'environnement ; les deux tests qui exigent que le travail SIMULE ait
#    tourne (termine, ecriture) sautent, avec motif, si le marqueur du mock
#    n'a pas ete vu. Leur bout en bout releve alors de l'essai terrain.
#  - Pas de mesure de l'apercu reel de croiser_onf ici (reseau, 376 Mo).

.dt_sleep <- 10   # duree du travail simule lent (s)
.dt_borne <- 5    # un appel detache doit rendre en moins de (s) ; spec : < 60

.dt_lit <- function(json) jsonlite::fromJSON(as.character(json), simplifyVector = TRUE)

.dt_sq <- function(x) {
  sf::st_polygon(list(rbind(c(x, 0), c(x + 100, 0), c(x + 100, 100),
                            c(x, 100), c(x, 0))))
}
.dt_env <- function(env = parent.frame()) {
  root <- withr::local_tempdir(.local_envir = env)
  withr::local_options(nemeton.app_options = list(project_dir = root),
                       .local_envir = env)
  # Jetons dans un repertoire temporaire (016), jamais le vrai R_user_dir.
  withr::local_envvar(
    NEMETON_MCP_JETONS_DIR = withr::local_tempdir(.local_envir = env),
    # Aucun reseau pour un processus enfant eventuel : proxy mort herite.
    https_proxy = "http://127.0.0.1:9", http_proxy = "http://127.0.0.1:9",
    HTTPS_PROXY = "http://127.0.0.1:9", HTTP_PROXY = "http://127.0.0.1:9",
    .local_envir = env)
  local_mocked_bindings(
    add_r5_to_indicators = function(base_sf, project) base_sf,
    add_regen_r_indicators = function(base_sf, project) base_sf,
    is_db_configured = function() FALSE,
    lock_status = function(pid) NULL,
    .env = env)
  root
}
.dt_projet <- function(nom = "Forêt de Dabo", calcule = TRUE) {
  p <- sf::st_sf(
    id = c("21001000AA0001", "21001000AA0002"), section = "AA",
    numero = c("0001", "0002"), contenance = c(1e4, 1e4),
    commune = "Test", code_insee = "21001",
    geometry = sf::st_sfc(.dt_sq(800000), .dt_sq(800100), crs = 2154))
  id <- suppressMessages(projet_creer(nom, p))
  if (calcule) {
    ugs <- load_ug_data(id)$ugs
    suppressMessages(save_indicators(id, data.frame(
      ug_id = ugs$ug_id, C1 = 50, B1 = 60)))
  }
  id
}
.dt_etat_mock <- function() {
  e <- new.env()
  e$vu <- 0L
  e$lent <- FALSE
  e
}

# Coeur ONF simule (AA0001 retenue, AA0002 ecartee). `etat$lent` : quand TRUE,
# le croisement dort `.dt_sleep` s (le travail lourd simule) ; `etat$vu` compte
# les passages (marqueur : le travail simule a tourne DANS ce processus).
.dt_mock_onf <- function(etat, env = parent.frame()) {
  local_mocked_bindings(
    .onf_construire = function(cad, onf, selection, cfg) {
      etat$vu <- etat$vu + 1L
      if (isTRUE(etat$lent)) Sys.sleep(.dt_sleep)
      x <- sf::st_sf(idu = "21001000AA0001", ugf_id = "F1-1",
                     nom_ugf = "Forêt test - parcelle 1", foret_id = "F1",
                     foret_nom = "Forêt test", parcelle = "1",
                     domaniale = FALSE, part_onf = 0.9, surface_m2 = 1e4,
                     geometry = sf::st_sfc(.dt_sq(800000), crs = 2154))
      attr(x, "parcelles") <- data.frame(
        idu = c("21001000AA0001", "21001000AA0002"), retenue = c(TRUE, FALSE),
        raison = c(NA, "hors ONF"), proprietaire = c("COMMUNE", "X"),
        couverture_onf = c(1, 0))
      x
    },
    onf_load_parcelles = function(...) list(status = "ok", parcelles = NULL),
    .env = env)
}

# Appel mesure : rend list(r = JSON lu, dt = secondes).
.dt_chrono <- function(expr) {
  t0 <- Sys.time()
  r <- .dt_lit(expr)
  list(r = r, dt = as.numeric(difftime(Sys.time(), t0, units = "secs")))
}

# Etat du job, par l'interface publique existante (`etat_calcul(projet)`).
.dt_etat <- function(id) .dt_lit(mcp_etat_calcul(id))

# Attend la fin du job (statut hors en_cours) ; rend le dernier etat. Chaque
# etat observe doit etre l'un des trois statuts de la fiche.
.dt_attendre <- function(id, max = 150) {
  t0 <- Sys.time()
  repeat {
    e <- .dt_etat(id)
    expect_true(e$statut %in% c("en_cours", "termine", "echec"),
                info = paste("statut hors fiche :", e$statut))
    if (!identical(e$statut, "en_cours")) return(e)
    if (as.numeric(difftime(Sys.time(), t0, units = "secs")) > max) return(e)
    Sys.sleep(0.5)
  }
}

# Erreur MCP classee (mission 003) : soit au premier niveau, soit dans le
# champ `erreur` d'un etat `echec`. Rend la liste {ok, classe, code, ...}.
.dt_erreur_de <- function(e) {
  if (is.list(e$erreur) && !is.null(e$erreur$classe)) return(e$erreur)
  e
}
.dt_attend_classee <- function(x, info = "") {
  for (champ in c("ok", "classe", "code", "langue", "erreur", "action")) {
    expect_true(champ %in% names(x), info = paste(info, ": champ absent :", champ))
  }
  expect_false(isTRUE(x$ok), info = info)
  expect_true(x$code %in% names(.MCP_ERREURS),
              info = paste(info, ": code hors referentiel :", paste(x$code, collapse = "")))
  expect_identical(x$classe, paste0("nemetonshiny_", x$code), info = info)
  expect_true(x$langue %in% c("fr", "en"), info = info)
  expect_true(is.character(x$action) && any(nzchar(x$action)), info = info)
  expect_true(is.character(x$erreur) && any(nzchar(x$erreur)), info = info)
}
.dt_job_id <- function(r, info = "") {
  expect_true(isTRUE(r$ok), info = paste(info, ": ok doit etre TRUE"))
  expect_true(is.character(r$job_id) && length(r$job_id) == 1L && nzchar(r$job_id),
              info = paste(info, ": `job_id` (chaine non vide) absent"))
  r$job_id
}

# --------------------------------------------------------------- generer_rapport

test_that("generer_rapport rend un job_id tout de suite, sans attendre le travail", {
  skip_if_not_installed("arrow")
  .dt_env()
  id <- .dt_projet()
  local_mocked_bindings(projet_rapport = function(...) {
    Sys.sleep(.dt_sleep)
    "inutile.pdf"
  })
  m <- .dt_chrono(mcp_generer_rapport(id))
  jid <- .dt_job_id(m$r, "generer_rapport")
  expect_lt(m$dt, .dt_borne)
  # Le job est suivi par etat_calcul avec le MEME job_id.
  e <- .dt_etat(id)
  expect_true(e$statut %in% c("en_cours", "termine", "echec"))
  expect_identical(e$job_id, jid)
  .dt_attendre(id)   # ne laisse pas d'enfant derriere le test
})

test_that("generer_rapport : fin de job = termine ou echec, jamais un statut inconnu", {
  skip_if_not_installed("arrow")
  .dt_env()
  id <- .dt_projet()
  etat <- .dt_etat_mock()
  local_mocked_bindings(projet_rapport = function(id, f, ...) {
    etat$vu <- etat$vu + 1L
    dir.create(dirname(f), showWarnings = FALSE, recursive = TRUE)
    writeLines("%PDF-simule", f)
    f
  })
  jid <- .dt_job_id(.dt_lit(mcp_generer_rapport(id)), "generer_rapport")
  e <- .dt_attendre(id)
  expect_identical(e$job_id, jid)
  expect_true(e$statut %in% c("termine", "echec"),
              info = "le job doit finir dans le delai du test")
  if (identical(e$statut, "echec")) {
    .dt_attend_classee(.dt_erreur_de(e), "etat echec")
  }
  # Le mock n'est vu que si le travail tourne dans CE processus.
  if (etat$vu == 0L) {
    skip("travail detache hors processus : le rapport simule n'a pas tourne ; essai terrain (Couchey)")
  }
  expect_identical(e$statut, "termine")
})

test_that("generer_rapport : l'erreur du job sort dans etat_calcul, erreur MCP classee", {
  skip_if_not_installed("arrow")
  .dt_env()
  id <- .dt_projet()
  local_mocked_bindings(projet_rapport = function(...) stop("rendu PDF impossible"))
  jid <- .dt_job_id(.dt_lit(mcp_generer_rapport(id)), "generer_rapport")
  e <- .dt_attendre(id)
  expect_identical(e$job_id, jid)
  if (identical(e$statut, "termine")) {
    skip("travail detache hors processus : le rapport reel a reussi, l'echec simule n'a pas eu lieu")
  }
  expect_identical(e$statut, "echec")
  .dt_attend_classee(.dt_erreur_de(e), "etat echec generer_rapport")
})

test_that("generer_rapport : projet introuvable ou ambigu = erreur classee, aucun job", {
  skip_if_not_installed("arrow")
  .dt_env()
  .dt_projet("Dabo nord")
  .dt_projet("Dabo sud")
  r <- .dt_lit(mcp_generer_rapport("Mouthe"))
  .dt_attend_classee(r, "introuvable")
  expect_identical(r$code, "projet_introuvable")
  expect_null(r$job_id)
  r2 <- .dt_lit(mcp_generer_rapport("dabo"))
  .dt_attend_classee(r2, "ambigu")
  expect_identical(r2$code, "projet_ambigu")
  expect_length(r2$candidats, 2L)
  expect_null(r2$job_id)
})

# ------------------------------------------------------------------ croiser_onf

test_that("croiser_onf, 1er appel : apercu synchrone inchange, jeton, pas de job", {
  skip_if_not_installed("arrow")
  .dt_env()
  id <- .dt_projet()
  etat <- .dt_etat_mock()
  .dt_mock_onf(etat)
  r <- .dt_lit(mcp_croiser_onf(id))
  expect_false(isTRUE(r$ok))
  expect_identical(r$code, "confirmation_requise")
  expect_identical(r$classe, "nemetonshiny_confirmation_requise")
  for (champ in c("jeton", "apercu", "expire_a")) {
    expect_true(champ %in% names(r), info = champ)
  }
  expect_null(r$job_id)
  expect_true(all(c("ecartees", "n_ecartees", "n_retenues") %in% names(r$apercu)))
  expect_identical(as.integer(r$apercu$n_ecartees), 1L)
  expect_identical(as.integer(r$apercu$n_retenues), 1L)
  expect_identical(.dt_etat(id)$statut, "aucun")  # aucun job lance par l'apercu
})

test_that("croiser_onf, 2e appel : jeton consomme puis job_id immediat", {
  skip_if_not_installed("arrow")
  .dt_env()
  id <- .dt_projet()
  etat <- .dt_etat_mock()
  .dt_mock_onf(etat)
  jeton <- .dt_lit(mcp_croiser_onf(id))$jeton
  expect_true(is.character(jeton) && nzchar(jeton))
  etat$lent <- TRUE                      # le croisement devient lent
  m <- .dt_chrono(mcp_croiser_onf(id, jeton = jeton))
  jid <- .dt_job_id(m$r, "croiser_onf")
  expect_lt(m$dt, .dt_borne)
  expect_null(m$r$n_ugf)                 # le resultat vient de etat_calcul, pas de l'appel
  e <- .dt_etat(id)
  expect_true(e$statut %in% c("en_cours", "termine", "echec"))
  expect_identical(e$job_id, jid)

  # Jeton consomme : le rejouer est refuse, et ne lance aucun second job.
  r2 <- .dt_lit(mcp_croiser_onf(id, jeton = jeton))
  expect_identical(r2$code, "jeton_invalide")
  expect_identical(r2$classe, "nemetonshiny_jeton_invalide")
  expect_null(r2$job_id)
  expect_identical(.dt_etat(id)$job_id, jid)
  .dt_attendre(id)
})

test_that("croiser_onf : jeton inconnu = jeton_invalide, aucun job, rien de croise", {
  skip_if_not_installed("arrow")
  .dt_env()
  id <- .dt_projet()
  etat <- .dt_etat_mock()
  .dt_mock_onf(etat)
  .dt_lit(mcp_croiser_onf(id))              # apercu : un passage du mock
  vu0 <- etat$vu
  r <- .dt_lit(mcp_croiser_onf(id, jeton = "jeton-qui-n-existe-pas"))
  .dt_attend_classee(r, "jeton inconnu")
  expect_identical(r$code, "jeton_invalide")
  expect_null(r$job_id)
  expect_identical(etat$vu, vu0)            # le croisement n'a pas ete lance
  expect_identical(.dt_etat(id)$statut, "aucun")
  expect_identical(as.character(load_parcels(id)$id),
                   c("21001000AA0001", "21001000AA0002"))
})

test_that("croiser_onf : jeton perime = jeton_invalide, aucun job", {
  skip_if_not_installed("arrow")
  .dt_env()
  id <- .dt_projet()
  etat <- .dt_etat_mock()
  .dt_mock_onf(etat)
  jeton <- .dt_lit(mcp_croiser_onf(id))$jeton
  # Horloge avancee de 11 min (duree du jeton : 10 min, provisoire).
  reel <- base::Sys.time
  local_mocked_bindings(Sys.time = function() reel() + 11 * 60, .package = "base")
  r <- .dt_lit(mcp_croiser_onf(id, jeton = jeton))
  expect_identical(r$code, "jeton_invalide")
  expect_null(r$job_id)
})

test_that("croiser_onf : projet introuvable ou ambigu = erreur SANS consommer le jeton", {
  skip_if_not_installed("arrow")
  .dt_env()
  nord <- .dt_projet("Dabo nord")
  .dt_projet("Dabo sud")
  etat <- .dt_etat_mock()
  .dt_mock_onf(etat)
  jeton <- .dt_lit(mcp_croiser_onf(nord))$jeton
  expect_true(is.character(jeton) && nzchar(jeton))

  r1 <- .dt_lit(mcp_croiser_onf("Mouthe", jeton = jeton))
  .dt_attend_classee(r1, "introuvable")
  expect_identical(r1$code, "projet_introuvable")
  expect_null(r1$job_id)

  r2 <- .dt_lit(mcp_croiser_onf("dabo", jeton = jeton))
  .dt_attend_classee(r2, "ambigu")
  expect_identical(r2$code, "projet_ambigu")
  expect_length(r2$candidats, 2L)
  expect_null(r2$job_id)
  expect_identical(.dt_etat(nord)$statut, "aucun")

  # Le jeton n'a pas ete consomme : le bon projet l'accepte et rend un job_id.
  r3 <- .dt_lit(mcp_croiser_onf(nord, jeton = jeton))
  .dt_job_id(r3, "jeton intact apres erreur de projet")
  .dt_attendre(nord)
})

test_that("croiser_onf : l'echec du croisement sort dans etat_calcul, erreur classee", {
  skip_if_not_installed("arrow")
  .dt_env()
  id <- .dt_projet()
  etat <- .dt_etat_mock()
  .dt_mock_onf(etat)
  jeton <- .dt_lit(mcp_croiser_onf(id))$jeton
  # Source ONF/DGFiP injoignable : en processus separe (reseau coupe par le
  # proxy mort) comme ici, le croisement echoue avec la meme classe.
  local_mocked_bindings(
    onf_load_parcelles = function(...) list(status = "unavailable", parcelles = NULL))
  jid <- .dt_job_id(.dt_lit(mcp_croiser_onf(id, jeton = jeton)), "croiser_onf")
  e <- .dt_attendre(id)
  expect_identical(e$job_id, jid)
  expect_identical(e$statut, "echec")
  x <- .dt_erreur_de(e)
  .dt_attend_classee(x, "etat echec croiser_onf")
  expect_identical(x$code, "onf_echec")
  expect_identical(x$classe, "nemetonshiny_onf_echec")
  # Echec : le projet n'a pas ete modifie.
  expect_identical(as.character(load_parcels(id)$id),
                   c("21001000AA0001", "21001000AA0002"))
})

test_that("croiser_onf : le job termine ecrit le projet, etat_calcul le dit", {
  skip_if_not_installed("arrow")
  .dt_env()
  id <- .dt_projet()
  etat <- .dt_etat_mock()
  .dt_mock_onf(etat)
  jeton <- .dt_lit(mcp_croiser_onf(id))$jeton
  vu0 <- etat$vu
  jid <- .dt_job_id(.dt_lit(mcp_croiser_onf(id, jeton = jeton)), "croiser_onf")
  e <- .dt_attendre(id)
  expect_identical(e$job_id, jid)
  expect_true(e$statut %in% c("termine", "echec"))
  if (etat$vu == vu0) {
    skip("travail detache hors processus : le croisement simule n'a pas tourne ; essai terrain (Couchey)")
  }
  expect_identical(e$statut, "termine")
  # purger resolu TRUE (reglage du projet) : AA0002 a quitte le projet.
  expect_identical(as.character(load_parcels(id)$id), "21001000AA0001")
})

# ------------------------------------------------------------------ declaration

test_that("les descriptions des deux outils renvoient a etat_calcul ; l'existant reste", {
  skip_if_not_installed("ellmer")
  outils <- mcp_tools()
  noms <- vapply(outils, function(t) t@name, character(1))
  for (n in c("lancer_calcul", "etat_calcul", "generer_rapport", "croiser_onf")) {
    expect_true(n %in% noms, info = n)
  }
  for (n in c("generer_rapport", "croiser_onf")) {
    d <- outils[[match(n, noms)]]@description
    expect_match(d, "etat_calcul", fixed = TRUE, info = n)
  }
  # Le jeton reste un argument de croiser_onf (contrat 010/016 inchange).
  expect_true("jeton" %in% names(outils[[match("croiser_onf", noms)]]@arguments@properties))
})

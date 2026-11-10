# Mission 002 - tests d'acceptation du critere 3 (spec pilotage, P6) :
# chaque outil mis en echec rend une erreur MCP marquee `isError`, classee,
# dans la langue demandee, avec l'action suivante proposee.
#
# Ecrits a partir de la spec, de la fiche (arbitrages de Pascal, 2026-11-10)
# et du comportement de mcptools 1.0.3 (`as_tool_call_result` pose `isError`
# seulement si le resultat est un `ellmer::ContentToolResult` dont `@error`
# est non nul), sans lire l'implementation. Rouges tant que la mission 003
# n'est pas fusionnee.
#
# Forme fixee par Pascal : JSON dans `ContentToolResult(error = ...)`, texte
# recu par le client "Tool calling failed with error <json>" ; champs
# `classe` (nom R complet, ex. nemetonshiny_projet_introuvable), `code`
# (court, ex. projet_introuvable). Forme JSON fixee par le plan 003 accepte
# par Pascal : {ok, classe, code, langue, erreur, action, candidats}
# (le message s'appelle `erreur`).
#
# Les outils sont appeles par les objets de `mcp_tools()` puis passes a
# `mcptools:::as_tool_call_result()` : c'est ce que voit le client MCP.

.er_sq <- function(x) {
  sf::st_polygon(list(rbind(c(x, 0), c(x + 100, 0), c(x + 100, 100),
                            c(x, 100), c(x, 0))))
}
.er_env <- function(env = parent.frame()) {
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
.er_projet <- function(nom) {
  parcelles <- sf::st_sf(
    id = c("21001000AA0001", "21001000AA0002"), section = "AA",
    numero = c("0001", "0002"), contenance = c(1e4, 1e4),
    commune = "Test", code_insee = "21001",
    geometry = sf::st_sfc(.er_sq(800000), .er_sq(800100), crs = 2154))
  suppressMessages(projet_creer(nom, parcelles))
}

# Appelle l'outil `nom` comme le fait mcptools et rend ce que verra le client.
.er_appel <- function(nom, ...) {
  skip_if_not_installed("mcptools")
  skip_if_not_installed("ellmer")
  skip_if_not_installed("jsonlite")
  outils <- mcp_tools()
  noms <- vapply(outils, function(t) t@name, character(1))
  expect_true(nom %in% noms, info = paste("outil absent :", nom))
  res <- outils[[match(nom, noms)]](...)
  rep <- getFromNamespace("as_tool_call_result", "mcptools")(list(id = 1L), res)
  texte <- paste(vapply(rep$result$content, function(x) x$text %||% "",
                        character(1)), collapse = "\n")
  list(is_error = isTRUE(rep$result$isError), texte = texte)
}

.er_prefixe <- "Tool calling failed with error "

# Retire le prefixe du client puis parse le JSON ; NULL si l'un des deux manque.
# Le tryCatch ne contient aucune expectation : il ne fait que parser.
.er_json <- function(texte) {
  if (!startsWith(texte, .er_prefixe)) return(NULL)
  brut <- substring(texte, nchar(.er_prefixe) + 1L)
  tryCatch(jsonlite::fromJSON(brut, simplifyVector = TRUE),
           error = function(e) NULL)
}

# Assertion exacte : isError, prefixe, JSON valide, `code` ET `classe`
# identiques (pas de correspondance par sous-chaine : "projet_introuvable"
# est contenu dans "nemetonshiny_projet_introuvable").
.er_attend <- function(r, code, info = code) {
  expect_true(r$is_error, info = info)
  expect_true(startsWith(r$texte, .er_prefixe), info = info)
  j <- .er_json(r$texte)
  expect_false(is.null(j), info = paste(info, ": JSON illisible"))
  expect_identical(j$code, code, info = info)
  expect_identical(j$classe, paste0("nemetonshiny_", code), info = info)
  j
}

.er_action <- function(j) paste(j$action, collapse = " ")

# --------------------------------------------------------------- nominal

test_that("un appel reussi n'est pas marque en erreur (nominal)", {
  .er_env()
  id <- .er_projet("Foret de Dabo")
  r <- .er_appel("resume_projet", projet = id)
  expect_false(r$is_error)
  r <- .er_appel("lister_projets")
  expect_false(r$is_error)
})

# --------------------------------------------------------- projet_introuvable

test_that("projet_introuvable : isError, JSON classe, action = lister_projets", {
  .er_env()
  .er_projet("Couchey")
  r <- .er_appel("resume_projet", projet = "Mouthe")
  j <- .er_attend(r, "projet_introuvable")
  # action suivante : retrouver le bon nom dans la liste des projets
  expect_match(.er_action(j), "lister_projets", fixed = TRUE)
})

test_that("chaque outil a projet, mis en echec par un projet inconnu, rend une erreur classee", {
  .er_env()
  .er_projet("Couchey")
  args <- list(
    resume_projet   = list(projet = "Mouthe"),
    lancer_calcul   = list(projet = "Mouthe"),
    etat_calcul     = list(projet = "Mouthe"),
    annuler_calcul  = list(projet = "Mouthe"),
    generer_rapport = list(projet = "Mouthe"),
    exporter_gpkg   = list(projet = "Mouthe"),
    appliquer_ugf   = list(projet = "Mouthe", fichier = "absent.gpkg"),
    croiser_onf     = list(projet = "Mouthe"),
    url_app         = list(projet = "Mouthe")
  )
  expect_length(args, 9L)  # les 10 outils moins lister_projets (sans projet)
  for (nom in names(args)) {
    r <- do.call(.er_appel, c(list(nom), args[[nom]]))
    .er_attend(r, "projet_introuvable", info = nom)
  }
})

# ------------------------------------------------------------ projet_ambigu

test_that("projet_ambigu : isError, JSON classe, et les deux candidats sont listes", {
  .er_env()
  .er_projet("Dabo nord")
  .er_projet("Dabo sud")
  r <- .er_appel("resume_projet", projet = "dabo")
  j <- .er_attend(r, "projet_ambigu")
  expect_match(r$texte, "Dabo nord", fixed = TRUE)
  expect_match(r$texte, "Dabo sud", fixed = TRUE)
  # et dans le champ `candidats` du JSON, pas seulement dans le texte brut
  cand <- paste(unlist(j$candidats), collapse = " ")
  expect_match(cand, "Dabo nord", fixed = TRUE)
  expect_match(cand, "Dabo sud", fixed = TRUE)
})

# --------------------------------------------------------- projet_verrouille

test_that("projet_verrouille : isError, JSON classe, detenteur nomme, action non vide", {
  skip_if_not_installed("arrow")
  .er_env()
  .er_projet("Couchey")
  local_mocked_bindings(
    onf_croise_tache = function(...) stop("ne doit pas etre lance"),
    lock_status = function(pid) list(holder_label = "Pascal", stale = FALSE))
  r <- .er_appel("croiser_onf", projet = "Couchey")
  j <- .er_attend(r, "projet_verrouille")
  expect_match(r$texte, "Pascal", fixed = TRUE)
  # action proposee : attendre la fin de l'edition ou prevenir le detenteur
  expect_true(is.character(j$action) && length(j$action) >= 1L &&
                all(nzchar(j$action)), info = "champ action non vide")
})

# ------------------------------------------------------------- calcul_en_cours

test_that("calcul_en_cours : le second lancement est une erreur classee qui renvoie a etat_calcul", {
  .er_env()
  id <- .er_projet("Couchey")
  local_mocked_bindings(.mcp_spawn = function(...) invisible(TRUE))
  r1 <- .er_appel("lancer_calcul", projet = id)
  expect_false(r1$is_error)
  r2 <- .er_appel("lancer_calcul", projet = id)
  j <- .er_attend(r2, "calcul_en_cours")
  expect_match(.er_action(j), "etat_calcul", fixed = TRUE)
})

# ------------------------------------------------------------------- FR / EN

test_that("la langue : FR par defaut, EN sur demande, chaque message propre a sa langue", {
  .er_env()
  .er_projet("Couchey")
  defaut <- .er_attend(.er_appel("resume_projet", projet = "Mouthe"),
                       "projet_introuvable")
  fr <- .er_attend(.er_appel("resume_projet", projet = "Mouthe", langue = "fr"),
                   "projet_introuvable")
  en <- .er_attend(.er_appel("resume_projet", projet = "Mouthe", langue = "en"),
                   "projet_introuvable")
  expect_identical(defaut$langue, "fr")
  expect_identical(fr$langue, "fr")
  expect_identical(en$langue, "en")
  for (m in list(fr$erreur, en$erreur))
    expect_true(is.character(m) && length(m) == 1L && nzchar(m))
  mf <- as.character(fr$erreur %||% "")[1]
  me <- as.character(en$erreur %||% "")[1]
  expect_identical(as.character(defaut$erreur %||% "")[1], mf)
  # chaque texte porte le message de sa langue, et seulement le sien
  # (un message vide, pattern "", compte comme un echec ici)
  expect_false(grepl(mf, me, fixed = TRUE))
  expect_false(grepl(me, mf, fixed = TRUE))
  # (comparer a get_i18n(<langue>)$t(<cle>) attend que les cles existent : 003)
})

test_that("generer_rapport accepte la langue pour son erreur", {
  .er_env()
  .er_projet("Couchey")
  en <- .er_attend(.er_appel("generer_rapport", projet = "Mouthe", langue = "en"),
                   "projet_introuvable")
  expect_identical(en$langue, "en")
})

# ----------------------------------------------- classes qui attendent la mission 010

test_that("confirmation_requise : un outil qui ecrit, appele sans jeton, n'ecrit rien (attend 010)", {
  skip("attend la mission 010 : confirmation_requise et jeton_invalide restent en skip (arbitrage de Pascal, 2026-11-10)")
  .er_env()
  .er_projet("Couchey")
  r <- .er_appel("appliquer_ugf", projet = "Couchey", fichier = "absent.gpkg")
  .er_attend(r, "confirmation_requise")
})

test_that("jeton_invalide : un jeton inconnu, perime ou reutilise est refuse (attend 010)", {
  skip("attend la mission 010 : parametre `jeton` suppose, a confirmer par l'ADR 0002")
  .er_env()
  .er_projet("Couchey")
  r <- .er_appel("appliquer_ugf", projet = "Couchey", fichier = "absent.gpkg",
                 jeton = "bidon")
  .er_attend(r, "jeton_invalide")
})

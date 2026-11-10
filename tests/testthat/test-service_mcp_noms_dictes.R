# Mission 006 - critere 1 de pilotage/spec.md : noms dictes (P1).
# Ecrit sans lire l'implementation. Fixture : fixtures/noms_dictes.csv (mission 005).
#
# Regle de rapprochement (arbitrages de Pascal, 2026-11-10) :
# - MOT-CLE tire du NOM du projet : "Foret communale de X (ONF)" -> X ; sinon
#   le nom lui-meme (Dabo, Reconfort, Aumur, Fordead, ForetAccess).
# - Un projet correspond au dicte si la distance (apres normalisation) entre le
#   dicte et le mot-cle est <= 2, OU si le dicte est une suite de MOTS ENTIERS
#   en tete du mot-cle ("marsannay" et "marsannay la" sont des debuts de
#   "marsannay la cote" ; "mars" n'en est pas un). Le prefixe "Foret communale
#   de" est tolere devant le dicte. Le dicte n'a PAS a etre contenu dans le nom
#   complet : "de", "onf", "foret", "communale" ne rapprochent aucun projet.
# - Au-dela -> introuvable. Deux projets qui correspondent -> erreur
#   projet_ambigu avec les deux en candidats. Un seul -> ce projet.
# - Forme des candidats livree par 003 : "nom (id)" ; on cherche le nom ou l'id
#   a l'interieur de chaque candidat (sous-chaine fixe), pas d'egalite stricte.

.nd_noms <- c("Dabo", "Forêt communale de Couchey (ONF)",
              "Forêt communale de Sombernon",
              "Forêt communale de Marsannay-la-côte (ONF)",
              "Reconfort", "Aumur", "Fordead", "ForetAccess")

.nd_norm <- function(x) {
  # chartr plutot qu'iconv(TRANSLIT) (fragile selon la locale). Suppose
  # toutefois une locale UTF-8 : sous LC_ALL=C, chartr echoue sur les accents.
  x <- chartr("àâäéèêëîïôöùûüç", "aaaeeeeiioouuuc", tolower(x))
  trimws(gsub("[^a-z0-9]+", " ", x))
}
.nd_motcle <- function(nom) {
  m <- sub("^Forêt communale de ", "", nom)
  m <- sub(" \\(ONF\\)$", "", m)
  .nd_norm(m)
}
# Regle de rapprochement : le projet `nom` correspond-il au dicte ?
.nd_rapproche <- function(dit, nom) {
  mc <- .nd_motcle(nom)
  d <- .nd_norm(dit)
  variantes <- unique(c(d, trimws(sub("^foret communale de ", "", d))))
  any(vapply(variantes, function(v) {
    nzchar(v) && (drop(utils::adist(v, mc)) <= 2 ||
                    identical(mc, v) || startsWith(mc, paste0(v, " ")))
  }, logical(1)))
}
# Un candidat "nom (id)" designe-t-il ce projet ? (sous-chaine fixe)
.nd_cand_est <- function(cand, nom, id) {
  grepl(id, cand, fixed = TRUE) || grepl(nom, cand, fixed = TRUE)
}
# Noms des projets de `monde` (nom -> id) que designe un vecteur de candidats.
.nd_cand_noms <- function(cands, monde) {
  noms <- names(monde)
  noms[vapply(noms, function(n)
    any(vapply(cands, function(cd) .nd_cand_est(cd, n, monde[[n]]), logical(1))),
    logical(1))]
}
# Mots contenus dans les noms complets mais qui ne designent aucun projet.
.nd_negatifs <- c("de", "onf", "foret", "communale", "mars")
.nd_fixture <- function() {
  f <- testthat::test_path("fixtures", "noms_dictes.csv")
  read.csv(f, stringsAsFactors = FALSE, encoding = "UTF-8", fileEncoding = "UTF-8")
}

.nd_sq <- function(x) {
  sf::st_polygon(list(rbind(c(x, 0), c(x + 100, 0), c(x + 100, 100),
                            c(x, 100), c(x, 0))))
}
.nd_parcelles <- function() {
  sf::st_sf(id = c("21001000AA0001", "21001000AA0002"), section = "AA",
            numero = c("0001", "0002"), contenance = c(1e4, 1e4),
            commune = "Test", code_insee = "21001",
            geometry = sf::st_sfc(.nd_sq(800000), .nd_sq(800100), crs = 2154))
}
.nd_env <- function(env = parent.frame()) {
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
  invisible(root)
}
# Projets portant les noms donnes, non calcules ; rend le vecteur nom -> id.
.nd_monde <- function(noms = .nd_noms, env = parent.frame()) {
  .nd_env(env)
  ids <- vapply(noms, function(n)
    suppressMessages(projet_creer(n, .nd_parcelles())), character(1))
  stats::setNames(ids, noms)
}

# Issue d'une resolution : statut = projet | candidats | introuvable | autre.
.nd_resoudre <- function(dit) {
  res <- tryCatch(list(statut = "projet", id = .mcp_resolve_project(dit)),
                  error = function(e) list(statut = "erreur", classes = class(e)))
  if (res$statut == "projet") return(res)
  if ("nemetonshiny_projet_introuvable" %in% res$classes) return(list(statut = "introuvable"))
  if ("nemetonshiny_projet_ambigu" %in% res$classes) {
    j <- jsonlite::fromJSON(mcp_resume_projet(dit), simplifyVector = TRUE)
    return(list(statut = "candidats",
                candidats = as.character(unlist(j$candidats, use.names = FALSE))))
  }
  list(statut = "autre", classes = res$classes)
}

test_that("la fixture 005 est lisible : 33 lignes, 3 colonnes, attendu connu", {
  fx <- .nd_fixture()
  expect_identical(names(fx), c("dit", "attendu", "source"))
  expect_identical(nrow(fx), 33L)
  expect_true(all(fx$attendu %in% c(.nd_noms, "introuvable")))
  expect_identical(sum(fx$attendu == "introuvable"), 4L)
  expect_false(anyNA(fx))
})

test_that("la regle de rapprochement du test traite la fixture comme Pascal l'a decide", {
  fx <- .nd_fixture()
  # Chaque ligne attendue sur un projet est rapprochee de CE projet.
  for (i in which(fx$attendu != "introuvable")) {
    expect_true(.nd_rapproche(fx$dit[i], fx$attendu[i]),
                label = sprintf("'%s' rapproche de '%s'", fx$dit[i], fx$attendu[i]))
  }
  # Chaque controle n'est rapproche d'aucun projet (distance > 2, ni debut, ni mot entier).
  for (dit in fx$dit[fx$attendu == "introuvable"]) {
    for (n in .nd_noms) {
      expect_false(.nd_rapproche(dit, n), label = sprintf("'%s' rapproche de '%s'", dit, n))
    }
  }
  # Marsannay est un debut (mots entiers) du mot-cle, bien qu'a 8 par la distance seule.
  expect_gt(drop(utils::adist("marsannay", "marsannay la cote")), 2)
  marsannay <- "Forêt communale de Marsannay-la-côte (ONF)"
  expect_true(.nd_rapproche("Marsannay", marsannay))
  expect_true(.nd_rapproche("Marsannay la", marsannay))
  # "mars" n'est pas un mot entier du mot-cle : pas un debut.
  expect_false(.nd_rapproche("mars", marsannay))
  # Controles negatifs : mots contenus dans les noms complets, ils ne rapprochent rien.
  for (mot in .nd_negatifs) for (n in .nd_noms) {
    expect_false(.nd_rapproche(mot, n), label = sprintf("'%s' rapproche de '%s'", mot, n))
  }
  # Mouthe est a 3 de "couchey" : le seuil 2 le laisse introuvable.
  expect_identical(drop(utils::adist("mouthe", "couchey")), 3)
  # Normalisation independante de la locale : cote -> cote.
  expect_identical(.nd_norm("Marsannay-la-côte"), "marsannay la cote")
})

test_that("critere 1 : chaque nom dicte donne le bon projet ou des candidats qui le contiennent, tous conformes a la regle", {
  skip_if_not_installed("arrow")
  ids <- .nd_monde()
  fx <- .nd_fixture()
  fx <- fx[fx$attendu != "introuvable", ]
  ko <- character(0)
  for (i in seq_len(nrow(fx))) {
    r <- .nd_resoudre(fx$dit[i])
    att <- fx$attendu[i]
    bon <- switch(r$statut,
      projet = identical(r$id, ids[[att]]),
      candidats = {
        noms_c <- .nd_cand_noms(r$candidats, ids)
        att %in% noms_c && length(noms_c) == length(r$candidats) &&
          all(vapply(noms_c, function(n) .nd_rapproche(fx$dit[i], n), logical(1)))
      },
      FALSE)
    if (!bon) ko <- c(ko, sprintf("'%s' -> %s (attendu %s)", fx$dit[i], r$statut, att))
  }
  expect_identical(ko, character(0))
})

test_that("critere 1 : aucun introuvable quand un mot-cle est a distance <= 2", {
  skip_if_not_installed("arrow")
  ids <- .nd_monde()
  motcles <- vapply(.nd_noms, .nd_motcle, character(1))
  proches <- c("couche", "couchay", "kouchet", "sombrenon", "dabbo", "aumurr",
               "fordeadd", "reconfrt", "foretacces", "dabol")
  ko <- character(0)
  for (dit in proches) {
    d <- min(drop(utils::adist(.nd_norm(dit), motcles)))
    stopifnot(d <= 2)
    r <- .nd_resoudre(dit)
    if (r$statut == "candidats") {
      noms_c <- .nd_cand_noms(r$candidats, ids)
      ok <- length(noms_c) == length(r$candidats) &&
        all(vapply(noms_c, function(n) .nd_rapproche(dit, n), logical(1)))
      if (!ok) ko <- c(ko, sprintf("'%s' -> candidats hors regle", dit))
    } else if (r$statut != "projet") {
      ko <- c(ko, sprintf("'%s' -> %s", dit, r$statut))
    } else if (!.nd_rapproche(dit, names(ids)[match(r$id, ids)])) {
      ko <- c(ko, sprintf("'%s' -> projet hors regle", dit))
    }
  }
  expect_identical(ko, character(0))
})

test_that("critere 1 : un nom sans projet proche reste introuvable, classe exacte", {
  skip_if_not_installed("arrow")
  .nd_monde()
  fx <- .nd_fixture()
  for (dit in c(fx$dit[fx$attendu == "introuvable"], .nd_negatifs)) {
    expect_error(.mcp_resolve_project(dit), class = "nemetonshiny_projet_introuvable")
    j <- jsonlite::fromJSON(mcp_resume_projet(dit), simplifyVector = TRUE)
    expect_false(j$ok)
    expect_identical(j$classe, "nemetonshiny_projet_introuvable")
  }
})

test_that("deux projets dont le mot-cle est a distance <= 2 : projet_ambigu, les deux candidats exactement", {
  skip_if_not_installed("arrow")
  # "couchet" est a 1 de "couchey" et a 1 de "couchay" : les deux correspondent.
  ids <- .nd_monde(c("Forêt communale de Couchey (ONF)", "Forêt communale de Couchay"))
  expect_true(all(vapply(names(ids), function(n) .nd_rapproche("Couchet", n), logical(1))))
  expect_error(.mcp_resolve_project("Couchet"), class = "nemetonshiny_projet_ambigu")
  r <- .nd_resoudre("Couchet")
  expect_identical(r$statut, "candidats")
  expect_length(r$candidats, 2L)
  expect_setequal(.nd_cand_noms(r$candidats, ids), names(ids))
})

test_that("deux projets dont l'un a le dicte pour mot entier de son mot-cle : projet_ambigu", {
  skip_if_not_installed("arrow")
  # "couchey" : exact pour le premier, mot entier (debut) de "couchey le haut" pour le second.
  ids <- .nd_monde(c("Forêt communale de Couchey (ONF)", "Forêt communale de Couchey-le-Haut"))
  expect_true(all(vapply(names(ids), function(n) .nd_rapproche("Couchey", n), logical(1))))
  expect_error(.mcp_resolve_project("Couchey"), class = "nemetonshiny_projet_ambigu")
  r <- .nd_resoudre("Couchey")
  expect_identical(r$statut, "candidats")
  expect_length(r$candidats, 2L)
  expect_setequal(.nd_cand_noms(r$candidats, ids), names(ids))
})

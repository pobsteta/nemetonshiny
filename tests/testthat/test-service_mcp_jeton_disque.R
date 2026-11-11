# Mission 016 : le registre des jetons survit au redemarrage du serveur MCP
# (VICTOR lance un `claude -p` neuf, donc un serveur neuf, par tache). Un
# fichier par jeton dans NEMETON_MCP_JETONS_DIR (ici un repertoire
# temporaire propre a chaque test, jamais le vrai R_user_dir).

.jd_dir <- function(env = parent.frame()) {
  d <- withr::local_tempdir(pattern = "jetons-016-", .local_envir = env)
  withr::local_envvar(NEMETON_MCP_JETONS_DIR = d, .local_envir = env)
  d
}
.jd_cond <- function(expr) tryCatch(expr, error = function(e) e)
.jd_emettre <- function(lien = list(a = 1)) {
  .jd_cond(.mcp_jeton_emettre("outil_x", "p1", lien, list(n = 1L)))$jeton
}
.jd_consommer <- function(j, lien = list(a = 1)) {
  .jd_cond(.mcp_jeton_consommer(j, "outil_x", "p1", lien, garde = NULL))
}

test_that("le repertoire vient de NEMETON_MCP_JETONS_DIR, cree au besoin", {
  d <- file.path(.jd_dir(), "sous", "jetons")
  withr::local_envvar(NEMETON_MCP_JETONS_DIR = d)
  expect_false(dir.exists(d))
  expect_identical(.mcp_jetons_dir(), d)
  expect_true(dir.exists(d))
})

test_that("defaut : R_user_dir('nemetonshiny', 'data')/mcp/jetons", {
  faux <- withr::local_tempdir()
  withr::local_envvar(NEMETON_MCP_JETONS_DIR = NA, R_USER_DATA_DIR = faux)
  attendu <- file.path(tools::R_user_dir("nemetonshiny", "data"), "mcp", "jetons")
  expect_true(startsWith(attendu, faux))
  expect_identical(.mcp_jetons_dir(), attendu)
})

test_that("un fichier par jeton, droits 0700 / 0600, contenu restreint", {
  d <- file.path(.jd_dir(), "neuf")
  withr::local_envvar(NEMETON_MCP_JETONS_DIR = d)
  j <- .jd_emettre()
  f <- file.path(d, paste0(j, ".rds"))
  expect_true(file.exists(f))
  expect_identical(list.files(d, all.files = TRUE, no.. = TRUE), basename(f))
  if (.Platform$OS.type == "unix") {
    expect_identical(format(file.mode(d)), "700")
    expect_identical(format(file.mode(f)), "600")
  }
  e <- readRDS(f)
  expect_setequal(names(e), c("jeton", "outil", "projet", "lien", "expire"))
  expect_identical(e$jeton, j)
  expect_identical(e$lien, list(a = 1))
  expect_true(is.numeric(e$expire))
})

test_that("consommer supprime le fichier ; rejouer -> jeton_invalide", {
  d <- .jd_dir()
  j <- .jd_emettre()
  expect_true(.mcp_jeton_consommer(j, "outil_x", "p1", list(a = 1), garde = NULL))
  expect_false(file.exists(file.path(d, paste0(j, ".rds"))))
  expect_s3_class(.jd_consommer(j), "nemetonshiny_jeton_invalide")
})

test_that("jeton perime sur disque : refuse et purge", {
  d <- .jd_dir()
  j <- .jd_emettre()
  autre <- .jd_emettre()
  reel <- base::Sys.time
  local_mocked_bindings(Sys.time = function() reel() + .MCP_JETON_DUREE + 1,
                        .package = "base")
  expect_s3_class(.jd_consommer(j), "nemetonshiny_jeton_invalide")
  # le perime presente ET l'autre perime sont purges a la consommation
  expect_length(list.files(d, pattern = "\\.rds$"), 0L)
  expect_false(file.exists(file.path(d, paste0(autre, ".rds"))))
})

test_that("fichier corrompu : jeton_invalide classe, purge a l'emission", {
  d <- .jd_dir()
  j <- .jd_emettre()
  f <- file.path(d, paste0(j, ".rds"))
  writeBin(as.raw(c(0x1f, 0x8b, 0x00, 0x42)), f)
  e <- .jd_consommer(j)
  expect_s3_class(e, "nemetonshiny_jeton_invalide")
  expect_false(file.exists(f))
  # une autre forme (liste sans expire) vaut aussi jeton inconnu
  j2 <- .jd_emettre()
  saveRDS(list(jeton = j2), file.path(d, paste0(j2, ".rds")))
  expect_s3_class(.jd_consommer(j2), "nemetonshiny_jeton_invalide")
  # un corrompu qui traine est purge a l'emission suivante
  writeLines("pas un rds", file.path(d, "abcdefghijklmnopqrstuvwx.rds"))
  j3 <- .jd_emettre()
  expect_identical(list.files(d, pattern = "\\.rds$"), paste0(j3, ".rds"))
})

test_that("un temporaire abandonne est purge passe une duree de jeton", {
  d <- .jd_dir()
  tmp <- file.path(d, ".abandon-x.tmp")
  writeLines("x", tmp)
  Sys.setFileTime(tmp, Sys.time() - .MCP_JETON_DUREE - 60)
  frais <- file.path(d, ".frais-x.tmp")
  writeLines("x", frais)
  .jd_emettre()
  expect_false(file.exists(tmp))
  expect_true(file.exists(frais))
})

test_that("un jeton hors forme ne designe jamais un fichier (pas de chemin fabrique)", {
  d <- .jd_dir()
  dehors <- file.path(dirname(d), paste0(basename(d), "-dehors.rds"))
  saveRDS(list(outil = "outil_x", projet = "p1", lien = list(a = 1),
               expire = as.numeric(Sys.time()) + 600), dehors)
  withr::defer(unlink(dehors))
  j <- paste0("../", basename(d), "-dehors")
  expect_s3_class(.jd_consommer(j), "nemetonshiny_jeton_invalide")
  expect_true(file.exists(dehors))
  expect_s3_class(.jd_consommer("ABCDEFGHIJKLMNOPQRSTUVWX"),
                  "nemetonshiny_jeton_invalide")
})

test_that("registre non inscriptible : erreur classee, pas une erreur R brute", {
  skip_on_os("windows")
  d <- .jd_dir()
  Sys.chmod(d, "0500", use_umask = FALSE)
  withr::defer(Sys.chmod(d, "0700", use_umask = FALSE))
  skip_if(file.access(d, 2) == 0, "repertoire inscriptible malgre 0500 (root)")
  e <- .jd_cond(.mcp_jeton_emettre("outil_x", "p1", list(a = 1), list(n = 1L)))
  expect_s3_class(e, "nemetonshiny_registre_jetons")
  expect_s3_class(e, "nemetonshiny_erreur")
  j <- jsonlite::fromJSON(as.character(.mcp_erreur(e)), simplifyVector = TRUE)
  expect_identical(j$code, "erreur")
})

test_that("registre impossible a creer : erreur classee a l'emission et a la consommation", {
  f <- withr::local_tempfile()
  writeLines("un fichier, pas un repertoire", f)
  withr::local_envvar(NEMETON_MCP_JETONS_DIR = file.path(f, "jetons"))
  e <- .jd_cond(.mcp_jeton_emettre("outil_x", "p1", list(a = 1), list(n = 1L)))
  expect_s3_class(e, "nemetonshiny_registre_jetons")
  expect_s3_class(.jd_consommer("abcdefghijklmnopqrstuvwx"),
                  "nemetonshiny_registre_jetons")
})

test_that("le tirage ignore set.seed() et ne touche pas .Random.seed", {
  .jd_dir()
  set.seed(42)
  avant <- get(".Random.seed", envir = globalenv())
  a <- .mcp_jeton_tirer()
  expect_identical(get(".Random.seed", envir = globalenv()), avant)
  set.seed(42)
  b <- .mcp_jeton_tirer()
  expect_false(identical(a, b))
  expect_match(c(a, b), "^[0-9a-z]{24}$")
  # loi uniforme : les 36 caracteres sortent sur un grand tirage
  x <- strsplit(paste(replicate(200, .mcp_jeton_tirer()), collapse = ""), "")[[1]]
  expect_setequal(unique(x), c(0:9, letters))
})

test_that("repli sans /dev/urandom : octets hors RNG de R, jetons distincts", {
  absent <- file.path(tempdir(), "pas-de-urandom-016")
  set.seed(1)
  a <- .mcp_octets_aleatoires(40L, source = absent)
  set.seed(1)
  b <- .mcp_octets_aleatoires(40L, source = absent)
  expect_length(a, 40L)
  expect_false(identical(a, b))
})

test_that("deux consommateurs du meme fichier : un seul file.remove() gagne", {
  d <- .jd_dir()
  j <- .jd_emettre()
  # Course simulee : l'autre serveur supprime le fichier apres notre lecture.
  local_mocked_bindings(file.remove = function(...) {
    unlink(file.path(d, paste0(j, ".rds")))
    FALSE
  }, .package = "base")
  expect_s3_class(.jd_consommer(j), "nemetonshiny_jeton_invalide")
})

# ------------------------------------------------- vrais sous-processus R

.jd_rscript <- function(lignes, dir) {
  racine <- normalizePath(test_path("..", ".."))
  script <- tempfile(fileext = ".R")
  writeLines(c(sprintf('suppressMessages(pkgload::load_all("%s", quiet = TRUE))',
                       racine), lignes), script)
  rscript <- file.path(R.home("bin"), "Rscript")
  sortie <- suppressWarnings(system2(rscript, script, stdout = TRUE,
    stderr = TRUE, env = paste0("NEMETON_MCP_JETONS_DIR=", shQuote(dir))))
  sortie
}
.jd_sauter_sans_sources <- function() {
  skip_on_cran()
  racine <- normalizePath(test_path("..", ".."))
  if (length(list.files(file.path(racine, "R"), pattern = "\\.R$")) == 0L) {
    skip("sources R absentes (paquet installe, covr)")
  }
}

test_that("vrai sous-processus : emettre dans un R, consommer dans un autre, rejouer -> jeton_invalide", {
  .jd_sauter_sans_sources()
  d <- .jd_dir()
  emis <- .jd_rscript(c(
    'e <- tryCatch(nemetonshiny:::.mcp_jeton_emettre("appliquer_ugf", "p1",',
    '  list(f = "x", md5 = "y"), list(n = 1L)), error = function(e) e)',
    'cat("JETON", e$jeton, "\\n")'), d)
  l <- grep("^JETON [0-9a-z]{24} *$", emis, value = TRUE)
  expect_length(l, 1L)
  j <- trimws(sub("^JETON", "", l[1]))
  conso <- c(
    sprintf('j <- "%s"', j),
    'r <- tryCatch(nemetonshiny:::.mcp_jeton_consommer(j, "appliquer_ugf", "p1",',
    '  list(f = "x", md5 = "y"), garde = NULL), error = function(e) e)',
    'cat("RES", if (isTRUE(r)) "ok" else class(r)[1], "\\n")')
  premier <- .jd_rscript(conso, d)
  expect_true(any(grepl("^RES ok *$", premier)), info = paste(premier, collapse = "\n"))
  second <- .jd_rscript(conso, d)
  expect_true(any(grepl("^RES nemetonshiny_jeton_invalide *$", second)),
              info = paste(second, collapse = "\n"))
})

test_that("vrai sous-processus : set.seed() identique dans deux processus -> jetons differents", {
  .jd_sauter_sans_sources()
  # Deux registres distincts : sinon le second processus verrait le fichier
  # du premier et retirerait un autre jeton, masquant un tirage par sample().
  d1 <- file.path(.jd_dir(), "a")
  d2 <- file.path(dirname(d1), "b")
  lignes <- c('set.seed(2026)',
              'e <- tryCatch(nemetonshiny:::.mcp_jeton_emettre("o", "p", 1, list()),',
              '  error = function(e) e)',
              'cat("JETON", e$jeton, "\\n")')
  a <- grep("^JETON [0-9a-z]{24} *$", .jd_rscript(lignes, d1), value = TRUE)
  b <- grep("^JETON [0-9a-z]{24} *$", .jd_rscript(lignes, d2), value = TRUE)
  expect_length(a, 1L)
  expect_length(b, 1L)
  expect_false(identical(a, b))
})

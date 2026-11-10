# Mission 003 : classement des erreurs MCP (.mcp_erreur) hors outils.

.lit_err <- function(x) jsonlite::fromJSON(as.character(x), simplifyVector = TRUE)

test_that("chaque code a un message et une action FR et EN, distincts", {
  for (code in names(.MCP_ERREURS)) {
    for (l in c("fr", "en")) {
      i18n <- get_i18n(l)
      expect_true(i18n$has(paste0("mcp_err_", code)), info = code)
      expect_true(i18n$has(paste0("mcp_act_", .MCP_ERREURS[[code]]$action)),
                  info = code)
    }
    expect_false(identical(
      get_i18n("fr")$t(paste0("mcp_err_", code), projet = "x", detenteur = "x"),
      get_i18n("en")$t(paste0("mcp_err_", code), projet = "x", detenteur = "x")),
      info = code)
  }
  cles <- grep("^mcp_(err|act)_", names(TRANSLATIONS), value = TRUE)
  expect_true(all(nchar(cles) <= 30L))
  expect_length(grep("^mcp_err_", cles), 16L)  # 14 (003) + 2 (010)
  expect_length(grep("^mcp_act_", cles), 11L)   # 9 (003) + 2 (010)
})

test_that("une classe connue garde son code ; la classe est le nom R complet", {
  e <- tryCatch(.api_abort("x", "nemetonshiny_calcul_en_cours"),
                error = function(e) e)
  r <- .mcp_erreur(e, "en")
  expect_true(isTRUE(attr(r, "mcp_erreur")))
  j <- .lit_err(r)
  expect_false(j$ok)
  expect_identical(j$code, "calcul_en_cours")
  expect_identical(j$classe, "nemetonshiny_calcul_en_cours")
  expect_identical(j$langue, "en")
})

test_that("classe inconnue ou erreur R brute : repli classe, detail joint", {
  e1 <- tryCatch(.api_abort("detail technique", "nemetonshiny_nouvelle"),
                 error = function(e) e)
  j1 <- .lit_err(.mcp_erreur(e1))
  expect_identical(j1$code, "erreur")
  expect_match(j1$erreur, "detail technique", fixed = TRUE)
  j2 <- .lit_err(.mcp_erreur(simpleError("boum")))
  expect_identical(j2$code, "erreur_inconnue")
  expect_identical(j2$classe, "nemetonshiny_erreur_inconnue")
  expect_match(j2$erreur, "boum", fixed = TRUE)
})

test_that("une langue inconnue retombe sur le francais", {
  expect_identical(.mcp_langue("de"), "fr")
  expect_identical(.mcp_langue(NULL), "fr")
  expect_identical(.mcp_langue("en"), "en")
})

test_that(".mcp_vers_outil marque l'erreur et laisse passer le succes", {
  skip_if_not_installed("ellmer")
  ok <- .mcp_call(list(a = 1))
  expect_identical(.mcp_vers_outil(ok), ok)
  r <- .mcp_vers_outil(.mcp_call(stop("boum")))
  expect_false(is.null(r@error))
})

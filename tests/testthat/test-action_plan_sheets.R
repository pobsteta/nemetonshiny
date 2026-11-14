# Fiches PDF par UGF du Plan d'actions (mission 024)
# Constructeur pur, cibles, noms, service ZIP (rendu mocke), archive.

.fx_plan <- function() {
  act <- function(id, ug, annee, cout = NA, revenu = NA, src = NULL, com = "") {
    list(id = id, ug_id = ug, type = "eclaircie", priorite = "haute",
         statut = "proposee", annee_cible = annee, objectifs_lies = c("P"),
         quantite = list(cout_eur = cout, revenu_eur = revenu),
         source = src, commentaire = com)
  }
  list(
    horizon_annees = 20L, annee_base = 2026L,
    actions = list(
      act("a3", "U1", 5, cout = 100, revenu = 400,
          src = list(origine = "family_C", extrait_texte = "Stock de carbone")),
      act("a1", "U1", 1, cout = 50),
      act("a2", "U1", 3, revenu = 10,
          src = list(origine = "synthesis", extrait_texte = "   ")),
      act("b1", "U2", 2, cout = 1000, revenu = 10),
      act("c1", "U3", 2, cout = 7)
    )
  )
}

.fx_ugs <- function() {
  data.frame(ug_id = c("U1", "U2", "U3"),
             label = c("\u00c9pic\u00e9a", "\u00c8pic\u00e9a", ""),
             surface_m2 = c(20000, 50000, 10000),
             stringsAsFactors = FALSE)
}

.fx_project <- function(path = NULL) {
  list(id = "p1", path = path, metadata = list(name = "For\u00eat Test"))
}

# ---- constructeur pur --------------------------------------------------------

test_that("constructeur : tri chronologique des actions de chaque UGF", {
  d <- nemetonshiny:::.build_action_plan_pdf_data(
    .fx_project(), .fx_plan(), .fx_ugs())
  u1 <- d$ugfs[[1]]
  expect_identical(vapply(u1$actions, function(a) a$id, ""), c("a1", "a2", "a3"))
  expect_equal(unlist(lapply(u1$actions, function(a) a$annee_realisation)),
               c(2027L, 2029L, 2031L))
})

test_that("constructeur : mode fiche = nom, surface et totaux de l'UGF", {
  d <- nemetonshiny:::.build_action_plan_pdf_data(
    .fx_project(), .fx_plan(), .fx_ugs(),
    filter_ug_ids = "U2", mode = "fiche")
  expect_identical(d$mode, "fiche")
  expect_identical(d$ug_name, "\u00c8pic\u00e9a")
  expect_equal(d$ug_surface_ha, 5)
  expect_equal(d$n_ugfs, 1L)
  expect_equal(d$n_actions, 1L)
  expect_equal(d$totals$cout, 1000)
  expect_equal(d$totals$revenu, 10)
  expect_equal(d$totals$bilan, -990)
  # mode plan : pas de nom d'UGF, totaux de tout le projet
  p <- nemetonshiny:::.build_action_plan_pdf_data(
    .fx_project(), .fx_plan(), .fx_ugs())
  expect_null(p$ug_name)
  expect_equal(p$n_ugfs, 3L)
  expect_equal(p$totals$cout, 1157)
})

test_that("constructeur : sources portees, origine famille traduite, code brut sinon", {
  d_fr <- nemetonshiny:::.build_action_plan_pdf_data(
    .fx_project(), .fx_plan(), .fx_ugs(), filter_ug_ids = "U1", language = "fr")
  acts <- stats::setNames(d_fr$ugfs[[1]]$actions,
                          vapply(d_fr$ugfs[[1]]$actions, function(a) a$id, ""))
  expect_identical(acts$a3$source_extrait, "Stock de carbone")
  expect_identical(acts$a3$source_origine, "family_C")
  fam <- INDICATOR_FAMILIES[["C"]]
  expect_identical(acts$a3$source_label, paste0(fam$name_fr, " (C)"))
  # code non famille : brut, sans libelle invente
  expect_identical(acts$a2$source_label, "synthesis")
  # action sans source : vide, jamais NA
  expect_identical(acts$a1$source_extrait, "")
  expect_identical(acts$a1$source_label, "")
  d_en <- nemetonshiny:::.build_action_plan_pdf_data(
    .fx_project(), .fx_plan(), .fx_ugs(), filter_ug_ids = "U1", language = "en")
  en <- Filter(function(a) a$id == "a3", d_en$ugfs[[1]]$actions)[[1]]
  expect_identical(en$source_label, paste0(fam$name_en, " (C)"))
})

test_that("constructeur : cartes injectees par UGF, NA sinon", {
  d <- nemetonshiny:::.build_action_plan_pdf_data(
    .fx_project(), .fx_plan(), .fx_ugs(), maps = list(U2 = "/tmp/x.png"))
  expect_true(is.na(d$ugfs[[1]]$map_png))
  expect_identical(d$ugfs[[2]]$map_png, "/tmp/x.png")
})

test_that("en-tete YAML : jetons remplaces, FR et EN", {
  src <- c('title: "@@TITLE@@"', 'subtitle: "@@SUBTITLE@@"', 'lang: "@@LANG@@"',
           "\\fancyhead[L]{\\small @@HEAD@@}")
  fr <- nemetonshiny:::.action_plan_qmd_localise(src, "fr")
  en <- nemetonshiny:::.action_plan_qmd_localise(src, "en")
  expect_false(any(grepl("@@", c(fr, en), fixed = TRUE)))
  expect_match(fr[3], 'lang: "fr"', fixed = TRUE)
  expect_match(en[3], 'lang: "en"', fixed = TRUE)
  expect_match(en[1], "action plan", fixed = TRUE)
  tpl <- readLines(system.file("quarto", "action_plan_template.qmd",
                               package = "nemetonshiny"), warn = FALSE)
  expect_false(any(grepl("@@", nemetonshiny:::.action_plan_qmd_localise(tpl, "en"),
                         fixed = TRUE)))
  expect_true(any(grepl('mode: "plan"', tpl, fixed = TRUE)))
  expect_true(any(grepl("Sources", tpl, fixed = TRUE)))
})

# ---- cible et noms --------------------------------------------------------------

test_that("cible : 1 UGF -> pdf ; plusieurs -> zip des seules selectionnees ; aucune -> toutes", {
  vis <- data.frame(ug_id = c("U1", "U1", "U2", "U3"), stringsAsFactors = FALSE)
  f <- nemetonshiny:::.action_plan_sheet_targets
  one <- f("U2", vis)
  expect_identical(one$mode, "pdf"); expect_identical(one$ug_ids, "U2")
  two <- f(c("U3", "U1"), vis)
  expect_identical(two$mode, "zip"); expect_setequal(two$ug_ids, c("U1", "U3"))
  all <- f(character(), vis)
  expect_identical(all$mode, "zip"); expect_setequal(all$ug_ids, c("U1", "U2", "U3"))
  # une seule UGF avec actions et aucune selection -> pdf
  expect_identical(f(NULL, vis[1:2, , drop = FALSE])$mode, "pdf")
})

test_that("cible : UGF selectionnee sans action visible ignoree ; tout ignore -> none", {
  vis <- data.frame(ug_id = c("U1", "U2"), stringsAsFactors = FALSE)
  f <- nemetonshiny:::.action_plan_sheet_targets
  r <- f(c("U1", "U9"), vis)
  expect_identical(r$mode, "pdf"); expect_identical(r$ug_ids, "U1")
  expect_identical(r$ignored, "U9")
  expect_identical(f("U9", vis)$mode, "none")
  expect_identical(f(character(), vis[0, , drop = FALSE])$mode, "none")
  expect_identical(f(character(), NULL)$mode, "none")
})

test_that("noms : accents aplatis -> uniques ; libelle vide -> ug_id", {
  st <- nemetonshiny:::.action_plan_sheet_stems(
    c("U1", "U2", "U3"), c("\u00c9pic\u00e9a", "\u00c8pic\u00e9a", ""))
  expect_equal(length(unique(st)), 3L)
  expect_true(all(grepl("^fiche_[A-Za-z0-9_-]+$", st)))
  expect_match(st[3], "U3")
  expect_match(st[1], "U1"); expect_match(st[2], "U2")
  # libelles identiques et ug_id identiques : make.unique en dernier recours
  expect_equal(length(unique(nemetonshiny:::.action_plan_sheet_stems(
    c("A", "A"), c("x", "x")))), 2L)
})

test_that("nom de fichier : pdf / zip / txt selon la cible, identique pour filename et archive", {
  f <- nemetonshiny:::.action_plan_sheet_filename
  ugs <- .fx_ugs()
  tg <- nemetonshiny:::.action_plan_sheet_targets(
    "U3", data.frame(ug_id = c("U1", "U3")))
  expect_identical(f(.fx_project(), tg, ugs), "For_t_Test_fiche_U3.pdf")
  tz <- nemetonshiny:::.action_plan_sheet_targets(
    character(), data.frame(ug_id = c("U1", "U3")))
  expect_identical(f(.fx_project(), tz, ugs), "For_t_Test_fiches_ugf.zip")
  tn <- nemetonshiny:::.action_plan_sheet_targets("zz", data.frame(ug_id = "U1"))
  expect_match(f(.fx_project(), tn, ugs), "\\.txt$")
})

# ---- archive : jamais d'ecrasement du PDF du plan -----------------------------------

test_that("archive fiches : nom propre, <slug>_action_plan.pdf non ecrase", {
  pdir <- withr::local_tempdir()
  project <- list(path = pdir, metadata = list(name = "Projet"))
  plan_pdf <- tempfile(fileext = ".pdf"); writeLines("PLAN", plan_pdf)
  nemetonshiny:::.archive_action_plan_pdf(plan_pdf, project)
  sheet <- tempfile(fileext = ".pdf"); writeLines("FICHE", sheet)
  expect_true(nemetonshiny:::.archive_action_plan_pdf(
    sheet, project, file_name = "Projet_fiche_U1.pdf"))
  zipf <- tempfile(fileext = ".zip"); writeLines("ZIP", zipf)
  expect_true(nemetonshiny:::.archive_action_plan_pdf(
    zipf, project, file_name = "Projet_fiches_ugf.zip"))
  ex <- file.path(pdir, "exports")
  expect_identical(readLines(file.path(ex, "Projet_action_plan.pdf")), "PLAN")
  expect_identical(readLines(file.path(ex, "Projet_fiche_U1.pdf")), "FICHE")
  expect_true(file.exists(file.path(ex, "Projet_fiches_ugf.zip")))
})

# ---- service (rendu mocke) ---------------------------------------------------------------

.mock_render <- function(fail = character(), null = character()) {
  calls <- new.env(); calls$ids <- character(); calls$modes <- character()
  fn <- function(project, plan, ug_sf, output_file, language = "fr",
                 filter_ug_ids = NULL, filter_action_ids = NULL, mode = "plan") {
    calls$ids <- c(calls$ids, filter_ug_ids); calls$modes <- c(calls$modes, mode)
    if (filter_ug_ids %in% fail) stop("boom LaTeX")
    if (filter_ug_ids %in% null) return(NULL)
    writeLines(paste("PDF", filter_ug_ids), output_file)
    output_file
  }
  list(fn = fn, calls = calls)
}

.run_sheets <- function(ids, out, ..., quarto = TRUE) {
  nemetonshiny:::generate_action_plan_ug_sheets(
    .fx_project(), .fx_plan(), .fx_ugs(), out, language = "fr",
    ug_ids = ids, ...)
}

test_that("service : 1 UGF -> un PDF, rendu en mode fiche filtre sur l'UGF", {
  m <- .mock_render()
  testthat::local_mocked_bindings(
    ensure_quarto_installed = function() TRUE,
    generate_action_plan_pdf = m$fn, .package = "nemetonshiny")
  out <- tempfile(fileext = ".pdf")
  r <- .run_sheets("U2", out)
  expect_identical(r$type, "pdf")
  expect_true(file.exists(out))
  expect_identical(m$calls$ids, "U2"); expect_identical(m$calls$modes, "fiche")
})

test_that("service : plusieurs UGF -> ZIP a plat, noms uniques, un PDF par UGF", {
  m <- .mock_render()
  testthat::local_mocked_bindings(
    ensure_quarto_installed = function() TRUE,
    generate_action_plan_pdf = m$fn, .package = "nemetonshiny")
  out <- tempfile(fileext = ".zip")
  r <- .run_sheets(c("U1", "U2", "U3"), out)
  expect_identical(r$type, "zip"); expect_equal(r$n_ok, 3L)
  nm <- utils::unzip(out, list = TRUE)$Name
  expect_equal(length(nm), 3L)
  expect_false(any(grepl("/", nm, fixed = TRUE)))
  expect_equal(length(unique(nm)), 3L)
  expect_true(all(grepl("^For_t_Test_fiche_.*\\.pdf$", nm)))
  expect_false("erreurs.txt" %in% nm)
})

test_that("service : UGF en echec (erreur OU NULL) ignoree et listee dans erreurs.txt", {
  m <- .mock_render(fail = "U1", null = "U3")
  testthat::local_mocked_bindings(
    ensure_quarto_installed = function() TRUE,
    generate_action_plan_pdf = m$fn, .package = "nemetonshiny")
  out <- tempfile(fileext = ".zip")
  r <- .run_sheets(c("U1", "U2", "U3"), out)
  expect_equal(r$n_ok, 1L)
  expect_setequal(names(r$failed), c("U1", "U3"))
  nm <- utils::unzip(out, list = TRUE)$Name
  expect_true("erreurs.txt" %in% nm)
  expect_equal(sum(grepl("\\.pdf$", nm)), 1L)
  d <- withr::local_tempdir()
  utils::unzip(out, files = "erreurs.txt", exdir = d)
  txt <- paste(readLines(file.path(d, "erreurs.txt"), encoding = "UTF-8"),
               collapse = "\n")
  expect_match(txt, "U1", fixed = TRUE); expect_match(txt, "boom LaTeX", fixed = TRUE)
  expect_match(txt, "U3", fixed = TRUE)
  expect_false(grepl("U2 ", txt, fixed = TRUE))
})

test_that("service : toutes en echec ou aucune UGF -> erreur, jamais de ZIP", {
  m <- .mock_render(fail = c("U1", "U2"))
  testthat::local_mocked_bindings(
    ensure_quarto_installed = function() TRUE,
    generate_action_plan_pdf = m$fn, .package = "nemetonshiny")
  out <- tempfile(fileext = ".zip")
  expect_error(.run_sheets(c("U1", "U2"), out), "boom LaTeX")
  expect_false(file.exists(out))
  expect_error(.run_sheets(character(), out), "Aucune UGF")
  expect_false(file.exists(out))
  # une seule UGF en echec : erreur aussi
  expect_error(.run_sheets("U1", tempfile(fileext = ".pdf")), "boom")
})

test_that("service : Quarto verifie une fois avant la boucle, echec net", {
  m <- .mock_render()
  testthat::local_mocked_bindings(
    ensure_quarto_installed = function() FALSE,
    generate_action_plan_pdf = m$fn, .package = "nemetonshiny")
  expect_error(.run_sheets(c("U1", "U2"), tempfile(fileext = ".zip")), "Quarto")
  expect_length(m$calls$ids, 0L)
})

test_that("service : plafond de 30 UGF refuse sans rien rendre", {
  m <- .mock_render()
  testthat::local_mocked_bindings(
    ensure_quarto_installed = function() TRUE,
    generate_action_plan_pdf = m$fn, .package = "nemetonshiny")
  ids <- sprintf("U%02d", 1:31)
  expect_error(.run_sheets(ids, tempfile(fileext = ".zip")), "31")
  expect_length(m$calls$ids, 0L)
  # 30 : accepte
  ids30 <- sprintf("U%02d", 1:30)
  ugs <- data.frame(ug_id = ids30, label = ids30)
  out <- tempfile(fileext = ".zip")
  r <- nemetonshiny:::generate_action_plan_ug_sheets(
    .fx_project(), .fx_plan(), ugs, out, ug_ids = ids30)
  expect_equal(r$n_ok, 30L)
})

test_that("service : progression i/N appelee avant chaque fiche", {
  m <- .mock_render()
  testthat::local_mocked_bindings(
    ensure_quarto_installed = function() TRUE,
    generate_action_plan_pdf = m$fn, .package = "nemetonshiny")
  seen <- character()
  .run_sheets(c("U1", "U2"), tempfile(fileext = ".zip"),
              progress = function(i, n) seen <<- c(seen, paste0(i, "/", n)))
  expect_identical(seen, c("1/2", "2/2"))
})

# ---- rendu reel (saute sans Quarto) -----------------------------------------------------------

test_that("rendu reel d'une fiche d'UGF (si Quarto + LaTeX disponibles)", {
  skip_on_cran()
  skip_if_not(nemetonshiny:::is_quarto_installed(), "Quarto indisponible")
  skip_if(Sys.which("xelatex") == "" && Sys.which("tlmgr") == "" &&
            !isTRUE(nzchar(Sys.getenv("NEMETON_TEST_RENDER"))),
          "LaTeX indisponible")
  out <- tempfile(fileext = ".pdf")
  res <- suppressWarnings(nemetonshiny:::generate_action_plan_pdf(
    .fx_project(), .fx_plan(), .fx_ugs(), out, language = "fr",
    filter_ug_ids = "U1", mode = "fiche"))
  skip_if(is.null(res), "rendu PDF impossible dans cet environnement")
  expect_gt(file.size(out), 1000)
})
